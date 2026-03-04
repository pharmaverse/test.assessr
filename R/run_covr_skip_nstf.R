
#' Run Coverage Analysis on Package Source and Non‑Standard Tests (NSTF)
#'
#' This function maps package source files to non‑standard test scripts, detects
#' failing or skipped tests, executes the remaining tests manually, computes code
#' coverage, and returns structured results. It is intended for packages that do
#' not use the standard `testthat` framework (e.g., base R tests under `tests/`,
#' Bioconductor‑style tests in `inst/tests/`, or other legacy/custom scripts).
#'
#' @param pkg_source_path Character. Path to the root of the package source
#'   directory.
#' @param test_pkg_data List. Output from \code{check_pkg_tests_and_snaps()}
#'   indicating the detected testing infrastructure (e.g., base tests,
#'   Bioconductor tests) and the presence of snapshots or related artifacts.
#' @param cov_env Environment that holds the temporary coverage workspace and
#'   shared state for the run.
#'
#' @details
#' This NSTF workflow does not rely on `testthat` reporters. Instead, it:
#' \itemize{
#'   \item maps source files to available NSTF test scripts,
#'   \item identifies failing or problematic tests and derives a skip map,
#'   \item runs the remaining test scripts, and
#'   \item computes coverage on the successfully executed regions.
#' }
#' The output mirrors the structure returned by the STF skip‑aware workflow so
#' that downstream reporting remains consistent across frameworks.
#'
#' @return
#' A named list containing coverage results and diagnostics:
#' \describe{
#'   \item{total_cov}{Numeric. Overall percentage of lines covered across the package.}
#'   \item{res_cov}{List. File‑ or element‑level coverage results (as derived from
#'         covr output), including per‑file coverage, any errors, and diagnostic notes.}
#'   \item{long_summary}{Long‑format summary of test outcomes (e.g., PASS/FAIL/ERROR/SKIPPED),
#'         when available from the NSTF execution.}
#'   \item{test_skip}{A data frame describing lines or regions skipped during coverage
#'         (if any). \code{NULL} if no skipping was required.}
#'   \item{untested_sources}{Character vector of source files for which no tests were found.}
#' }
#' If no valid NSTF test directory is detected, an “empty” coverage object may be
#' returned via \code{create_empty_covr_list()}.
#'
#'
#' @family nstf_utility
#' @rdname run_covr_skip_nstf
#' @keywords internal
run_covr_skip_nstf <- function(pkg_source_path, 
                               test_pkg_data,
                               cov_env
                               ) {
 
  
  # --- Input checking (CRAN-safe and explicit) ---
  checkmate::assert_string(pkg_source_path, .var.name = "pkg_source_path")
  checkmate::assert_directory_exists(pkg_source_path, .var.name = "pkg_source_path")
  checkmate::assert_true(!missing(cov_env), .var.name = "cov_env")
  checkmate::assert_environment(cov_env, .var.name = "cov_env")
  
  
  pkg_name <- get_pkg_name(pkg_source_path)
  
  # set up path to test directory 
  path    <- as.character(pkg_source_path)
  testdir <- file.path(path, "tests")
  
  # Get a *list* of candidate paths
  test_path_list <- get_nstf_test_path(test_pkg_data, testdir)
  
  # Normalize *all* candidates and keep names (e.g., "testthat", "testit", etc.)
  test_path <- normalizePath(
    unlist(test_path_list, use.names = TRUE),
    winslash = "/",
    mustWork = FALSE
  )
  
  message(paste0("performing source test mapping for ", pkg_name))
  mapping <- tryCatch({
    get_source_test_mapping_nstf(pkg_source_path, test_path)
  }, error = function(e) {
    cleanup_and_return_null(
      paste0("Error in source-test mapping for ", pkg_name, " : ", e$message),
      env = cov_env
    )
  })
  if (is.null(mapping)) return(NULL)
  
  message(paste0("identifying problem tests for ", pkg_name))
  
  # check for testit testing framework
  if (isTRUE(test_pkg_data$has_testit)) {
    problems <- tryCatch({
      check_covr_skip_testit(pkg_name, mapping, test_path)
    }, error = function(e) {
      cleanup_and_return_null(
        paste0("Error identifying skipped/problematic tests for ", pkg_name, " : ", e$message),
        env = cov_env
      )
    })
  } else {
    problems <- tryCatch({
      check_covr_skip_nstf(pkg_source_path, mapping)
    }, error = function(e) {
      cleanup_and_return_null(
        paste0("Error identifying skipped/problematic tests for ", pkg_name, " : ", e$message),
        env = cov_env
      )
    })
  }
  if (is.null(problems)) return(NULL)
  
  # check for tests to skip
  skip_tests <- problems$test_file[
    !is.na(problems$test_file) &
      !startsWith(problems$issue_type, "warning:") &
      problems$issue_type != "No tests skipped"
  ]
  
  # create df with source files with no tests
  no_tests_df <- get_function_no_tests(mapping) 
  
  # remove source files with no tests
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  
  # create test files clean with testit testing framework and tests testing framework
  if (isTRUE(test_pkg_data$has_testit)) {
    test_files_clean <- file.path(pkg_source_path, "tests/testit", clean_mapping$test_file)
    if (!is.null(skip_tests) | (length(skip_tests) > 0)) {
      test_files_clean <- setdiff(test_files_clean, file.path(pkg_source_path, "tests/testit", skip_tests))
    }
  } else {
    test_files_clean <- file.path(pkg_source_path, "tests", clean_mapping$test_file)
    if (!is.null(skip_tests) | (length(skip_tests) > 0)) {
      test_files_clean <- setdiff(test_files_clean, file.path(pkg_source_path, "tests", skip_tests))
    }
  }
  
  message(paste0("loading package and datasets for ", pkg_name))
  tryCatch({
    pkgload::load_all(pkg_source_path)
    
    pkg_all <- ls(envir = asNamespace(pkg_name), all.names = TRUE)
    
    for (obj in pkg_all) {
      tryCatch({
        assign(obj, get(obj, envir = asNamespace(pkg_name)), envir = cov_env)
      }, error = function(e) {
        message(paste0("Failed to assign object ", obj, " : ", e$message))
      })
    }
    
    # Load dependencies
    message(paste0("Loading dependencies into coverage environment for: ", pkg_name))
    load_dependencies_into_env(pkg_source_path, cov_env)
    
    datasets <- load_package_datasets(pkg_name, env = cov_env)
    message("Datasets loaded: ", paste(datasets, collapse = ", "))
  }, error = function(e) {
    cleanup_and_return_null(
      paste0("Error loading package or datasets for ", pkg_name, " : ", e$message),
      env = cov_env
    )
  })
  
  message(paste0("running coverage manually for ", pkg_name))
  
  coverage <- tryCatch({
    covr::environment_coverage(
      env = cov_env,
      test_files = test_files_clean
    )
  }, error = function(e) {
    cleanup_and_return_null(
      paste0("Error running coverage for ", pkg_name, " : ", e$message),
      env = cov_env
    )
  })
  
  if (!is.null(coverage)) {
    
    # create coverage list
    res_cov <- tryCatch({
      coverage_list <- covr::coverage_to_list(coverage)
      list(name = pkg_name, coverage = coverage_list, errors = NA, notes = NA)
    },
    error = function(cond){
      coverage_list <- list(filecoverage = NA, totalcoverage = NA_integer_)
      list(
        name = pkg_name, coverage = coverage_list,
        errors = cond,
        notes = NA
      )
    })
    
    # set up inputs for calculating package coverage
    percent_cov <- as.numeric(res_cov$coverage$totalcoverage)
    
    functions_no_tests_df <- no_tests_df   # data.frame of functions lacking tests
    tests_passing         <- test_files_clean      # character vector of passing test files
    tests_skipped         <- skip_tests            # character vector of skipped test files
    
    # calculate package coverage
    calc_covr_list <- compute_total_coverage(
      percent_cov, 
      functions_no_tests_df, 
      tests_passing,
      tests_skipped
    )
    
    total_cov <- calc_covr_list$total_cov
    
    percent_cov_round <- round(total_cov, 2)
    message(paste0("Total Coverage for ", pkg_name, " is: ", percent_cov_round, "%"))
    
    percent_cov         <- calc_covr_list$percent_cov
    n_testfiles         <- calc_covr_list$n_testfiles
    n_no_function_tests <- calc_covr_list$n_no_function_tests
    n_skipped_files     <- calc_covr_list$n_skipped_files  # (kept name, fixing stray space)
    
    message(sprintf(
      "Approx total coverage: (%.2f%%), counts: tested_files=%d, untested_functions=%d, skipped_tests=%d",
      percent_cov, n_testfiles, n_no_function_tests, n_skipped_files
    ))
    
    # Final cleanup (env bindings + temp workspace under tempdir())
    cleanup_and_return_null(env = cov_env)
    
    # structure the return list
    covr_list <- list(
      total_cov = total_cov,
      res_cov = list(
        name     = res_cov$name,
        coverage = res_cov$coverage,
        errors   = res_cov$errors,
        notes    = res_cov$notes
      ),
      functions_no_tests = no_tests_df,
      tests_skipped      = skip_tests,
      tests_passing      = test_files_clean
    )
    
  } else {
    message("No test coverage for this configuration")
    covr_list <- list(
      total_cov = 0,
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
          totalcoverage = 0
        ),
        errors = "No testthat or testit configuration",
        notes = NA
      )
    )
  }
  
  return(covr_list)
}



#' Check for Skipped or Errored Test Files (non-testthat)
#'
#' This internal utility runs test files mapped to source files and identifies
#' any that produce warnings or errors when sourced. This helper does **not**
#' evaluate tests written using `testthat`; it is intended for legacy or
#' lightweight “non-testthat” test files.
#'
#' The function sources each test file in a clean environment and captures
#' whether sourcing resulted in a warning or an error. Files that produce no
#' issues are omitted from the output.
#'
#' @param pkg_source_path Character string. Path to the root directory of the
#'   package source.
#' @param mapping A data frame containing at least a column `test_file`
#'   identifying test file names found under `tests/`.
#'
#' @return
#' A data frame with the following columns:
#'
#' * **test_file** — The name of the test file that produced a diagnostic  
#' * **issue_type** — One of:
#'   * `"warning"` — sourcing the file triggered a warning  
#'   * `"error"` — sourcing the file threw an error  
#'   * `"No tests skipped"` — returned when no problems are detected
#'
#' The returned object is always a base R `data.frame`.  
#' If no warnings or errors occur in any sourced test file, the function returns
#' a single-row data frame indicating `"No tests skipped"`.
#'
#' @section Output Meaning:
#' The output is intended to help developers identify test files that fail to
#' run cleanly **before** test execution, ensuring they do not silently skip or
#' break coverage workflows.
#'
#' @keywords internal
#'
#' @family nstf_utility
#'
#' @importFrom utils capture.output
check_covr_skip_nstf <- function(pkg_source_path, mapping) {
  
  test_dir <- file.path(pkg_source_path, "tests")
  
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  
  problems <- data.frame(test_file = character(), issue_type = character(), stringsAsFactors = FALSE)
  
  problem_list <- lapply(clean_mapping$test_file, function(test) {
    test_path <- file.path(test_dir, test)
    
    result <- tryCatch(
      {
        suppressMessages(
          suppressWarnings(
            capture.output(
            source(test_path, local = new.env()),
            file = NULL
            )
          )
        )
        NULL
      },
      warning = function(w) "warning",
      error = function(e) "error"
    )
    
    if (!is.null(result)) {
      return(data.frame(test_file = test, issue_type = result, stringsAsFactors = FALSE))
    } else {
      return(NULL)
    }
  })
  
  problems <- do.call(rbind, problem_list)
  
  if (is.null(problems) || nrow(problems) == 0) {
    problems <- data.frame(
      test_file = NA_character_,
      issue_type = "No tests skipped",
      stringsAsFactors = FALSE
    )
  }
  
  return(problems)
}


#' Run testit Tests Individually and Capture Issues
#'
#' This internal NSTF utility runs `testit` test files one-by-one and reports
#' whether sourcing each file produces a warning or an error. The function
#' uses dynamic package-root detection by searching upward for a
#' `DESCRIPTION` file to ensure tests run in the correct working directory.
#'
#' The function does not evaluate testthat-based tests and is intended only
#' for lightweight or legacy `testit` scripts.
#'
#' @param pkg_name Character string. The package name. (Not used internally,
#'   retained only for compatibility with other NSTF utilities.)
#' @param mapping A data frame containing a column `test_file` listing test
#'   files to be run from the `test_path` directory.
#' @param test_path Path to the directory containing the `testit` test files.
#'
#' @return
#' A base R `data.frame` with two columns:
#'
#' * **test_file** — The name of the testit file evaluated  
#' * **issue_type** — One of:  
#'     * `"warning: <message>"` — A warning was raised while sourcing the file  
#'     * `"error: <message>"` — An error occurred while sourcing the file  
#'     * `"No tests skipped"` — Returned as a single-row result if no
#'       warnings or errors occurred in any test file  
#'
#' The data frame summarizes which testit files fail to run cleanly, allowing
#' early detection of broken or skipped tests in NSTF workflows.
#'
#' @section Output Meaning:
#' The returned data frame indicates potential problems with testit files
#' before coverage or build steps occur. There is no side effect other than
#' evaluation of test files; all results are reported in the data frame.
#'
#' @keywords internal
#'
#' @family nstf_utility
#'
#' @importFrom utils capture.output
check_covr_skip_testit <- function(pkg_name, mapping, test_path) {
  
  # Helper to find the package root by searching for DESCRIPTION upward
  find_pkg_root <- function(start_path) {
    current <- normalizePath(start_path, mustWork = TRUE)
    while (!file.exists(file.path(current, "DESCRIPTION"))) {
      parent <- dirname(current)
      if (parent == current) stop("Package root (with DESCRIPTION) not found.")
      current <- parent
    }
    current
  }
  
  pkg_root <- find_pkg_root(test_path)
  
  # Prepare mapping
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  
  problems <- data.frame(test_file = character(), issue_type = character(), stringsAsFactors = FALSE)
  
  problem_list <- lapply(clean_mapping$test_file, function(test) {
    test_file_path <- file.path(test_path, test)
    # Set working directory to package root for each test
    original_wd <- getwd()
    setwd(pkg_root)
    on.exit(setwd(original_wd), add = TRUE)
    
    result <- tryCatch(
      {
        capture.output(
          source(test_file_path, local = new.env()),
          file = NULL
        )
        NULL
      },
      warning = function(w) paste("warning:", conditionMessage(w)),
      error = function(e) paste("error:", conditionMessage(e))
    )
    
    if (!is.null(result)) {
      data.frame(test_file = test, issue_type = result, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  problems <- do.call(rbind, problem_list)
  
  if (is.null(problems) || nrow(problems) == 0) {
    problems <- data.frame(
      test_file = NA_character_,
      issue_type = "No tests skipped",
      stringsAsFactors = FALSE
    )
  }
  
  return(problems)
}



#' Map R Source Files to Corresponding Test Files
#'
#' This internal NSTF utility scans the package's `R/` directory and the
#' specified test directory and attempts to match each source file with a
#' corresponding test file. Matching is based on filename similarity between
#' source files and test files (case-insensitive, substring matching).
#'
#' The function is intended for lightweight structural checks in NSTF workflows
#' and does not interpret test contents.
#'
#' @param pkg_source_path Character string. Path to the root directory of the
#'   package source.
#' @param test_dir Character string. Path to the directory containing test files
#'   (typically `"tests/"`, `"tests/testthat/"`, or `"tests/testit/"`).
#'
#' @return
#' A base R `data.frame` containing two columns:
#'
#' * **source_file** — Name of the `.R` file found in the `R/` directory  
#' * **test_file** — Name of the matched test file from `test_dir`, or `NA`
#'   if no matching file is found  
#'
#' The mapping shows which source files have corresponding tests and which do not,
#' enabling automated checks for test completeness in NSTF pipelines.
#'
#' @section Output Meaning:
#' A row with `test_file = NA` indicates that no test file appears to match the
#' given source file. The returned data frame is solely a structural mapping;
#' no tests are run, and no side effects occur.
#'
#' @keywords internal
#'
#' @family nstf_utility
get_source_test_mapping_nstf <- function(pkg_source_path, test_dir) {
  source_dir <- file.path(pkg_source_path, "R")
  # test_dir <- file.path(pkg_source_path, "tests")
  
  src_files <- list.files(source_dir, pattern = "\\.R$", full.names = FALSE)
  test_files <- list.files(test_dir, pattern = "\\.R$", full.names = FALSE)
  
  mapping <- do.call(rbind, lapply(tools::file_path_sans_ext(src_files), function(src) {
    match <- test_files[grepl(src, tools::file_path_sans_ext(test_files), ignore.case = TRUE)]
    data.frame(
      source_file = paste0(src, ".R"),
      test_file = ifelse(length(match) == 0, NA, match[1]),
      stringsAsFactors = FALSE
    )
  }))
  
  return(mapping)
}


#' Collect All Applicable Nonstandard Test Paths for a Package (internal)
#'
#' This internal NSTF utility detects and returns all relevant test directories
#' used by an R package. It combines declared test framework flags in
#' \code{test_pkg_data} with filesystem-based detection for robustness.
#'
#' Supported directory layouts include:
#' \itemize{
#'   \item \strong{testthat}: \code{tests/testthat/}
#'   \item \strong{testit} (standard): \code{tests/testit/}
#'   \item \strong{testit} (nonstandard): both \code{tests/test-ci/} and
#'         \code{tests/test-cran/} present, using \code{tests/} as the root
#'   \item \strong{base R tests}: \code{*.R} files directly under \code{tests/}
#'   \item \strong{Bioconductor unit tests}: \code{inst/unitTests/}
#' }
#'
#' If no test directory of any supported type is detected, the output falls back
#' to a single entry:
#' \code{default_tests_root = <tests/>}.
#'
#' @param test_pkg_data \code{list}. A list of logical flags describing detected
#'   test frameworks. Missing fields default to \code{FALSE}. Expected (optional)
#'   fields include:
#'   \itemize{
#'     \item \code{has_testthat}
#'     \item \code{has_testit}
#'     \item \code{has_tests_base}
#'     \item \code{has_bioc_unit_tests}
#'   }
#'
#' @param testdir \code{character}. Path to the package's \code{tests/} directory.
#'   The package root is inferred as \code{dirname(testdir)} for detection of
#'   Bioconductor-style \code{inst/unitTests/}.
#'
#' @return \code{list}. A named list of character vectors giving all detected
#'   applicable test directories. Possible names include:
#'   \itemize{
#'     \item \code{testthat} – path to \code{tests/testthat/}
#'     \item \code{testit} – path to \code{tests/testit/}
#'     \item \code{nonstandard_testit_root} – \code{tests/} when both
#'           \code{test-ci/} and \code{test-cran/} exist
#'     \item \code{base_tests_root} – \code{tests/} if \code{*.R} files exist directly
#'     \item \code{bioc_unitTests} – path to \code{inst/unitTests/}
#'     \item \code{default_tests_root} – fallback \code{tests/} directory
#'   }
#'
#' @section Output Meaning:
#' The returned list describes all test frameworks detected in a package. Each
#' element is a filesystem path that the NSTF workflow should consider when
#' identifying or running tests. Duplicate paths (e.g., multiple roles pointing
#' to \code{tests/}) are automatically removed.
#'
#' @keywords internal
#'
#' @family nstf_utility
get_nstf_test_path <- function(test_pkg_data, testdir) {
  
  # Normalize key dirs
  testdir <- normalizePath(testdir, winslash = "/", mustWork = FALSE)
  pkg_root <- normalizePath(file.path(testdir, ".."), winslash = "/", mustWork = FALSE)
  
  # Candidate paths
  paths <- list(
    testthat = file.path(testdir, "testthat"),
    testit = file.path(testdir, "testit"),
    test_ci = file.path(testdir, "test-ci"),
    test_cran = file.path(testdir, "test-cran"),
    tests_root = testdir,
    bioc_unitTests = file.path(pkg_root, "inst", "unitTests")
  )
  
  # Helper: detect base R scripts directly in tests/ root
  has_base_R_in_tests_root <- function(tdir) {
    if (!dir.exists(tdir)) return(FALSE)
    files <- list.files(tdir, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
    length(files) > 0
  }
  
  # Build the output list of active test paths
  test_path_list <- list()
  
  # testthat
  if (isTRUE(test_pkg_data$has_testthat) || dir.exists(paths$testthat)) {
    test_path_list$testthat <- paths$testthat
  }
  
  # testit (standard)
  if (isTRUE(test_pkg_data$has_testit) || dir.exists(paths$testit)) {
    test_path_list$testit <- paths$testit
  }
  
  # nonstandard testit pattern: both test-ci and test-cran present => use tests/ as the root
  if ((dir.exists(paths$test_ci) && dir.exists(paths$test_cran)) ||
      isTRUE(test_pkg_data$has_testit)) {
    test_path_list$nonstandard_testit_root <- paths$tests_root
  }
  
  # base R tests in tests/ root (when *.R directly under tests/)
  if (isTRUE(test_pkg_data$has_tests_base) || has_base_R_in_tests_root(paths$tests_root)) {
    test_path_list$base_tests_root <- paths$tests_root
  }
  
  # Bioconductor unit tests (inst/unitTests)
  if (isTRUE(test_pkg_data$has_bioc_unit_tests) || dir.exists(paths$bioc_unitTests)) {
    test_path_list$bioc_unitTests <- paths$bioc_unitTests
  }
  
  # De-duplicate identical paths (e.g., tests/ might appear for multiple roles)
  if (length(test_path_list) > 0) {
    uniq_paths <- !duplicated(unname(unlist(test_path_list)))
    test_path_list <- test_path_list[uniq_paths]
  }
  
  # If nothing detected, return the provided testdir as a conservative default
  if (length(test_path_list) == 0) {
    test_path_list$default_tests_root <- paths$tests_root
  }
  
  return(test_path_list)
}