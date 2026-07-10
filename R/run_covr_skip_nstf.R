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
#' @param framework Optional character. When provided (e.g. from
#'   \code{run_one_framework}), restricts test paths to the framework-specific
#'   directory. Use when a package has multiple frameworks (e.g. testthat + testit)
#'   to avoid mixing paths across frameworks.
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
                               cov_env,
                               framework = NULL
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
  
  # restrict paths 
  test_path_list <- restrict_test_paths_to_framework(test_path_list, framework)
  
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
  
  # check for testit testing framework (unchanged)
  if (isTRUE(test_pkg_data$has_testit)) {
    problems <- tryCatch({
      check_covr_skip_testit(pkg_name, mapping, test_path)
    }, error = function(e) {
      cleanup_and_return_null(
        paste0("Error identifying skipped/problematic tests for ", pkg_name, " : ", e$message), 
        env = cov_env)
    })
  } else {
    # tinytest files live under inst/tinytest, not tests/; probe the correct
    # directory so skip detection finds the files (base/MASS keep tests/).
    nstf_base_dir <- if (isTRUE(test_pkg_data$has_tinytest)) {
      file.path(pkg_source_path, "inst", "tinytest")
    } else {
      file.path(pkg_source_path, "tests")
    }
    problems <- tryCatch({
      check_covr_skip_nstf(pkg_source_path, mapping, base_dir = nstf_base_dir)
    }, error = function(e) {
      cleanup_and_return_null(
        paste0("Error identifying skipped/problematic tests for ", pkg_name, " : ", e$message),
        env = cov_env)
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
  
  # ---- build test files list per framework ----
  if (isTRUE(test_pkg_data$has_testit)) {
    test_files_clean <- file.path(pkg_source_path, "tests", "testit", clean_mapping$test_file)
    if (!is.null(skip_tests) | (length(skip_tests) > 0)) {
      test_files_clean <- setdiff(test_files_clean, file.path(pkg_source_path, "tests", "testit", skip_tests))
    }
  } else if (isTRUE(test_pkg_data$has_tinytest)) {
    # tinytest lives in inst/tinytest
    test_files_clean <- file.path(pkg_source_path, "inst", "tinytest", clean_mapping$test_file)
    if (!is.null(skip_tests) | (length(skip_tests) > 0)) {
      test_files_clean <- setdiff(test_files_clean, file.path(pkg_source_path, "inst", "tinytest", skip_tests))
    }
    # do not include any tinytest runners
    tinytest_runner_basenames <- c("tinytest.R", "runTinyTests.R", "run_tinytest.R")
    test_files_clean <- test_files_clean[!(basename(test_files_clean) %in% tinytest_runner_basenames)]
    if (!is.null(test_pkg_data$tinytest_runner_paths)) {
      runner_paths_norm <- normalizePath(
        test_pkg_data$tinytest_runner_paths,
        winslash = "/",
        mustWork = FALSE
      )
      test_files_clean <- setdiff(test_files_clean, runner_paths_norm)
    }
    # keep only files that actually exist under inst/tinytest
    test_files_clean <- test_files_clean[file.exists(test_files_clean)]
  } else {
    # base / MASS / testthat
    test_files_clean <- file.path(pkg_source_path, "tests", clean_mapping$test_file)
    if (!is.null(skip_tests) | (length(skip_tests) > 0)) {
      test_files_clean <- setdiff(test_files_clean, file.path(pkg_source_path, "tests", skip_tests))
    }
  }
  
  # --------------------------------------------
  
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
    
    message("Loading Base R packages into coverage environment")
    base_r_loaded <- load_base_r_packages_into_env(
      env = cov_env,
      overwrite = FALSE
    )
    
    datasets <- load_package_datasets(pkg_name, env = cov_env)
    message("Datasets loaded: ", paste(datasets, collapse = ", "))
  }, error = function(e) {
    cleanup_and_return_null(
      paste0("Error loading package or datasets for ", pkg_name, " : ", e$message),
      env = cov_env)
  })
  
  # ---- compute coverage: tinytest vs. base/MASS vs. fallback ----
  if (isTRUE(test_pkg_data$has_tinytest)) {
    message(sprintf("running tinytest coverage for %s", pkg_name))
    tiny_cov <- tryCatch({
      create_tinytest_coverage(
        pkg_source_path = pkg_source_path,
        pkg_name        = pkg_name,
        tiny_dir        = file.path(pkg_source_path, "inst", "tinytest"),
        at_home         = TRUE,
        color           = FALSE
      )
    }, error = function(e) {
      cleanup_and_return_null(
        paste0("Error running tinytest coverage for ", pkg_name, " : ", e$message),
        env = cov_env
      )
    })
    if (is.null(tiny_cov)) return(NULL)
    coverage <- tiny_cov$coverage
    
    # Refine bookkeeping from the per-file run when available. Real runs return
    # `file_status` (keyed by full path); stubbed unit tests do not, so this is a
    # no-op there and the source-mapping based values are retained.
    fs <- tiny_cov$file_status
    if (!is.null(fs) && length(fs) > 0L) {
      ok_paths  <- names(fs)[fs == "ok"]
      err_paths <- names(fs)[fs != "ok"]
      test_files_clean <- ok_paths
      skip_tests       <- basename(err_paths)
      if (length(err_paths) > 0L) {
        err_df <- data.frame(
          test_file  = basename(err_paths),
          issue_type = unname(fs[err_paths]),
          stringsAsFactors = FALSE
        )
        no_skip <- is.null(problems) ||
          (nrow(problems) == 1L &&
             identical(problems$issue_type[[1L]], "No tests skipped"))
        problems <- if (no_skip) {
          err_df
        } else {
          rbind(problems[, c("test_file", "issue_type"), drop = FALSE], err_df)
        }
      }
    }
    
  } else {
    # Detect a base/MASS style layout: tests/ present and no tinytest/testit declared
    has_testthat_dir <- dir.exists(file.path(pkg_source_path, "tests", "testthat"))
    is_base_layout   <- !isTRUE(test_pkg_data$has_testit) &&
      !isTRUE(test_pkg_data$has_tinytest) &&
      !has_testthat_dir
    
    if (is_base_layout) {
      message(sprintf("running base-style tests coverage for %s", pkg_name))
      base_cov <- tryCatch({
        # Ensure absolute paths for sys.source() calls
        abs_tests <- normalizePath(test_files_clean, winslash = "/", mustWork = FALSE)
        create_base_tests_coverage(pkg_source_path, pkg_name, abs_tests)
      }, error = function(e) {
        cleanup_and_return_null(
          paste0("Error running base-style tests for ", pkg_name, " : ", e$message),
          env = cov_env
        )
      })
      if (is.null(base_cov)) return(NULL)
      coverage <- base_cov$coverage
    } else {
      # Fallback: environment_coverage for other layouts (e.g., testthat)
      message(paste0("running coverage manually for ", pkg_name))
      coverage <- tryCatch({
        covr::environment_coverage(
          env        = cov_env,
          test_files = test_files_clean
        )
      }, error = function(e) {
        cleanup_and_return_null(
          paste0("Error running coverage for ", pkg_name, " : ", e$message),
          env = cov_env
        )
      })
    }
  }
  
  # -----------------------------------------------------------
  
  if (!is.null(coverage)) {
    
    covr_list <- create_nstf_covr_list(coverage, pkg_name, 
                                       no_tests_df, test_files_clean,
                                       skip_tests, problems)
    
  } else {
    message("No env coverage generated. Falling back to covr::package_coverage(type = 'tests').")
    
    covr <- tryCatch({
      
      withr::with_dir(
        pkg_source_path,
        covr::package_coverage(
          path       = pkg_source_path,
          type       = "tests",
          quiet      = FALSE
        )
      )
      
      
    }, error = function(e) {
      
      message("package_coverage() failed: ", conditionMessage(e))
      NULL
      
    })
    
    
    covr_list <- create_nstf_covr_list(covr, pkg_name, 
                                       no_tests_df, test_files_clean,
                                       skip_tests, problems)
  }  
  
  if (is.null(covr_list$res_cov)) {
    message("No test coverage for this configuration")
    covr_list <- list(
      total_cov = 0,
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage   = matrix(0, nrow = 1, dimnames = list("No functions tested")),
          totalcoverage  = 0
        ),
        errors = "No testthat or testit configuration",
        notes  = NA
      )
    )
  }
  
  # Final cleanup (env bindings + temp workspace under tempdir())
  cleanup_and_return_null(env = cov_env)
  
  return(covr_list)
}


#' Check for skipped or errored test files (non-testit)
#'
#' This internal function attempts to run test files mapped to source files and
#' identifies any that produce warnings or errors. It does not handle `testit`.
#'
#' @param pkg_source_path Path to the root of the package source.
#' @param mapping A data frame with a column `test_file` listing test file names.
#' @param base_dir Directory containing the test files to probe. Defaults to
#'   `<pkg>/tests`, but may also be `inst/tinytest` or `tests/testthat`
#'   depending on package layout.
#'
#' @return A data frame listing test files and any issues encountered.
#' @keywords internal
#' @importFrom utils capture.output
check_covr_skip_nstf <- function(pkg_source_path, mapping,
                                 base_dir = file.path(pkg_source_path, "tests")) {
  
  test_dir <- base_dir
  pkg_name <- get_pkg_name(pkg_source_path)
  
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  
  # Detect test layout from base_dir
  norm_base   <- normalizePath(test_dir, winslash = "/", mustWork = FALSE)
  is_tinytest <- grepl("(?:/|\\\\)inst(?:/|\\\\)tinytest$",  norm_base, ignore.case = TRUE)
  is_testthat <- grepl("(?:/|\\\\)tests(?:/|\\\\)testthat$", norm_base, ignore.case = TRUE)
  # Base/MASS style: tests/ (without testthat subdir)
  is_base     <- grepl("(?:/|\\\\)tests$", norm_base, ignore.case = TRUE) && !is_testthat
  
  # Parent env for non-tinytest probing
  parent_env <- if (is_testthat) {
    asNamespace("testthat")
  } else if (is_base) {
    # set up asNamespace to load env properly
    # Use the package namespace so unqualified calls hit the loaded source build
    asNamespace(pkg_name)
  } else {
    baseenv()
  }
  
  problem_list <- lapply(clean_mapping$test_file, function(test) {
    # Absolute path so the file is located before chdir takes effect
    
    abs_test_path <- normalizePath(file.path(test_dir, test), winslash = "/", mustWork = FALSE)
    
    if (!file.exists(abs_test_path)) {
      return(data.frame(
        test_file  = test,
        issue_type = paste0("error: file not found: ", abs_test_path),
        stringsAsFactors = FALSE
      ))
    }
    
    res <- tryCatch(
      {
        if (is_tinytest) {
          # Run with tinytest runner (captures expectations as data; runs from
          # file dir). Output is suppressed so env-gated suites (e.g. Rcpp,
          # which exit_file() early when gates are unset) do not emit confusing
          # "0 tests ... [Exited]" probe noise. This probe is intentionally run
          # with gates off; only genuine errors/failed expectations are flagged.
          tt <- NULL
          suppressMessages(suppressWarnings(
            capture.output(
              tt <- tinytest::run_test_file(abs_test_path, at_home = TRUE, color = FALSE),
              file = NULL
            )
          ))
          if (tinytest::any_fail(tt)) "failed expectation" else NULL
        } else {
          # Probe by sourcing from the file's directory so relative paths behave as expected
          suppressMessages(suppressWarnings(
            capture.output(
              sys.source(abs_test_path, envir = new.env(parent = parent_env), chdir = TRUE),
              file = NULL
            )
          ))
          NULL
        }
      },
      warning = function(w) paste0("warning: ", conditionMessage(w)),
      error   = function(e) paste0("error: ",   conditionMessage(e))
    )
    
    if (!is.null(res)) {
      data.frame(test_file = test, issue_type = res, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  problems <- do.call(rbind, problem_list)
  
  if (is.null(problems) || nrow(problems) == 0) {
    problems <- data.frame(
      test_file  = NA_character_,
      issue_type = "No tests skipped",
      stringsAsFactors = FALSE
    )
  }
  problems
}

#' Run testit tests individually, capturing all warnings and errors, with dynamic package root detection
#'
#' @param pkg_name The package name (not used in this function, but kept for compatibility)
#' @param mapping A data frame with a column `test_file` listing test file names
#' @param test_path The path to the test directory (e.g., tests/testit)
#'
#' @return A data frame with test_file and issue_type columns
#' @keywords internal
#' @importFrom utils capture.output
#'
#' @family nstf_utility
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
#' This internal helper function scans the `R/` and `tests/` directories of a package
#' and attempts to match each source file with a corresponding test file based on filename similarity.
#'
#' @param pkg_source_path Character string. Path to the root of the package source directory.
#' @param test_dir character string. directory where tests are
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{source_file}{Name of the source file in the `R/` directory.}
#'   \item{test_file}{Name of the matched test file in the `tests/` directory, or \code{NA} if no match is found.}
#' }
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


#' Collect all applicable nonstandard test paths for a package (internal)
#'
#' Detects and returns all relevant testing directories used by an R package,
#' combining framework-specific and filesystem-based checks. Supported layouts:
#' \itemize{
#'   \item \strong{testthat}: \code{tests/testthat/}
#'   \item \strong{tinytest}: \code{inst/tinytest/} (primary location).
#'         Some packages also include a small runner under \code{tests/}
#'         (e.g., \code{tests/tinytest.R}) which this function does not return
#'         as a path, but it will still detect \code{inst/tinytest/}.
#'   \item \strong{testit} (standard): \code{tests/testit/}
#'   \item \strong{testit} (nonstandard): both \code{tests/test-ci/} and
#'         \code{tests/test-cran/} present, using \code{tests/} as the root
#'   \item \strong{base R tests}: \code{*.R} files directly under \code{tests/}
#'   \item \strong{Bioconductor unit tests}: \code{inst/unitTests/}
#' }
#'
#' The function returns a named list of paths for all detected testing setups.
#' It uses both the flags contained in \code{test_pkg_data} and direct filesystem
#' checks for robustness. If no paths are detected, the returned list contains
#' a single entry \code{default_tests_root = tests/}.
#'
#' @param test_pkg_data \code{list}. A list of booleans/fields describing detected
#'   test frameworks and layouts. All fields are optional and default to \code{FALSE}
#'   or \code{NULL} if missing:
#'   \itemize{
#'     \item \code{has_testthat}: logical; whether \code{tests/testthat/} is present.
#'     \item \code{has_tinytest} (or \code{has_tiny_test}): logical; whether
#'           tinytest is present. If true, the function will look for
#'           \code{inst/tinytest/} (or use \code{tinytest_dir} if provided).
#'     \item \code{tinytest_dir}: character; optional explicit path to the tinytest
#'           directory. May be absolute or relative to the package root; when
#'           missing, defaults to \code{inst/tinytest/}.
#'     \item \code{has_testit}: logical; whether \code{tests/testit/} or the
#'           nonstandard \code{tests/test-ci/ + tests/test-cran/} layout is present.
#'     \item \code{has_tests_base}: logical; whether \code{*.R} test files exist
#'           directly under \code{tests/}.
#'     \item \code{has_bioc_unit_tests}: logical; whether \code{inst/unitTests/}
#'           exists (Bioconductor layout).
#'   }
#'
#' @param testdir \code{character}. Path to the package \code{tests/} directory. The function
#'   also derives the package root as \code{dirname(testdir)} to locate paths such as
#'   \code{inst/unitTests/} and \code{inst/tinytest/}.
#'
#' @return \code{list}. A named list of character paths to all applicable test directories found.
#'   Possible names include:
#'   \itemize{
#'     \item \code{tinytest} — \code{inst/tinytest/}
#'     \item \code{testthat} — \code{tests/testthat/}
#'     \item \code{testit} — \code{tests/testit/}
#'     \item \code{nonstandard_testit_root} — \code{tests/} (when both \code{test-ci} and \code{test-cran} exist)
#'     \item \code{base_tests_root} — \code{tests/} (when base \code{*.R} files exist in \code{tests/} root)
#'     \item \code{bioc_unitTests} — \code{inst/unitTests/}
#'     \item \code{default_tests_root} — \code{tests/} (returned only if no other paths are detected)
#'   }
#'
#' @details
#' Identical paths arising from multiple detection routes (e.g., \code{tests/} playing
#' multiple roles) are de-duplicated while preserving the first descriptive label.
#' If \code{tinytest_dir} is provided and relative, it is resolved against the
#' package root (i.e., \code{dirname(testdir)}).
#'
#' @keywords internal
#'
#' @family nstf_utility
get_nstf_test_path <- function(test_pkg_data, testdir) {
  
  # Normalize key dirs
  testdir  <- normalizePath(testdir, winslash = "/", mustWork = FALSE)
  pkg_root <- normalizePath(file.path(testdir, ".."), winslash = "/", mustWork = FALSE)
  
  # Helper: absolute-path check (POSIX and Windows)
  is_absolute_path <- function(p) {
    is.character(p) && length(p) == 1L && nzchar(p) &&
      grepl("^(/|[A-Za-z]:[/\\])", p)
  }
  
  # Resolve tinytest directory from test_pkg_data or default to inst/tinytest
  resolve_tinytest_dir <- function() {
    # prefer explicit tinytest_dir if provided
    td <- test_pkg_data$tinytest_dir
    if (!is.null(td) &&
        !is.na(td) &&
        is.character(td) &&
        length(td) == 1L &&
        nzchar(td)) {
      if (!is_absolute_path(td)) td <- file.path(pkg_root, td)
      return(normalizePath(td, winslash = "/", mustWork = FALSE))
    }
    # default: <pkg_root>/inst/tinytest
    normalizePath(file.path(pkg_root, "inst", "tinytest"), winslash = "/", mustWork = FALSE)
  }
  
  # Candidate paths
  paths <- list(
    testthat       = file.path(testdir, "testthat"),
    testit         = file.path(testdir, "testit"),
    test_ci        = file.path(testdir, "test-ci"),
    test_cran      = file.path(testdir, "test-cran"),
    tests_root     = testdir,
    bioc_unitTests = file.path(pkg_root, "inst", "unitTests"),
    tinytest       = resolve_tinytest_dir()
  )
  
  # Helper: detect base R scripts directly in tests/ root
  has_base_R_in_tests_root <- function(tdir) {
    if (!dir.exists(tdir)) return(FALSE)
    files <- list.files(tdir, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
    length(files) > 0
  }
  
  # Build the output list of active test paths
  test_path_list <- list()
  
  # --- tinytest (new) ---
  # accept both has_tinytest and has_tiny_test for robustness
  if (isTRUE(test_pkg_data$has_tinytest) || isTRUE(test_pkg_data$has_tiny_test) || dir.exists(paths$tinytest)) {
    test_path_list$tinytest <- paths$tinytest
  }
  
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
  if (!(isTRUE(test_pkg_data$has_tinytest) || dir.exists(paths$tinytest))) {
    if (isTRUE(test_pkg_data$has_tests_base) || has_base_R_in_tests_root(paths$tests_root)) {
      test_path_list$base_tests_root <- paths$tests_root
    }
  }
  
  # Bioconductor unit tests (inst/unitTests)
  # Note: caller may pass test_pkg_data$has_bioc_unit_tests (your previous function used has_bioc_unit_tests)
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


#' Enable environment-gated tinytest suites (internal)
#'
#' Some packages gate their entire \pkg{tinytest} suite behind environment
#' variables, exiting each test file early via \code{tinytest::exit_file()}
#' unless a variable is set to a specific value. \pkg{Rcpp} is the canonical
#' example: every file under \code{inst/tinytest} begins with a guard such as
#' \code{if (Sys.getenv("RunAllRcppTests") != "yes") exit_file(...)}, so a plain
#' \code{tinytest::run_test_dir()} executes zero tests and reports 0\% coverage.
#'
#' This helper scans the headers of the discovered test files for gate patterns
#' of the form \code{Sys.getenv("VAR") != "VALUE"} and sets the discovered
#' variables to their required values so the tests run during coverage. It is
#' generic: any package using this idiom (not only \pkg{Rcpp}) benefits without
#' hard-coding package names. Packages without such gates (e.g. \pkg{digest})
#' produce no matches, so nothing is changed and behaviour is identical to
#' before.
#'
#' Both gate idioms are recognised: the primary \dQuote{exit unless opted in}
#' form (\code{Sys.getenv("VAR") != "VALUE"}) and the complementary
#' \code{== "VALUE"} form used for secondary \dQuote{verbose} suites
#' (e.g. \code{RunVerboseRcppTests}). In both cases the suite runs only when
#' \code{VAR == VALUE}, so the variable is set to that value. Verbose suites
#' exercise packaging helpers in-process (which covr can trace) but may also run
#' \code{R CMD build} / \code{install.packages} in child processes that can fail;
#' \code{create_tinytest_coverage()} therefore drives test files in isolation so
#' one failure cannot stop the run.
#'
#' The function is intentionally defensive and never throws: on any problem it
#' returns a no-op result so the surrounding tinytest coverage path is
#' unaffected. Callers should register the returned \code{teardown} via
#' \code{on.exit()} to restore the previous environment state.
#'
#' @param tiny_dir Character scalar. Path to the tinytest directory
#'   (typically \code{inst/tinytest}).
#' @param max_files Integer scalar. Maximum number of test files to scan.
#'   Defaults to \code{40L}.
#' @param header_lines Integer scalar. Number of lines read from the top of
#'   each test file when searching for gates. Defaults to \code{60L}.
#'
#' @return A list with components:
#' \describe{
#'   \item{set_vars}{Named character vector of environment variables that were
#'     set (variable name to required value); empty when no gates are found.}
#'   \item{teardown}{A function that restores the previous environment state
#'     (unsetting variables that were previously unset).}
#' }
#'
#' @keywords internal
#'
#' @family nstf_utility
prepare_tinytest_run_env <- function(tiny_dir,
                                     max_files = 40L,
                                     header_lines = 60L) {
  
  noop <- list(set_vars = character(0), teardown = function() invisible(NULL))
  
  result <- tryCatch({
    
    if (!is.character(tiny_dir) || length(tiny_dir) != 1L ||
        is.na(tiny_dir) || !nzchar(tiny_dir) || !dir.exists(tiny_dir)) {
      return(noop)
    }
    
    test_files <- list.files(
      tiny_dir,
      pattern    = "\\.[rR]$",
      full.names = TRUE,
      recursive  = TRUE
    )
    
    # Never scan tinytest runners; they are not gated test files.
    runner_basenames <- c("tinytest.R", "runTinyTests.R", "run_tinytest.R")
    test_files <- test_files[!(basename(test_files) %in% runner_basenames)]
    
    if (length(test_files) == 0L) {
      return(noop)
    }
    
    # Match both gate idioms (single or double quotes):
    #   Sys.getenv("VAR" [, ...]) != "VALUE"   -> exit_file() UNLESS VAR == VALUE
    #   Sys.getenv("VAR" [, ...]) == "VALUE"   -> ".run <- ... ; if (!.run) exit"
    # In both forms the suite runs only when VAR == VALUE, so setting VAR = VALUE
    # enables it. The "==" form covers Rcpp's secondary "verbose" suites
    # (e.g. RunVerboseRcppTests). Those exercise packaging helpers
    # (Rcpp.package.skeleton, compileAttributes, exposeClass) in-process, which
    # covr can trace; their R CMD build / install.packages steps run in child
    # processes (uncoverable) and may fail, which is why the coverage run drives
    # files in isolation (see create_tinytest_coverage()).
    gate_re <- "Sys\\.getenv\\(\\s*[\"']([^\"']+)[\"'][^)]*\\)\\s*(?:!=|==)\\s*[\"']([^\"']+)[\"']"
    
    n_scan <- min(as.integer(max_files), length(test_files))
    gates  <- character(0)
    
    for (tf in test_files[seq_len(n_scan)]) {
      hdr <- tryCatch(
        readLines(tf, n = as.integer(header_lines), warn = FALSE),
        error = function(e) character(0)
      )
      if (length(hdr) == 0L) next
      
      # gregexpr captures every gate on every header line (a line may hold
      # more than one, e.g. RunVerboseRcppTests && RunAllRcppTests).
      matches <- unlist(
        regmatches(hdr, gregexpr(gate_re, hdr, perl = TRUE)),
        use.names = FALSE
      )
      for (m in matches) {
        var <- sub(gate_re, "\\1", m, perl = TRUE)
        val <- sub(gate_re, "\\2", m, perl = TRUE)
        if (nzchar(var)) gates[var] <- val
      }
    }
    
    if (length(gates) == 0L) {
      return(noop)
    }
    
    var_names <- names(gates)
    old_vals  <- Sys.getenv(var_names, unset = NA_character_, names = TRUE)
    
    # Set every discovered gate variable to its required value.
    do.call(Sys.setenv, as.list(gates))
    
    teardown <- function() {
      for (nm in var_names) {
        ov <- old_vals[[nm]]
        if (is.na(ov)) {
          Sys.unsetenv(nm)
        } else {
          args <- list(ov)
          names(args) <- nm
          do.call(Sys.setenv, args)
        }
      }
      invisible(NULL)
    }
    
    list(set_vars = gates, teardown = teardown)
    
  }, error = function(e) {
    message("prepare_tinytest_run_env: skipping env gate setup : ", conditionMessage(e))
    noop
  })
  
  result
}


#' Add dev-tree include paths for self-compiling tinytest suites (internal)
#'
#' Some \pkg{tinytest} files compile inline C/C++ during the run (e.g. via
#' \code{Rcpp::sourceCpp()}). When coverage loads the package from its source
#' tree with \code{pkgload::load_all()}, public headers live under
#' \code{inst/include/}, but the compiler flags emitted by self-compilation
#' helpers typically reference the \emph{installed} layout (\code{include/}).
#' Packages that ship headers in \code{inst/include} (e.g. \pkg{Rcpp}) therefore
#' fail with \sQuote{Rcpp.h: No such file or directory} unless the source-tree
#' include directory is prepended to the preprocessor flags.
#'
#' This helper detects \code{inst/include/} under \code{pkg_source_path}, optionally
#' adds \code{<tiny_dir>/cpp/} when present, and prepends corresponding \code{-I}
#' flags to \code{PKG_CPPFLAGS} and \code{CLINK_CPPFLAGS}. Packages without
#' \code{inst/include/} (e.g. \pkg{digest}) are unchanged.
#'
#' The function is intentionally defensive and never throws: on any problem it
#' returns a no-op result so the surrounding tinytest coverage path is
#' unaffected. Callers should register the returned \code{teardown} via
#' \code{on.exit()} to restore the previous environment state.
#'
#' @param pkg_source_path Character scalar. Path to the package source root.
#' @param tiny_dir Character scalar. Path to the tinytest directory
#'   (typically \code{inst/tinytest}).
#'
#' @return A list with components:
#' \describe{
#'   \item{include_dirs}{Character vector of absolute include directories that
#'     were added; empty when no dev-tree headers are present.}
#'   \item{teardown}{A function that restores the previous \code{PKG_CPPFLAGS}
#'     and \code{CLINK_CPPFLAGS} values (unsetting variables that were
#'     previously unset).}
#' }
#'
#' @keywords internal
#'
#' @family nstf_utility
prepare_tinytest_dev_includes <- function(pkg_source_path, tiny_dir) {
  
  noop <- list(include_dirs = character(0), teardown = function() invisible(NULL))
  
  result <- tryCatch({
    
    if (!is.character(pkg_source_path) || length(pkg_source_path) != 1L ||
        is.na(pkg_source_path) || !nzchar(pkg_source_path)) {
      return(noop)
    }
    
    inst_include <- file.path(pkg_source_path, "inst", "include")
    if (!dir.exists(inst_include)) {
      return(noop)
    }
    
    include_dirs <- normalizePath(inst_include, winslash = "/", mustWork = FALSE)
    
    if (is.character(tiny_dir) && length(tiny_dir) == 1L &&
        !is.na(tiny_dir) && nzchar(tiny_dir)) {
      cpp_dir <- file.path(tiny_dir, "cpp")
      if (dir.exists(cpp_dir)) {
        include_dirs <- c(
          include_dirs,
          normalizePath(cpp_dir, winslash = "/", mustWork = FALSE)
        )
      }
    }
    
    i_flags <- paste(sprintf("-I\"%s\"", include_dirs), collapse = " ")
    
    cpp_env_vars <- c("PKG_CPPFLAGS", "CLINK_CPPFLAGS")
    old_vals <- Sys.getenv(cpp_env_vars, unset = NA_character_, names = TRUE)
    
    for (ev in cpp_env_vars) {
      old <- old_vals[[ev]]
      new_val <- if (is.na(old) || !nzchar(old)) {
        i_flags
      } else {
        paste(i_flags, old)
      }
      args <- list(new_val)
      names(args) <- ev
      do.call(Sys.setenv, args)
    }
    
    teardown <- function() {
      for (ev in cpp_env_vars) {
        ov <- old_vals[[ev]]
        if (is.na(ov)) {
          Sys.unsetenv(ev)
        } else {
          args <- list(ov)
          names(args) <- ev
          do.call(Sys.setenv, args)
        }
      }
      invisible(NULL)
    }
    
    list(include_dirs = include_dirs, teardown = teardown)
    
  }, error = function(e) {
    message(
      "prepare_tinytest_dev_includes: skipping dev include setup : ",
      conditionMessage(e)
    )
    noop
  })
  
  result
}


#' Create coverage from tinytest under covr instrumentation (internal)
#'
#' Instruments the package **namespace** with \pkg{covr}, runs all
#' \pkg{tinytest} files in \code{inst/tinytest}, and returns a covr coverage
#' object together with the tinytest results. This mirrors tinytest's execution
#' model (expectation helpers available; tests run from each file's directory)
#' while ensuring calls into the package namespace are traced.
#'
#' @details
#' Intended for **source-tree** coverage of tinytest suites:
#' \itemize{
#'   \item The package namespace is traced via \code{covr::trace_environment()}
#'         so that calls made by tests to package functions are recorded
#'         for coverage.
#'   \item \code{NOT_CRAN} is set to \code{"true"} during the run (restored on exit)
#'         so tinytest \code{at_home()} logic treats the run as local.
#'   \item Environment-gated suites (e.g. \pkg{Rcpp}'s \code{RunAllRcppTests})
#'         are enabled via \code{prepare_tinytest_run_env()} before the run and
#'         restored on exit, so gated tests execute instead of exiting early.
#'   \item Packages with headers in \code{inst/include/} that self-compile during
#'         tests receive dev-tree \code{-I} flags via
#'         \code{prepare_tinytest_dev_includes()} (restored on exit).
#'   \item Test files are driven by \code{tinytest::run_test_dir()} from
#'         \code{file.path(pkg_source_path, "inst", "tinytest")}. A fatal
#'         top-level error in a single file (e.g. a test that asserts the
#'         installed package layout, which differs from a \code{load_all} source
#'         tree) is caught so it does not discard coverage already recorded for
#'         the files that ran.
#'   \item Coverage is built from covr's counters using \code{covr::as_coverage()}.
#'   \item Objects that test files create in \code{.GlobalEnv} are removed on
#'         exit. Some suites (e.g. \pkg{Rcpp}) call \code{Rcpp::sourceCpp()},
#'         whose \code{env} argument defaults to \code{globalenv()}, exporting
#'         many compiled wrappers into the user's workspace. \code{.GlobalEnv} is
#'         snapshotted before the run and only newly-added objects are removed
#'         afterwards, so pre-existing objects are preserved.
#' }
#'
#' \strong{Preconditions}
#' \itemize{
#'   \item The package under test should already be \emph{loaded} (e.g.,
#'         \code{pkgload::load_all(pkg_source_path)}), so that
#'         \code{asNamespace(pkg_name)} exists and reflects the current source.
#'   \item The directory \code{inst/tinytest} must exist and contain test files.
#' }
#'
#' @param pkg_source_path Character scalar. Path to the package source root.
#' @param pkg_name        Character scalar. Package name (as in \code{DESCRIPTION}).
#' @param tiny_dir        Optional override for the tinytest directory. Defaults
#'                        to \code{file.path(pkg_source_path, "inst", "tinytest")}.
#' @param at_home         Logical; passed to \code{tinytest::run_test_dir()}.
#'                        Defaults to \code{TRUE}.
#' @param color           Logical; color output from tinytest. Defaults to \code{FALSE}.
#'
#' @return A list with components:
#' \describe{
#'   \item{coverage}{An object of class \code{"coverage"} (from \pkg{covr}).}
#'   \item{tinytests}{The \code{tinytests} object returned by tinytest.}
#' }
#'
#' @keywords internal
#'
#' @family nstf_utility
create_tinytest_coverage <- function(pkg_source_path,
                                     pkg_name,
                                     tiny_dir = file.path(pkg_source_path, "inst", "tinytest"),
                                     at_home = TRUE,
                                     color = FALSE) {
  
  message(sprintf("running tinytest coverage for %s", pkg_name))
  
  
  # Resolve covr internals in a version-agnostic way (exported or not)
  covr_ns <- asNamespace("covr")
  trace_env     <- get("trace_environment",  envir = covr_ns)  # exported in newer covr
  reset_traces  <- get("reset_traces",       envir = covr_ns)
  clear_counters<- get("clear_counters",     envir = covr_ns)
  as_coverage   <- get("as_coverage",        envir = covr_ns)
  
  # 1) Trace the *namespace* so tinytest calls are instrumented
  ns_env <- asNamespace(pkg_name)
  trace_env(ns_env)  # covr::trace_environment(ns_env) when exported
  
  # Always clean up covr traces/counters
  on.exit({
    reset_traces()
    clear_counters()
  }, add = TRUE)
  
  # 2) Ensure "local run" semantics (restored on exit)
  old_notcran <- Sys.getenv("NOT_CRAN", unset = NA)
  on.exit({
    if (is.na(old_notcran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv(NOT_CRAN = old_notcran)
  }, add = TRUE)
  Sys.setenv(NOT_CRAN = "true")
  
  # 2b) Enable environment-gated tinytest suites (e.g. Rcpp's RunAllRcppTests).
  #     Variables are restored on exit even if the run below errors. Packages
  #     without gates (e.g. digest) yield an empty result and no-op teardown.
  tiny_env <- prepare_tinytest_run_env(tiny_dir)
  on.exit(tiny_env$teardown(), add = TRUE)
  if (length(tiny_env$set_vars) > 0L) {
    message(sprintf(
      "Enabling gated tinytest variable(s) for %s: %s",
      pkg_name,
      paste(names(tiny_env$set_vars), unname(tiny_env$set_vars),
            sep = "=", collapse = ", ")
    ))
  }
  
  # 2c) Dev-tree include paths for self-compiling tests (e.g. Rcpp::sourceCpp).
  #     Restored on exit even if the run below errors. No-op without inst/include.
  dev_includes <- prepare_tinytest_dev_includes(pkg_source_path, tiny_dir)
  on.exit(dev_includes$teardown(), add = TRUE)
  if (length(dev_includes$include_dirs) > 0L) {
    message(sprintf(
      "Adding dev-tree include path(s) for %s: %s",
      pkg_name,
      paste(dev_includes$include_dirs, collapse = ", ")
    ))
  }
  
  # 2d) Snapshot .GlobalEnv so objects that tinytest files create there can be
  #     removed afterwards. Rcpp's tests call Rcpp::sourceCpp() (and friends),
  #     whose `env` argument defaults to globalenv(), so the generated wrapper
  #     functions are exported into the user's workspace. Only objects added
  #     during the run are removed on exit; pre-existing objects (and anything a
  #     caller such as get_package_coverage() stores later) are preserved. This
  #     keeps the run compliant with the "no .GlobalEnv modifications" rule.
  global_before <- ls(envir = globalenv(), all.names = TRUE)
  on.exit({
    new_globals <- setdiff(ls(envir = globalenv(), all.names = TRUE), global_before)
    if (length(new_globals) > 0L) {
      suppressWarnings(rm(list = new_globals, envir = globalenv()))
      message(sprintf(
        "Removed %d object(s) created in .GlobalEnv during %s tinytest coverage",
        length(new_globals), pkg_name
      ))
    }
  }, add = TRUE)
  
  # 3) Run each tinytest file in isolation. A single file may throw a fatal
  #    top-level error (e.g. a failed R CMD build in a verbose suite, or Rcpp's
  #    test_system.R, which asserts the *installed* include path that is empty
  #    under pkgload::load_all). Running per file with run_test_file() ensures
  #    such an error cannot stop the remaining files: covr counters accumulate
  #    globally across calls, so coverage reflects every file that ran. This
  #    mirrors run_test_dir()'s default selection and per-file working directory.
  test_files <- list.files(
    tiny_dir,
    pattern    = "^test.*\\.[rR]$",
    full.names = TRUE
  )
  runner_basenames <- c("tinytest.R", "runTinyTests.R", "run_tinytest.R")
  test_files <- sort(test_files[!(basename(test_files) %in% runner_basenames)])
  
  tt_list     <- list()
  file_status <- character(0)   # keyed by full path: "ok" or "error: <msg>"
  
  for (tf in test_files) {
    res <- tryCatch(
      tinytest::run_test_file(tf, at_home = at_home, color = color),
      error = function(e) {
        message(sprintf(
          "tinytest file %s stopped early (%s); continuing with remaining files",
          basename(tf), conditionMessage(e)
        ))
        structure(list(message = conditionMessage(e)),
                  class = "tinytest_file_error")
      }
    )
    if (inherits(res, "tinytest_file_error")) {
      file_status[tf] <- paste0("error: ", res$message)
    } else {
      tt_list[[tf]]   <- res
      file_status[tf] <- "ok"
    }
  }
  
  # 4) Convert covr's recorded counters into a coverage object
  counters <- get(".counters", envir = covr_ns)
  coverage <- as_coverage(counters)
  
  list(
    coverage    = coverage,
    tinytests   = tt_list,
    test_files  = test_files,
    file_status = file_status
  )
}


#' Create coverage by running base-style tests under covr (internal)
#'
#' Traces the package namespace with covr, then sources each test script
#' from its own directory (so relative file references like 'load("solr.rda")'
#' in MASS/tests work). Converts covr counters to a coverage object.
#'
#' @param pkg_source_path Character; package source root.
#' @param pkg_name        Character; package name.
#' @param test_files      Character vector of test file paths (absolute or relative).
#' @return list(coverage = <covr coverage object>)
#' @keywords internal
#'
#' @family nstf_utility
create_base_tests_coverage <- function(pkg_source_path, pkg_name, test_files) {
  
  if (!pkg_name %in% loadedNamespaces()) {
    stop("Namespace '", pkg_name, "' is not loaded. ",
         "Call pkgload::load_all(pkg_source_path) first.", call. = FALSE)
  }
  
  covr_ns        <- asNamespace("covr")
  trace_env      <- get("trace_environment",  envir = covr_ns)
  reset_traces   <- get("reset_traces",       envir = covr_ns)
  clear_counters <- get("clear_counters",     envir = covr_ns)
  as_coverage    <- get("as_coverage",        envir = covr_ns)
  
  # trace the *package namespace* so calls from tests are instrumented
  trace_env(asNamespace(pkg_name))
  on.exit({ reset_traces(); clear_counters() }, add = TRUE)
  
  # source each test from its own directory so relative files (e.g., 'solr.rda') are found
  # MASS has tests/glmmPQL.R which does load("solr.rda") from tests/. 
  
  for (tf in test_files) {
    tf_abs <- normalizePath(tf, winslash = "/", mustWork = TRUE)
    try(
      sys.source(tf_abs, envir = new.env(parent = asNamespace(pkg_name)), chdir = TRUE),
      silent = TRUE
    )
  }
  
  coverage <- as_coverage(get(".counters", envir = covr_ns))
  list(coverage = coverage)
}

#' Restrict a list of test paths to a single framework
#'
#' Filters a named list of candidate test directories (typically produced by
#' \code{get_nstf_test_path()}) down to the entry that corresponds to a requested
#' framework. If no usable mapping can be made, the input list is returned
#' unchanged.
#'
#' The function maps \code{framework} to a primary key in \code{test_path_list}
#' as follows:
#' \itemize{
#'   \item \code{"testthat"}   → \code{"testthat"}
#'   \item \code{"testit"}     → \code{"testit"} (falls back to
#'         \code{"nonstandard_testit_root"} if \code{"testit"} is \code{NULL})
#'   \item \code{"tests_base"} → \code{"base_tests_root"}
#'   \item \code{"tinytest"}   → \code{"tinytest"}
#' }
#'
#' @param test_path_list \code{list}. Named list of candidate test paths. Known
#'   names include \code{"testthat"}, \code{"testit"}, \code{"tinytest"},
#'   \code{"base_tests_root"}, and optionally \code{"nonstandard_testit_root"}.
#' @param framework \code{character(1)} or \code{NULL}. Framework identifier
#'   (e.g., \code{"testthat"}, \code{"testit"}, \code{"tests_base"},
#'   \code{"tinytest"}). If \code{NULL} or empty (\code{""}), the list is
#'   returned unchanged.
#'
#' @return \code{list}. Either:
#' \itemize{
#'   \item a single-element named list containing only the resolved framework
#'         path (name is the selected key), or
#'   \item the original \code{test_path_list} if \code{framework} is
#'         \code{NULL}/empty, maps to an unknown key, or the resolved key is
#'         absent in \code{test_path_list}.
#' }
#'
#' @details
#' For \code{framework == "testit"}, if \code{test_path_list[["testit"]]} is
#' \code{NULL} and \code{test_path_list[["nonstandard_testit_root"]]} is present,
#' the latter is used instead. No directories are created or validated here; the
#' function only selects from the provided list.
#'
#' @seealso \code{\link{get_nstf_test_path}}
#' @family nstf_utility
#' @keywords internal
restrict_test_paths_to_framework <- function(test_path_list, framework) {
  out <- test_path_list
  if (!is.null(framework) && nzchar(framework)) {
    primary_key <- switch(
      framework,
      testit     = "testit",
      tests_base = "base_tests_root",
      tinytest   = "tinytest",
      testthat   = "testthat",
      NULL
    )
    if (!is.null(primary_key)) {
      fw_key <- primary_key
      if (identical(framework, "testit") &&
          is.null(test_path_list[["testit"]]) &&
          !is.null(test_path_list[["nonstandard_testit_root"]])) {
        fw_key <- "nonstandard_testit_root"
      }
      if (!is.null(test_path_list[[fw_key]])) {
        out <- setNames(list(test_path_list[[fw_key]]), fw_key)
      }
    }
  }
  return(out)
}

#' Create the NSTF Coverage List
#'
#' Internal helper that constructs the final `covr_list` structure from a
#' coverage object. It wraps extraction of coverage information, performs
#' total coverage computation, and assembles all diagnostic components.
#'
#' @param coverage A coverage object returned from covr.
#' @param pkg_name A Character string - package name.
#' @param no_tests_df A dataframe - functions with no tests.
#' @param test_files_clean A Character vector - functions with working tests
#' @param skip_tests A Character vector - functions skipped
#' @param problems A Character vector - test problems
#' 
#' @return A named list (`covr_list`) containing:
#'   \describe{
#'     \item{total_cov}{Numeric; overall package coverage percentage.}
#'     \item{res_cov}{List; raw coverage extraction results with coverage data, errors, notes.}
#'     \item{functions_no_tests}{Data frame of functions lacking tests.}
#'     \item{tests_skipped}{Character vector of skipped test files.}
#'     \item{tests_passing}{Character vector of passing test files.}
#'     \item{tests_problems}{Character vector describing test problems.}
#'   }
#'   
#' @family nstf_utility
#' @keywords internal
create_nstf_covr_list <- function(coverage, pkg_name,
                                  no_tests_df, 
                                  test_files_clean,
                                  skip_tests,
                                  problems
) {
  
  # safely extract coverage to list
  res_cov <- tryCatch({
    coverage_list <- covr::coverage_to_list(coverage)
    list(
      name     = coverage$package,
      coverage = coverage_list,
      errors   = NA,
      notes    = NA
    )
  },
  error = function(cond) {
    coverage_list <- list(filecoverage = NA, totalcoverage = NA_integer_)
    list(
      name     = coverage$package,
      coverage = coverage_list,
      errors   = cond,
      notes    = NA
    )
  })
  
  # extract percent coverage
  percent_cov <- as.numeric(res_cov$coverage$totalcoverage)
  
  # inputs expected by compute_total_coverage()
  functions_no_tests_df <- no_tests_df
  tests_passing         <- test_files_clean
  tests_skipped         <- skip_tests
  problems              <- problems
  
  # compute combined coverage information
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
  n_skipped_files     <- calc_covr_list$n_skipped_files  
  
  message(sprintf(
    "Approx total coverage: (%.2f%%), counts: tested_files=%d, untested_functions=%d, skipped_tests=%d",
    percent_cov, n_testfiles, n_no_function_tests, n_skipped_files
  ))
  
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
    tests_passing      = test_files_clean,
    tests_problems     = problems
  )
  
  return(covr_list)
}

