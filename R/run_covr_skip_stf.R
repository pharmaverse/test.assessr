#' Run Coverage with Skipped Failing Tests (Standard Testing Framework)
#'
#' This function runs testthat-based test execution and code coverage
#' while identifying and skipping failing tests. It is designed for use
#' with standard testing frameworks (STF), primarily `testthat`, and
#' supports automated skip mapping to enable coverage calculation even
#' when some tests fail.
#'
#' The function:
#' \itemize{
#'   \item executes tests and collects detailed test results,
#'   \item identifies failing, erroring, or skipped tests,
#'   \item constructs skip maps to temporarily comment out failing test lines,
#'   \item computes coverage on the remaining tests, and
#'   \item returns a combined object containing coverage and test summaries.
#' }
#'
#' @param pkg_source_path Character. Path to the root directory of the
#'   package source.
#' @param test_pkg_data List. Output from 
#'   \code{check_pkg_tests_and_snaps()}, containing information about
#'   the package's testing setup (presence of testthat, snapshots, etc.).
#' @param cov_env Environment that holds the temporary coverage workspace and
#'   shared state for the run. This must be an environment created by
#'   `initialise_covr_env()`, which sets `cov_env$work_dir` to a unique
#'   subdirectory under `tempdir()`. `run_covr_skip_stf()` will not create or
#'   modify `cov_env` beyond reading helpers into it; all file outputs produced
#'   by downstream helpers are expected to be written under
#'   `cov_env$work_dir` (never to the package directory or the user's home).
#'   The caller is responsible for lifecycle management; register
#'   `on.exit(cleanup_and_return_null(env = cov_env), add = TRUE)` in the
#'   calling scope to ensure bindings and the temporary workspace are cleaned
#'   up even on error.

#'
#' @return 
#' A named list containing coverage results and test diagnostics. The structure
#' includes:
#' \describe{
#'   \item{total_cov}{Numeric. Overall coverage percentage.}
#'   \item{res_cov}{A nested list with file-level coverage, errors, and notes.}
#'   \item{long_summary}{Long-format test results including PASS/FAIL/ERROR/SKIPPED.}
#'   \item{test_skip}{A data frame describing test lines skipped during coverage,
#'     if applicable.}
#' }
#'
#' If no tests fail, the returned list describes full coverage based on
#' unskipped tests. If no valid STF test directory is found, an empty coverage
#' object is returned via \code{create_empty_covr_list()}.
#'
#' @rdname run_covr_skip_stf  
#' @family stf_utility
#' @keywords internal
run_covr_skip_stf <- function(pkg_source_path, 
                              test_pkg_data, 
                              cov_env
                              ) {
  
  # Input checking
  checkmate::assert_string(pkg_source_path)
  checkmate::assert_directory_exists(pkg_source_path)
  
  # Ensure the argument was provided
  checkmate::assert_true(!missing(cov_env), .var.name = "cov_env")
  
  # Ensure it is an environment
  checkmate::assert_environment(cov_env, .var.name = "cov_env")
  
  path <- as.character(pkg_source_path)
  testdir <- file.path(path, "tests")
  pkg <- get_pkg_name(pkg_source_path)
  
  test_path <- get_stf_test_path(test_pkg_data, testdir)
  
  if (is.null(test_path)) {
    message(paste0("No test directory found for ", pkg))
    return(create_empty_covr_list(pkg, "No test directory found"))
  }
  
  # --- Ensure WD is restored (CRAN reviewer request) ---
  original_wd <- getwd()
  setwd(path)
  on.exit(setwd(original_wd), add = TRUE)
  
  # --- Setup coverage environment here (no env creation; CRAN-safe) ---
  # This will NOT write to tests/; it just loads helpers into the provided env.
  covr_setup <- setup_covr_env(pkg_source_path = pkg_source_path,
                               pkg_name        = pkg,
                               test_path       = test_path,
                               cov_env         = cov_env)
  
  # Optionally stash metadata onto env for downstream consumers (non-breaking)
  cov_env$datasets_loaded <- covr_setup$datasets_loaded
  cov_env$root_dir        <- covr_setup$root_dir
  cov_env$initial_state   <- covr_setup$initial_state
  cov_env$helpers_loaded  <- covr_setup$helpers_loaded
  
  message(paste0("Starting reporter process for ", pkg))
  testthat::set_max_fails(Inf)
  
  # unload target package
  try(unloadNamespace(pkg), silent = TRUE)
  
  # create fail reporter
  create_fail_reporter(test_path, pkg, cov_env)
  
  # create default reporter
  test_results <- create_test_reporter(test_path, pkg, cov_env)
  
  if (is.null(test_results)) {
    return(create_empty_covr_list(pkg, "Default reporter execution failed"))
  } else {
    # update context column 
    test_results <- fix_test_context(test_results)
  }
  
  message(paste0("Mapping package test structure for ", pkg))
  map_path <- test_path
  test_map <- tryCatch({
    map_tests_stf(map_path)
  }, error = function(e) {
    message(paste0("Error mapping tests for ", pkg, " : ", e$message))
    return(NULL)
  })
 
  if (is.null(test_map)) {
    message(paste0("Test map could not be created for ", pkg))
    return(create_empty_covr_list(pkg, "Test map creation failed"))
  }
  
  test_results_long <- get_tests_long_sum_stf(test_results)
  
  if (is.null(test_results_long)) {
    message(paste0("Unable to create skipped tests for :", pkg))
    return(create_empty_covr_list(pkg, 
                                  "Skipped tests map creation failed", 
                                  test_pkg_data = test_pkg_data))
  }
  
  lookup <- c(code_script = "context", context = "test")
  test_results_long <- dplyr::rename(test_results_long, dplyr::all_of(lookup))
  
  test_results_filtered <- test_results_long[test_results_long$status != 'PASS', ]
  test_results_filtered$file <- gsub('#(.*?)$', '', basename(test_results_filtered$file))
  test_map <- dplyr::rename(test_map, context = "test")
  
  status_filter <- c("FAILED", "ERROR", "SKIPPED")
  problem_tests <- subset(test_results_filtered, status %in% status_filter)
  
  if (nrow(problem_tests) == 0) {
    problem_tests <- data.frame(
      file = "", code_script = "", context = "", status = "",
      n = NA_integer_, time = NA_real_, stringsAsFactors = FALSE
    )
  }
  
  failed <- any(test_results_long$status %in% c('ERROR', 'FAILED'))
  
  if (failed) {
    test_skip <- get_tests_skip_stf(problem_tests, test_map)
    message(paste0("Creating skip test map for ", pkg))
    
    # --- Pass cov_env down (create_coverage_skip_stf signature updated elsewhere) ---
    cvr <- create_coverage_skip_stf(pkg_source_path = pkg_source_path,
                                    pkg             = pkg,
                                    test_path       = map_path,
                                    test_skip       = test_skip,
                                    test_map        = test_map,
                                    cov_env         = cov_env)
    
    message(paste0("Creating test coverage for ", pkg))
    res_sum <- create_results_summary(cvr)
    
    # check if test coverage result exist
    no_cov_check <- is_no_coverage_cov(res_sum)
    
    if (no_cov_check) {
      message("Detected 'no_coverage.R' sentinel. Falling back to covr::package_coverage(type = 'tests').")
      cvr <- tryCatch({
        withr::with_dir(pkg_source_path, covr::package_coverage(path = pkg_source_path, type = "tests", quiet = TRUE))
      }, error = function(e) {
        message("package_coverage() failed: ", conditionMessage(e))
        NULL
      })
      res_sum <- create_results_summary(cvr)
    }
    
    
    covr_list <- list(
      total_cov = res_sum$total_cov,
      res_cov = list(
        name = res_sum$res_cov$name,
        coverage = res_sum$res_cov$coverage,
        errors = res_sum$res_cov$errors,
        notes = res_sum$res_cov$notes
      ),
      long_summary = test_results_long,
      test_skip = test_skip
    )
  } else {
    message(paste0("Creating test results for no skipped tests for ", pkg))
    
    # --- Pass cov_env down (create_covr_list_no_skip signature updated elsewhere) ---
    covr_list <- create_covr_list_no_skip(test_map        = test_map,
                                          test_results_long = test_results,
                                          pkg_source_path = pkg_source_path,
                                          pkg             = pkg,
                                          cov_env         = cov_env)
  }
  
  return(covr_list)
}


#' @title Create Test Reporter
#' @description Executes all tests in a given directory using `testthat::test_dir()`
#' and applies context fixes to ensure each test has a valid context.
#'
#' @param test_path Character string specifying the path to the test directory.
#' @param pkg Character string specifying the package name.
#' @param cov_env environment covr environment.
#'
#' @return A list of test results with corrected contexts, or `NULL` if an error occurs.
#' @keywords internal
create_test_reporter <- function(test_path, pkg, cov_env) {
  
  message(paste0("Creating test reporter for: ", pkg))
  test_results <- tryCatch({
    sink(tempfile())
    res <- testthat::test_dir(
      path = test_path,
      reporter = testthat::default_reporter(),
      env = cov_env,
      stop_on_failure = FALSE,
      stop_on_warning = FALSE,
      package = pkg,
      load_package = "source"
    )
    sink()
    res
  }, error = function(e) {
    message(paste0("Error running tests for ", pkg, " : ", e$message))
    return(NULL)
  })
  test_results
}

#' @title Create Fail Reporter
#' @description Executes all tests in a given directory using `testthat::test_dir()`
#' and applies context fixes to ensure each test has a valid context.
#'
#' @param test_path Character string specifying the path to the test directory.
#' @param pkg Character string specifying the package name.
#' @param cov_env environment convr environment.
#'
#' @return A list of test results with corrected contexts, or `NULL` if an error occurs.
#' @keywords internal
create_fail_reporter <- function(test_path, pkg, cov_env) {
  message("Creating failing tests reporter for: ", pkg)
  
  rep <- testthat::FailReporter$new()
  
  test_results <- tryCatch({
    testthat::test_dir(
      path = test_path,
      reporter = rep,
      env = cov_env,
      stop_on_failure = FALSE,
      stop_on_warning  = FALSE,
      package = pkg,
      load_package = "source"
    )
    # rep$results contains every expectation recorded
    rep$results
  }, error = function(e) {
    message("Error running tests for ", pkg, " : ", e$message)
    NULL
  })
  
  test_results
}


#' @title Fix Missing Test Contexts
#' @description Ensures that each test result has a valid `context` field.
#' If missing or empty, the context is set to the file name (without extension).
#'
#' @param test_results A list of test result objects from `testthat`.
#'
#' @return A list of test results with updated contexts.
#' @keywords internal
fix_test_context <- function(test_results) {
  lapply(test_results, function(x) {
    if (is.null(x$context) || x$context == "") {
      file_name <- tools::file_path_sans_ext(basename(x$file))
      x$context <- file_name
    }
    x
  })
}


#' Run Coverage While Skipping Failing Tests
#'
#' This function runs coverage analysis using `covr` while temporarily
#' commenting out known failing tests. It is intended for situations where
#' specific tests must be skipped (e.g., due to upstream bugs or known
#' failures) so that coverage can still be computed on remaining tests.
#'
#' The function:
#' \itemize{
#'   \item sets up a temporary coverage environment,
#'   \item identifies lines to skip based on `test_skip`,
#'   \item comments out those test lines,
#'   \item runs `covr::environment_coverage()`, and
#'   \item restores the original test files on exit.
#' }
#'
#' @param pkg_source_path Character. Path to the root of the package source.
#' @param pkg Character. Name of the package being tested.
#' @param test_path Character. Path to the test directory.
#'   Defaults to `'tests/testthat'`.
#' @param test_skip Data frame. Mapping of failing tests, typically containing
#'   columns `file`, `line1`, and `line2`. Each row describes a span of lines
#'   that should be commented out before running coverage.
#' @param test_map Data frame. Mapping of all tests within the package (used
#'   for diagnostic messaging and consistency checks).
#' @param cov_env Environment that holds the temporary coverage workspace and
#'   shared state for the run.   
#'
#' @return 
#' A coverage object (a list) returned by `covr::environment_coverage()`,
#' filtered to remove problematic entries. If coverage fails, the function
#' returns `NULL`.
#'
#' The returned structure typically contains:
#' \describe{
#'   \item{filename}{Name of the file being measured.}
#'   \item{functions}{Function-level coverage information.}
#'   \item{linecoverage}{Line-by-line execution counts.}
#'   \item{srcref}{Source references extracted for coverage.}
#' }
#'
#' @details
#' If `test_skip` is `NULL`, failing tests must be detected elsewhere before
#' calling this function. This function does not perform test detection; it
#' only applies the skip mappings supplied in `test_skip`.
#'
#' @seealso
#'   \code{\link[testthat]{test_dir}},
#'   \code{\link[covr]{package_coverage}},
#'   \code{\link[covr]{environment_coverage}}
#' 
#' @keywords internal
#' @family stf_utility
#' @importFrom testthat test_dir
#' @importFrom covr package_coverage environment_coverage
#' @importFrom stats setNames
create_coverage_skip_stf <- function(
    pkg_source_path,
    pkg,
    test_path = "tests/testthat",
    test_skip,
    test_map,
    cov_env
) {
  
  message(paste0("Setting up coverage environment for : ", pkg))
  
  # --- Assertions (checkmate) ---
  checkmate::assert_string(pkg_source_path, .var.name = "pkg_source_path")
  checkmate::assert_string(pkg,            .var.name = "pkg")
  checkmate::assert_directory_exists(test_path, .var.name = "test_path")
  checkmate::assert_true(!missing(cov_env), .var.name = "cov_env")
  checkmate::assert_environment(cov_env,     .var.name = "cov_env")
  
  # --- Assertions: Validate cov_env$work_dir early and explicitly ---
  if (!base::exists("work_dir", envir = cov_env, inherits = FALSE)) {
    message(
      "Missing `cov_env$work_dir`. Please set a working directory in `cov_env` ",
      "that already exists and is writable (ideally a subdirectory of tempdir()). ",
      "Exiting create_coverage_skip_stf() early."
    )
    cleanup_and_return_null(env = cov_env)
    return(invisible(NULL))
  }
  work_dir <- base::get("work_dir", envir = cov_env, inherits = FALSE)
  checkmate::assert_string(work_dir, min.chars = 1, .var.name = "cov_env$work_dir")
  
  # Must exist on disk -> message + cleanup + early return (per request)
  if (!dir.exists(work_dir)) {
    message(
      "`cov_env$work_dir` does not exist on disk: ", work_dir,
      ". Exiting create_coverage_skip_stf() early."
    )
    cleanup_and_return_null(env = cov_env)
    return(invisible(NULL))
  }
  
  # Must be writable
  wd_norm <- normalizePath(work_dir, winslash = "/", mustWork = FALSE)
  if (base::file.access(wd_norm, 2) != 0) {
    stop("`cov_env$work_dir` is not writable: ", wd_norm)
  }
  
  # Prefer (but don't require) location under tempdir(), warn if not
  td_norm <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  starts_with_dir <- function(path, prefix) {
    path <- sub("/+$", "", path)
    prefix <- sub("/+$", "", prefix)
    identical(path, prefix) || startsWith(paste0(path, "/"), paste0(prefix, "/"))
  }
  if (!starts_with_dir(wd_norm, td_norm)) {
    warning(
      "`cov_env$work_dir` (", wd_norm, 
      ") is not located under tempdir() (", td_norm, "). ",
      "For CRAN-safety and to avoid side effects, consider using a subdirectory of tempdir()."
    )
  }
  # --- END assertion block ---
  
  
  # Pull optional metadata from cov_env if available
  helpers_loaded <- if (base::exists("helpers_loaded", envir = cov_env, inherits = FALSE))
    base::get("helpers_loaded", envir = cov_env, inherits = FALSE) else character(0)
  
  initial_cov_env_state <- if (base::exists("initial_state", envir = cov_env, inherits = FALSE))
    base::get("initial_state", envir = cov_env, inherits = FALSE) else
      base::ls(envir = cov_env, all.names = TRUE)
  
  root_dir <- if (base::exists("root_dir", envir = cov_env, inherits = FALSE))
    base::get("root_dir", envir = cov_env, inherits = FALSE) else
      normalizePath(pkg_source_path, winslash = "/", mustWork = FALSE)
  
  # Ensure we clean the env/workspace if the caller didn't (idempotent)
  on.exit({
    message(paste0("Cleaning up environment for ", pkg))
    cleanup_and_return_null(env = cov_env)
  }, add = TRUE)
  
  
  # Report loaded helpers
  if (length(helpers_loaded) > 0) {
    message("Helper files loaded:")
    for (helper in helpers_loaded) {
      message("  - ", helper)
    }
  } else {
    message("No helper files were loaded.")
  }
  
  # Identify lines to skip
  message(paste0("Identifying test lines to skip for : ", pkg))
  test_skip_lines <- lapply(
    split(test_skip, test_skip$file),
    function(x) {
      # keep only rows with valid finite line numbers
      ok <- is.finite(x$line1) & is.finite(x$line2)
      
      if (!any(ok)) {
        return(integer(0))
      }
      
      unlist(
        mapply(
          seq,
          from = x$line1[ok],
          to   = x$line2[ok],
          SIMPLIFY = FALSE
        ),
        use.names = FALSE
      )
    }
  )
  
  # -------------------------------------------------------------------
  # Prepare test files in a TEMPORARY workspace (CRAN-safe).
  # We never modify files under `tests/`; instead, copy to work_dir.
  # -------------------------------------------------------------------
  tmp_tests_dir <- file.path(cov_env$work_dir, "stf_tests")
  dir.create(tmp_tests_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Helper to copy a single test file to temp (preserving name) and return its path
  copy_to_tmp <- function(src_path) {
    tgt_path <- file.path(tmp_tests_dir, basename(src_path))
    # use file.copy to temp; overwrite to be safe on re-runs
    ok <- file.copy(src_path, tgt_path, overwrite = TRUE)
    if (!ok) stop("Failed to copy test file to temp workspace: ", src_path)
    tgt_path
  }
  
  message(paste0("Preparing test files to check for : ", pkg))
  if (length(test_skip_lines) > 0) {
    # Only copy & modify the subset of test files that have skip ranges
    test_files <- file.path(test_path, names(test_skip_lines))
    tmp_files  <- vapply(test_files, copy_to_tmp, character(1), USE.NAMES = FALSE)
    names(tmp_files) <- names(test_skip_lines)
    
    # Apply "comment-out" on the TEMP copies only
    for (nm in names(test_skip_lines)) {
      file_path <- tmp_files[[nm]]
      test_lines <- readLines(file_path, warn = FALSE)
      lines_to_comment <- test_skip_lines[[nm]]
      # Robust bounds check
      lines_to_comment <- lines_to_comment[lines_to_comment >= 1 & lines_to_comment <= length(test_lines)]
      if (length(lines_to_comment)) {
        test_lines[lines_to_comment] <- sprintf("#%s", test_lines[lines_to_comment])
        cat(test_lines, file = file_path, sep = "\n")
      }
    }
    
    # Use the temp copies for coverage
    test_files <- unname(tmp_files)
    
  } else {
    # No specific skip ranges: copy all tests to temp and use those
    all_tests <- list.files(test_path, full.names = TRUE)
    test_files <- vapply(all_tests, copy_to_tmp, character(1))
  }
  
  # Run coverage
  message(paste0("Running environment coverage for ", pkg))
  cvr <- tryCatch({
    withr::with_dir(root_dir, {
      cov <- covr::environment_coverage(env = cov_env, test_files = test_files)
      # clean problematic entries (avoid devtools vapply bug)
      cov <- Filter(function(x) length(c(x$srcref, x$value)) == 9, cov)
      cov
    })
  }, error = function(e) {
    NULL
  })
  
  
  # Clean up newly introduced globals
  message(paste0("Removing new globals from coverage environment for ", pkg))
  remove_new_globals(env = cov_env, initial_state = initial_cov_env_state)
  
  # Final cleanup
  
  # cleanup_and_return_null(env = cov_env)
  
  return(cvr)
}



#' Create a Summary of Coverage Results
#'
#' This function extracts overall test coverage and file‑level coverage
#' information from a covr coverage object. It provides a simplified
#' summary for downstream reporting and scoring.
#'
#' @param cvr A covr coverage object, typically produced by
#'   `covr::environment_coverage()` or by higher‑level wrapper functions in
#'   this package (e.g., skip‑aware coverage for STF or NSTF workflows).
#'
#' @return 
#' A data frame containing summary statistics, including:
#' \describe{
#'   \item{total_cov}{Numeric. Overall percentage of lines covered.}
#'   \item{name}{Character. File or package name associated with the coverage result.}
#'   \item{coverage}{List or data structure summarizing file‑level or
#'         element‑level coverage from the covr object.}
#'   \item{errors}{Character vector containing any coverage‑related errors (if present).}
#'   \item{notes}{Additional diagnostic notes returned by coverage processing.}
#' }
#'
#' The summary object is used by downstream reporting functions to
#' generate test coverage summaries and scoring metrics.
#'
#' @keywords internal
#' @importFrom dplyr %>%
create_results_summary <- function(cvr) {
  
  # check for empty cvr object
  if (is.null(cvr)) {
    # create filecoverage object
    filecoverage <- structure(
      c(NA_real_),
      dim = c(1),
      dimnames = list("no_coverage.R")
    )
    # create covr_list object
    covr_list <- list(
      total_cov = NA,
      res_cov = list(
        name = NA,
        coverage = list(
          filecoverage = filecoverage,
          totalcoverage = NA
        ),
        errors = NA,
        notes = NA
      )
    )
    
  } else {
    # Extract package name
    pkg <- attr(cvr, "package")$package
    # Extract overall coverage percentage
    total_cov <- covr::percent_coverage(cvr)
    
    # convert percentage into a score between 0 and 1
    total_cov <- total_cov / 100
    
    
    # Aggregate coverage by base filename
    file_coverage <- lapply(names(cvr), function(srcref_name) {
      entries <- cvr[[srcref_name]]
      values <- entries$value
      base_file <- sub(":.*", "", srcref_name)  # Extract base filename before first colon
      data.frame(filename = base_file, covered = values > 0)
    })
    
    file_coverage_df <- do.call(rbind, file_coverage)
    
    # Summarize coverage per base file
    file_summary <- file_coverage_df %>%
      dplyr::group_by(filename) %>%
      dplyr::summarise(
        percent = round(100 * sum(covered) / length(covered), 2),
        .groups = "drop"
      )
    
    # Create 1D numeric vector with dimnames (correct structure)
    filecoverage <- setNames(file_summary$percent, file_summary$filename)
    filecoverage <- structure(
      filecoverage,
      dim = c(length(filecoverage)),
      dimnames = list(names(filecoverage))
    )
    
    # Build result structure
    covr_list <- list(
      total_cov = total_cov,
      res_cov = list(
        name = pkg,
        coverage = list(
          filecoverage = filecoverage,
          totalcoverage = total_cov
        ),
        errors = NA,
        notes = NA
      )
    )
  }
  return(covr_list)
}


#' create covr_list when no tests are skipped
#'
#' @param test_map - R object with mapped tests
#' @param test_results_long - cvr object with no tests skipped
#' @param pkg_source_path - path to the package
#' @param pkg - name of the package
#' @param cov_env Environment that holds the temporary coverage workspace and
#'   shared state for the run.   
#'
#' @returns - summary cvr object
#' @keywords internal
#' @importFrom utils data
create_covr_list_no_skip <- function(test_map, 
                                     test_results_long, 
                                     pkg_source_path, 
                                     pkg,
                                     cov_env
                                     ) {
  
  # --- Input checks ---
  checkmate::assert_string(pkg_source_path, .var.name = "pkg_source_path")
  checkmate::assert_string(pkg,            .var.name = "pkg")
  checkmate::assert_true(!missing(cov_env), .var.name = "cov_env")
  checkmate::assert_environment(cov_env,     .var.name = "cov_env")
  
  # Attempt to unload package safely
  safe_unload_package(pkg)
  
  # Create environment for coverage
  cov_env <- new.env(parent = globalenv())
  
  # Load package into cov_env
  load_package_into_env(pkg_source_path, pkg, cov_env)
  
  # Detect datasets used in test files
  available_datasets <- utils::data(package = pkg)$results[, "Item"]
  
  test_files <- unique(file.path(pkg_source_path, "tests", "testthat", test_map$file))
  
  # Validate file paths
  valid_test_files <- Filter(function(f) {
    is.character(f) && length(f) == 1 && file.exists(f)
  }, test_files)
  
  
  used_datasets <- unique(unlist(lapply(valid_test_files, function(file) {
    lines <- readLines(file, warn = FALSE)
    matches <- grep(paste(available_datasets, collapse = "|"), lines, value = TRUE)
    regmatches(matches, gregexpr(paste(available_datasets, collapse = "|"), matches))
  })))
  
  
  # Flatten and clean dataset names
  used_datasets <- unique(unlist(used_datasets))
  used_datasets <- used_datasets[used_datasets %in% available_datasets]
  
  # Load datasets if needed
  if (length(used_datasets) > 0) {
    message(paste0("Loading datasets used in testing: ", paste(used_datasets, collapse = ", ")))
    for (ds in used_datasets) {
      tryCatch({
        utils::data(list = ds, package = pkg, envir = cov_env)
      }, error = function(e) {
        message(paste0("Failed to load dataset ", ds, " : ", e$message))
      })
    }
    
    message(paste0("Running coverage manually for ", pkg))
    
    coverage <- tryCatch({
      result <- covr::environment_coverage(
        env = cov_env,
        test_files = test_files
      )
      rm(list = ls(envir = cov_env), envir = cov_env)
      # clean problematic entries (avoid devtools vapply bug)
      result <- Filter(function(x) length(c(x$srcref, x$value)) == 9, result)
      result
    }, error = function(e) {
      cleanup_and_return_null(paste0("Error running coverage for ", pkg, " : ", e$message), env = cov_env)
    })
    
  } else {
    message(paste0("Creating package coverage for ", pkg))
    # unload package before running package_coverage
    try(unloadNamespace(pkg), silent = TRUE)
    coverage <- covr::package_coverage(path = pkg_source_path, type = "tests")
    rm(list = ls(envir = cov_env), envir = cov_env)
  }
  
  # Enrich each test entry with passed, failed, and skipped counts
  test_results_long <- lapply(test_results_long, function(entry) {
    results <- entry$results
    entry$passed  <- sum(vapply(results, inherits, logical(1), "expectation_success"))
    entry$failed  <- sum(vapply(results, inherits, logical(1), "expectation_failure"))
    entry$skipped <- sum(vapply(results, inherits, logical(1), "expectation_skip"))
    return(entry)
  })
  
  if (!is.null(coverage)) {
    # Extract coverage list
    coverage_list <- covr::coverage_to_list(coverage)
    filecoverage <- coverage_list$filecoverage
    totalcoverage <- coverage_list$totalcoverage
    
    total_passed <- sum(sapply(test_results_long, function(entry) entry$passed))
    total_failed <- sum(sapply(test_results_long, function(entry) entry$failed))
    tests_skipped <- Filter(function(entry) entry$skipped > 0, test_results_long)
    
    res_cov <- list(
      name = pkg,
      coverage = list(
        filecoverage = filecoverage,
        totalcoverage = totalcoverage
      ),
      errors = NA,
      notes = NA,
      passed = total_passed,
      failed = total_failed
    )
    
    total_cov <- if (!is.na(totalcoverage)) totalcoverage / 100 else NA_real_
    
    
    covr_list <- list(
      total_cov = total_cov,
      res_cov = res_cov,
      tests_skipped = tests_skipped
    )
   } else {
      message("No test coverage for this configuration")
      covr_list <- list(
        total_cov = 0,
        res_cov = list(
          name = pkg,
          coverage = list(
            filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
            totalcoverage = 0
          ),
          errors = "No testthat or testit configuration",
          notes = NA
        )
      )
    } 
    
  cleanup_and_return_null(env = cov_env)
  
  return(covr_list)
}

#' Determine the appropriate standard test path for a package
#'
#' This internal utility function inspects the `test_pkg_data` structure to identify
#' which testing framework is used in the package and returns the corresponding test path.
#'
#' @param test_pkg_data A named list containing logical flags such as `has_testthat`, `has_testit`, `has_tests_base`, etc.
#' @param testdir A character string specifying the root directory where test folders are located.
#'
#' @return A character string representing the path to the test directory, or `NULL` if no known test framework is found.
#'
#' @keywords internal
get_stf_test_path <- function(test_pkg_data, testdir) {
  if (isTRUE(test_pkg_data$has_testthat)) {
    return(file.path(testdir, "testthat"))
  } else if (isTRUE(test_pkg_data$has_testit)) {
    return(file.path(testdir, "testit"))
  } else if (
    dir.exists(file.path(testdir, "test-ci")) &&
    dir.exists(file.path(testdir, "test-cran"))
  ) {
    return(testdir)  # fallback for nonstandard testit
  } else if (isTRUE(test_pkg_data$has_tests_base)) {
    return(testdir)  # base R scripts
  }
  
  return(NULL)
}


#' Detect covr's "no_coverage" sentinel
#'
#' @description
#' `is_no_coverage_cov()` returns `TRUE` when a covr coverage object contains
#' the special **no coverage** sentinel—i.e., a 1×1 `NA` `filecoverage` whose
#' sole filename (row name) is `"no_coverage.R"`. This commonly indicates that
#' instrumentation ran but no executable lines were attributed to the target, or
#' that the target test files did not exercise any package code.
#'
#' @details
#' The function is defensive across covr versions/shapes:
#' - It looks for `cov$coverage$filecoverage`, returning `FALSE` if missing.
#' - It accepts file names from either `dimnames(filecoverage)[[1]]` or
#'   `names(filecoverage)`.
#' - It positively identifies the sentinel *only* when **both** conditions hold:
#'   (1) the coverage array is length 1 and `NA`, and (2) the associated filename
#'   is `"no_coverage.R"`.
#'
#' @param cov A coverage object returned by `covr::environment_coverage()`,
#'   `covr::package_coverage()`, or a list-like object that exposes
#'   `$coverage$filecoverage`.
#'
#' @return A single `logical`: `TRUE` if the object represents “no coverage”,
#'   otherwise `FALSE`.
#' 
#' @keywords internal
#' @noRd
is_no_coverage_cov <- function(cov) {
  # Try to retrieve the filecoverage array safely
  
  fc <- tryCatch(cov$res_cov$coverage$filecoverage, error = function(e) NULL)
  if (!is.null(fc)) {
    # Derive file labels from dimnames (prefer) or names (fallback)
    dn <- dimnames(fc)
    files <- if (!is.null(dn) && length(dn) >= 1L && !is.null(dn[[1L]])) {
      dn[[1L]]
    } else {
      names(fc)
    }
  
    # Check for the precise sentinel shape and label
    is_len1_na <- (length(fc) == 1L) && is.na(fc[[1L]])
    has_no_cov_name <- isTRUE(length(files) >= 1L) && any(files %in% "no_coverage.R")
    
    if (!is.null(fc) && is_len1_na && has_no_cov_name) {
      no_cov_check <- TRUE
    } else {
      no_cov_check <- FALSE
    }  
  } else {
    no_cov_check <- FALSE
  }
  return(no_cov_check)
}
