#' Check for tests/testthat and _snaps folder and count golden tests
#'
#' @param pkg_source_path Path to the root of the package source
#'
#' @return A list with:
#'   - `has_testthat`: Does tests/testthat exist?
#'   - `has_testit`: Does tests/testit exist?
#'   - `has_tests_base`: Do tests exist in tests subdirectory?
#'   - `has_BioG_test`: Do BioGenerics tests exist in inst/tests subdirectory?
#'   - `bioc_unit_tests_dir` : location of BioGenerics unit tests
#'   - `bioc_run_ut_path`: location of BioGenerics `run_unitTests.R`
#'   - `has_snaps`: Does _snaps exist inside tests/testthat?
#'   - `n_golden_tests`: Number of snapshot files inside _snaps
#'   - `n_test_files`: Number of test-*.R files inside tests/testthat
#' @keywords internal
check_pkg_tests_and_snaps <- function(pkg_source_path) {
  
  message("checking package test config")
  test_dir <- file.path(pkg_source_path, "tests")
  testthat_path <- file.path(test_dir, "testthat")
  snaps_path <- file.path(testthat_path, "_snaps")
  
  testit_path <- file.path(test_dir, "testit")
  test_ci_path <- file.path(test_dir, "test-ci")
  test_cran_path <- file.path(test_dir, "test-cran")
  
  # Check for testthat and testit (standard and nonstandard)
  has_testthat <- dir.exists(testthat_path)
  has_testit <- dir.exists(testit_path) || (dir.exists(test_ci_path) && dir.exists(test_cran_path))
  
  # Count golden test snapshot files
  has_snaps <- dir.exists(snaps_path)
  n_golden_tests <- if (has_snaps) {
    snapshot_files <- list.files(snaps_path, recursive = TRUE, full.names = TRUE)
    length(snapshot_files)
  } else {
    0
  }
  
  # Count test-*.R files in testthat
  n_test_files <- if (has_testthat) {
    test_files <- list.files(
      testthat_path,
      pattern = "^test-.*\\.R$",
      recursive = TRUE,
      full.names = TRUE
    )
    test_files <- test_files[!grepl("_snaps", test_files)]
    length(test_files)
  } else {
    0
  }
 
  # Only check for base R test scripts if none of the known test frameworks are present
  has_tests_base <- FALSE
  if (!has_testthat && !has_testit && !dir.exists(test_ci_path) && !dir.exists(test_cran_path)) {
    base_test_files <- list.files(
      test_dir,
      pattern = "\\.R$",
      full.names = TRUE
    )
    # Exclude files in known subdirectories
    base_test_files <- base_test_files[dirname(base_test_files) == test_dir]
    # EXCLUDE tests/run_unitTests.R (launcher for Bioconductor RUnit tests)
    base_test_files <- base_test_files[basename(base_test_files) != "run_unitTests.R"]
    has_tests_base <- length(base_test_files) > 0
  }
  
  # BiocGenerics 3-part check ----
  # 1) tests under inst/unitTests
  bioc_unit_tests_dir <- file.path(pkg_source_path, "inst", "unitTests")
  has_bioc_unit_tests <- dir.exists(bioc_unit_tests_dir)
  bioc_unit_tests_dir <- if (has_bioc_unit_tests) bioc_unit_tests_dir else NA_character_
  
  # 2) presence of tests/run_unitTests.R (as in BiocGenerics/tests/run_unitTests.R)
  bioc_run_ut_path <- file.path(pkg_source_path, "tests", "run_unitTests.R")
  has_bioc_run_unitTests <- file.exists(bioc_run_ut_path)
  bioc_run_ut_path <- if (has_bioc_run_unitTests) bioc_run_ut_path else NA_character_
  
  # 3) presence of .test in R/zzz.R (Bioconductor RUnit hook)
  zzz_path <- file.path(pkg_source_path, "R", "zzz.R")
  has_bioc_dot_test <- FALSE
  if (file.exists(zzz_path)) {
    zzz_lines <- tryCatch(readLines(zzz_path, warn = FALSE), error = function(e) character())
    # Look for a function assignment like `.test <- function(...)`
    has_bioc_dot_test <- any(grepl("\\.test\\s*<-\\s*function\\b", zzz_lines))
  }
  
  # Overall Bioconductor test flag: TRUE only if all three are present
  has_BioG_test <- has_bioc_unit_tests && has_bioc_run_unitTests && has_bioc_dot_test
  
  
  return(list(
    has_testthat = has_testthat,
    has_snaps = has_snaps,
    has_testit = has_testit,
    has_tests_base = has_tests_base,
    has_BioG_test =  has_BioG_test,
    bioc_unit_tests_dir = bioc_unit_tests_dir,
    bioc_run_ut_path = bioc_run_ut_path,
    n_golden_tests = n_golden_tests,
    n_test_files = n_test_files
  ))
}


#' Compute approximate total coverage combining line coverage and test breadth
#'
#' @description
#' Computes an approximate "total coverage" metric by combining:
#' 1) line coverage percentage (`percent_cov`, 0–100), and
#' 2) a conservative proxy for breadth of tests based on counts of passing
#'    test files, functions without tests, and skipped test files.
#'
#' The final metric scales line coverage (as a proportion) by the fraction of
#' tested over total function-space, where:
#' - `n_testfiles` = number of passing test files (if 0, treated as 1 to avoid divide-by-zero),
#' - `n_no_function_tests` = number of functions lacking tests (`nrow(functions_no_tests_df)`),
#' - `n_skipped_files` = number of skipped test files (`length(tests_skipped)`),
#' - `total_functions` = `n_tested + n_untested` with `n_tested = max(n_testfiles, 1)`,
#' - `total_cov` = (percent_cov / 100) * (n_tested / total_functions).
#'
#' @details
#' This function is intentionally conservative:
#' - Passing test *files* are used as a proxy for tested functions.
#' - Skipped test files contribute to "untested breadth".
#' - If there are zero passing test files, `n_tested` is set to 1 to avoid
#'   divide-by-zero and produce a small non-zero denominator.
#'
#' @param percent_cov Numeric scalar. Line coverage percentage (0–100).
#' @param functions_no_tests_df A `data.frame` (or `NULL`) of functions lacking tests.
#' @param tests_passing A character vector (or `NULL`) of passing test file paths/names.
#' @param tests_skipped A character vector (or `NULL`) of skipped test file paths/names.
#'
#' @return A named list with:
#' \itemize{
#'   \item \code{total_cov} (numeric): Approximate total coverage (proportion 0–1).
#'   \item \code{percent_cov_round} (numeric): \code{total_cov} rounded to 2 decimals (still 0–1).
#'   \item \code{n_testfiles} (integer): Count of passing test files.
#'   \item \code{n_no_function_tests} (integer): Count of functions lacking tests.
#'   \item \code{n_skipped_files} (integer): Count of skipped test files.
#' }
#'
#' @keywords internal
compute_total_coverage <- function(
    percent_cov,
    functions_no_tests_df,
    tests_passing,
    tests_skipped
) {
  
  # Clamp to [0, 100] to avoid pathological inputs
  percent_cov <- max(0, min(100, percent_cov))
  
  # --- Counts (approximate breadth) ---
  n_no_function_tests <- if (!is.null(functions_no_tests_df)) nrow(functions_no_tests_df) else 0L
  n_skipped_files     <- if (!is.null(tests_skipped)) length(tests_skipped) else 0L
  
  # Treat skipped test files as contributing to "untested breadth"
  n_untested <- n_no_function_tests + n_skipped_files
  
  # Proxy for "tested functions": number of passing test files (conservative)
  n_testfiles <- if (!is.null(tests_passing)) length(tests_passing) else 0L
  
  # Avoid divide-by-zero if coverage exists but counts are zero
  n_tested <- if (n_testfiles > 0L) n_testfiles else 1L
  
  # Approximate total function-space
  total_functions <- n_tested + n_untested
  
  # Rounded proportion (still 0..1)
  percent_cov_round <- round(percent_cov, 2)
  
  # Return the requested elements
  calc_cov_list <- list(
    total_cov = percent_cov,
    percent_cov_round = percent_cov_round,
    n_testfiles = n_testfiles,
    n_no_function_tests = n_no_function_tests,
    n_skipped_files = n_skipped_files
  )
  
  return(calc_cov_list)
}  






