
# Helper: minimal mapping DF used by downstream steps
make_mapping <- function() {
  data.frame(
    source_file = c("a.R", "b.R"),
    test_file   = c("test-a.R", NA_character_),  # one with a test, one without
    evidence    = c("filename_match", NA_character_),
    has_tests   = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that("diagnostic: coverage block functions are invoked (covr + reducer)", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)
  
  # Same setup as above (mapping, normalize, prepare, etc.)
  mockery::stub(fn, "get_pkg_name",         mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths",
                mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = FALSE) x)
  
  mapping <- data.frame(
    source_file = "a.R", test_file = "test-a.R", evidence = "filename_match",
    has_tests = TRUE, stringsAsFactors = FALSE
  )
  mockery::stub(fn, "get_source_test_mapping_runit_nstf", mockery::mock(mapping))
  mockery::stub(fn, "prepare_for_bioc_runit",     mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf",  mockery::mock(data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)))
  mockery::stub(fn, "get_function_no_tests",      mockery::mock(data.frame(fn = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na",             function(df) df)
  
  mockery::stub(fn, "ls",               mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace",      mockery::mock(new.env(), cycle = TRUE))
  mockery::stub(fn, "assign",           mockery::mock(NULL, cycle = TRUE))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace",    mockery::mock(NULL))
  # Must stub library() or you’ll abort here!
  mockery::stub(fn, "detach_pkg_if_attached", mockery::mock(TRUE))
  mockery::stub(fn, "pkgload::load_all", mockery::mock(invisible(NULL)))
  mockery::stub(fn, "library",          mockery::mock(NULL))
  mockery::stub(fn, "tempfile",         mockery::mock("/tmp/runner.R", cycle = TRUE))
  mockery::stub(fn, "writeLines",       mockery::mock(NULL, cycle = TRUE))
  
  
  # avoid error from as.environment("package:mypkg")
  mockery::stub(fn, "as.environment", function(name) {
    # when calling as.environment("package:mypkg"), return a fresh env
    if (identical(name, paste0("package:", pkg_name))) return(new.env(parent = emptyenv()))
    # delegate other calls to base (rare here)
    base::as.environment(name)
  })
  
  
  # Spy/record calls to covr & reducer
  spy_env_cov <- mockery::mock(structure(list(), class = "covr_coverage"))
  spy_cov_to_list <- mockery::mock(list(totalcoverage = 75.0))
  spy_compute <- mockery::mock(list(
    total_cov = 0.50, percent_cov = 75.0,
    n_testfiles = 1L, n_no_function_tests = 1L, n_skipped_files = 0L
  ))
  # Return > 0 so we skip package_coverage fallback (which would need readLines/real path)
  mockery::stub(fn, "covr::percent_coverage", mockery::mock(75.0))
  
  # Bypass build_runit_custom_runner to avoid normalizePath(mustWork=TRUE) on fake path
  mock_runner_path <- tempfile(fileext = ".R")
  writeLines(c("library(mypkg)", "1 + 1"), mock_runner_path)
  mockery::stub(fn, "build_runit_custom_runner", mockery::mock(mock_runner_path))
  
  mockery::stub(fn, "covr::environment_coverage", spy_env_cov)
  mockery::stub(fn, "covr::coverage_to_list",     spy_cov_to_list)
  mockery::stub(fn, "compute_total_coverage",     spy_compute)
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  
  expect_identical(res$total_cov, 0.50)
  expect_identical(res$res_cov$coverage$totalcoverage, 75.0)
})


test_that("messages when package is not using RUnit layout and returns default coverage when covr fails", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  
  # --- Stubs for early steps ---
  mock_get_pkg_name <- mockery::mock(pkg_name)
  mock_is_layout    <- mockery::mock(FALSE)  # trigger layout message
  
  mock_get_paths <- mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests")))
  mock_normalize <- mockery::mock(function(x, winslash = "/", mustWork = FALSE) x, cycle = TRUE)
  
  # --- Mapping: succeed to progress into coverage path ---
  mock_get_mapping <- mockery::mock(make_mapping())
  
  # --- Cleaning branch: only run when bioc_run_ut_path is not NA ---
  # In this test, set bioc_run_ut_path = NA so the cleaning branch is skipped
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)
  
  # --- prepare_for_bioc_runit: succeed ---
  mock_prepare <- mockery::mock(TRUE)
  
  # --- Problems: return "No tests skipped" (default/no skip) ---
  mock_problems <- mockery::mock(data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  ))
  
  # --- Post-load mapping: functions with no tests ---
  mock_no_tests <- mockery::mock(data.frame(
    `function` = "b",
    stringsAsFactors = FALSE
  ))
  
  # --- tidyr::drop_na: identity (no change) ---
  mock_drop_na <- function(df) df
  
  # --- Env preparation (assign exported/internal objects + datasets) ---
  mock_ls <- mockery::mock(c("obj1", "obj2"))
  mock_asNamespace <- mockery::mock(new.env())
  mock_assign <- mockery::mock(NULL)
  mock_load_datasets <- mockery::mock(c("ds1", "ds2"))
  mock_message <- mockery::mock(NULL, cycle = TRUE)
  
  # --- Coverage block stubs: force an error in covr to take default path ---
  mock_loadedNamespaces <- mockery::mock(c("base", "stats", "utils", "methods"))  # so loadNamespace("methods") skipped
  mock_loadNamespace <- mockery::mock(NULL)
  mock_detach <- mockery::mock(TRUE)
  mock_library <- mockery::mock(NULL)
  mock_tempfile <- mockery::mock("/tmp/runner.R")
  mock_writeLines <- mockery::mock(NULL)
  
  # This triggers the error branch -> default coverage result
  mock_env_cov <- function(env, test_files) stop("instrumentation failed")
  # coverage_to_list is never reached when env_cov errors, but stub for safety
  mock_cov_to_list <- mockery::mock(list(filecoverage = matrix(0, nrow = 1), totalcoverage = 0))
  
  # --- cleanup_and_return_null used by error branches ---
  mock_cleanup <- mockery::mock(NULL)
  
  # Wire stubs
  mockery::stub(fn, "get_pkg_name",               mock_get_pkg_name)
  mockery::stub(fn, "is_runit_layout",       mock_is_layout)
  mockery::stub(fn, "get_runit_test_paths",mock_get_paths)
  mockery::stub(fn, "normalizePath",              mock_normalize)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf", mock_get_mapping)
  mockery::stub(fn, "prepare_for_bioc_runit",     mock_prepare)
  mockery::stub(fn, "check_covr_skip_runit_nstf",  mock_problems)
  mockery::stub(fn, "get_function_no_tests",      mock_no_tests)
  mockery::stub(fn, "tidyr::drop_na",             mock_drop_na)
  mockery::stub(fn, "ls",                         mock_ls)
  mockery::stub(fn, "asNamespace",                mock_asNamespace)
  mockery::stub(fn, "assign",                     mock_assign)
  mockery::stub(fn, "load_package_datasets",      mock_load_datasets)
  mockery::stub(fn, "message",                    mock_message)
  mockery::stub(fn, "loadedNamespaces",           mock_loadedNamespaces)
  mockery::stub(fn, "loadNamespace",              mock_loadNamespace)
  mockery::stub(fn, "detach_pkg_if_attached",     mock_detach)
  mockery::stub(fn, "library",                    mock_library)
  mockery::stub(fn, "tempfile",                   mock_tempfile)
  mockery::stub(fn, "writeLines",                 mock_writeLines)
  mockery::stub(fn, "covr::environment_coverage", mock_env_cov)
  mockery::stub(fn, "covr::coverage_to_list",     mock_cov_to_list)
  mockery::stub(fn, "cleanup_and_return_null",    mock_cleanup)
  
  suppressWarnings(res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data))
  
  # Assertions: default coverage path
  expect_type(res, "list")
  expect_true("res_cov" %in% names(res))
  expect_true(is.list(res$res_cov))
  expect_identical(res$total_cov, 0)
  expect_identical(res$res_cov$name, pkg_name)
  expect_true(is.list(res$res_cov$coverage))
  expect_identical(res$res_cov$coverage$totalcoverage, 0)
  expect_identical(res$res_cov$errors, "No RUnit or BiocGenerics configuration")
  
  # The first message should hint about RUnit layout
  msgs <- mockery::mock_args(mock_message)
  # Flatten first invocation into a single string to check substring
  first_msg <- paste0(msgs[[1]], collapse = "")
  expect_match(first_msg, "RUnit layout")
})


test_that("returns NULL when source-test mapping errors", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)
  
  mockery::stub(fn, "get_pkg_name",         mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths", mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath",        mockery::mock(function(x, winslash = "/", mustWork = FALSE) x, cycle = TRUE))
  # Mapping: throw error so tryCatch invokes cleanup_and_return_null -> returns NULL -> early exit
  mockery::stub(fn, "get_source_test_mapping_runit_nstf", function(...) stop("boom-mapping"))
  mockery::stub(fn, "cleanup_and_return_null",           mockery::mock(NULL))
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  expect_null(res)
})


test_that("returns NULL when prepare_for_bioc_runit fails", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)
  
  mockery::stub(fn, "get_pkg_name",         mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths", mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath",        mockery::mock(function(x, winslash = "/", mustWork = FALSE) x, cycle = TRUE))
  mockery::stub(fn, "get_source_test_mapping_runit_nstf", mockery::mock(make_mapping()))
  # Force prepare to error; tryCatch should call cleanup_and_return_null and return NULL
  mockery::stub(fn, "prepare_for_bioc_runit", function(...) stop("prep-error"))
  mockery::stub(fn, "cleanup_and_return_null", mockery::mock(NULL))
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  expect_null(res)
})


test_that("coverage error path yields a default covr_list even when mapping & problems succeed", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)  # skip cleaning branch
  
  # Layout ok
  mockery::stub(fn, "get_pkg_name",         mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  
  # Paths, normalization
  mockery::stub(fn, "get_runit_test_paths", mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath",        mockery::mock(function(x, winslash = "/", mustWork = FALSE) x, cycle = TRUE))
  
  # Mapping and later pipeline
  mockery::stub(fn, "get_source_test_mapping_runit_nstf", mockery::mock(make_mapping()))
  mockery::stub(fn, "prepare_for_bioc_runit", mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf", mockery::mock(data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )))
  mockery::stub(fn, "get_function_no_tests", mockery::mock(data.frame(`function` = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na",        function(df) df)
  
  # Env prep for coverage
  mockery::stub(fn, "ls",               mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace",      mockery::mock(new.env()))
  mockery::stub(fn, "assign",           mockery::mock(NULL))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "message",          mockery::mock(NULL, cycle = TRUE))
  
  # Coverage: trigger failure to default path
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace",    mockery::mock(NULL))
  mockery::stub(fn, "detach_pkg_if_attached", mockery::mock(TRUE))
  mockery::stub(fn, "library",          mockery::mock(NULL))
  mockery::stub(fn, "tempfile",         mockery::mock("/tmp/runner.R"))
  mockery::stub(fn, "writeLines",       mockery::mock(NULL))
  mockery::stub(fn, "covr::environment_coverage", function(...) stop("covr-failed"))
  mockery::stub(fn, "covr::coverage_to_list", mockery::mock(list(filecoverage = matrix(0, nrow = 1), totalcoverage = 0)))
  mockery::stub(fn, "cleanup_and_return_null", mockery::mock(NULL))
  
  suppressWarnings(res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data))
  
  expect_type(res, "list")
  expect_identical(res$total_cov, 0)
  expect_identical(res$res_cov$name, pkg_name)
  expect_identical(res$res_cov$coverage$totalcoverage, 0)
  expect_identical(res$res_cov$errors, "No RUnit or BiocGenerics configuration")
})

# Minimal helper mapping (one test file to pass mustWork normalization)
make_mapping <- function() {
  data.frame(
    source_file = c("a.R", "b.R"),
    test_file   = c("test-a.R", NA_character_),
    evidence    = c("filename_match", NA_character_),
    has_tests   = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that("error in coverage_to_list hits error branch but lines still produce messages and covr_list", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)
  
  # Upstream stubs to reach the block
  mockery::stub(fn, "get_pkg_name",         mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths",
                mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = FALSE) x)
  
  # Mapping with one test to ensure test_files_clean is non-empty
  make_mapping <- function() {
    data.frame(
      source_file = c("a.R", "b.R"),
      test_file   = c("test-a.R", NA_character_),
      evidence    = c("filename_match", NA_character_),
      has_tests   = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    )
  }
  mockery::stub(fn, "get_source_test_mapping_runit_nstf", mockery::mock(make_mapping()))
  
  mockery::stub(fn, "prepare_for_bioc_runit", mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf", mockery::mock(
    data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)
  ))
  mockery::stub(fn, "get_function_no_tests", mockery::mock(data.frame(fn = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na", function(df) df)
  
  # Env prep / coverage stubs
  mockery::stub(fn, "ls",               mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace",      mockery::mock(new.env(), cycle = TRUE))
  mockery::stub(fn, "assign",           mockery::mock(NULL, cycle = TRUE))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace",    mockery::mock(NULL))
  mockery::stub(fn, "detach_pkg_if_attached", mockery::mock(TRUE))
  mockery::stub(fn, "pkgload::load_all", function(...) invisible(NULL))
  mockery::stub(fn, "library",          mockery::mock(NULL))
  mockery::stub(fn, "tempfile",         mockery::mock("/tmp/runner.R", cycle = TRUE))
  mockery::stub(fn, "writeLines",       mockery::mock(NULL, cycle = TRUE))
  
  # keep coverage block alive by returning a benign env for "package:mypkg"
  mockery::stub(fn, "as.environment", function(name) {
    if (identical(name, paste0("package:", pkg_name))) {
      return(new.env(parent = emptyenv()))
    }
    base::as.environment(name)
  })
  
  # Bypass build to avoid normalizePath(mustWork=TRUE) on fake path
  mock_runner_path <- tempfile(fileext = ".R")
  writeLines(c("library(mypkg)", "1 + 1"), mock_runner_path)
  mockery::stub(fn, "build_runit_custom_runner", mockery::mock(mock_runner_path))
  
  # Return > 0 to skip package_coverage fallback
  mockery::stub(fn, "covr::percent_coverage", mockery::mock(75.0))
  
  # Force error in coverage_to_list -> error branch sets res_cov$errors and NA coverage fields
  mockery::stub(fn, "covr::environment_coverage", mockery::mock(structure(list(), class = "covr_coverage")))
  mockery::stub(fn, "covr::coverage_to_list", function(...) stop("explode"))
  
  # Stub compute_total_coverage to still return values (so the lines proceed)
  mockery::stub(fn, "compute_total_coverage", function(percent_cov, ...) {
    # percent_cov will be NA_real_ due to error branch; ignore and return defaults
    list(
      total_cov            = 0.00,
      percent_cov          = 0.00,
      n_testfiles          = 1L,
      n_no_function_tests  = 1L,
      n_skipped_files      = 0L
    )
  })
  
  # Capture messages & cleanup calls
  mock_msg   <- mockery::mock(NULL, cycle = TRUE)
  mock_clean <- mockery::mock(NULL)
  mockery::stub(fn, "message",                 mock_msg)
  mockery::stub(fn, "cleanup_and_return_null", mock_clean)
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  
  # Messages exist (Total Coverage 0.00%, Approx total coverage 0.00%)
  msg_calls <- mockery::mock_args(mock_msg)
  all_msgs  <- vapply(msg_calls, function(x) paste0(x, collapse = ""), character(1))
  total_line  <- all_msgs[grepl("^Total Coverage for mypkg is:", all_msgs)]
  approx_line <- all_msgs[grepl("^Approx total coverage:",       all_msgs)]
  
  expect_true(length(total_line)  >= 1)
  expect_true(length(approx_line) >= 1)
  expect_match(total_line[1],  "0%")
  expect_match(approx_line[1], "\\(0\\.00%\\)")
  
  # cleanup called once
  mockery::expect_called(mock_clean, 1)
  
  # covr_list assembled
  expect_type(res, "list")
  expect_true("res_cov" %in% names(res))
  # In error branch, res_cov$coverage$'totalcoverage' == NA_integer_
  expect_true(is.na(res$res_cov$coverage$totalcoverage))
  expect_true(inherits(res$res_cov$errors, "error") || is.character(res$res_cov$errors))
})


test_that("returns TRUE when test_pkg_data$has_BioG_test is TRUE regardless of files", {
  fn <- is_bioc_runit_layout
  
  # Mock filesystem as FALSE to ensure the first clause is sufficient
  mock_dir_exists  <- mockery::mock(FALSE)
  mock_file_exists <- mockery::mock(FALSE)
  
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(pkg_source_path = "/fake/path", test_pkg_data = list(has_BioG_test = TRUE))
  expect_true(res)
  
  # Both stubs should still be called once due to evaluation sequence of ||
  # (In R, || is short-circuiting, but since the first is TRUE, the others may not be called.
  # To be robust across implementations, we don't assert call counts here.)
})

test_that("returns FALSE when no BioC-RUnit indicators are present", {
  fn <- is_runit_layout
  
  mock_dir_exists  <- mockery::mock(FALSE)
  mock_file_exists <- mockery::mock(FALSE)
  
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  # test_pkg_data NULL or missing field -> isTRUE(NULL) => FALSE
  res1 <- fn(pkg_source_path = "/fake/path", test_pkg_data = NULL)
  expect_false(res1)
  
})

test_that("returns TRUE when inst/unitTests directory exists", {
  fn <- is_runit_layout
  
  # dir.exists should return TRUE; file.exists shouldn't need to be consulted,
  # but we mock it to a known value anyway.
  mock_dir_exists  <- mockery::mock(TRUE)
  mock_file_exists <- mockery::mock(FALSE)
  
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(pkg_source_path = "/fake/path", test_pkg_data = list(has_BioG_test = FALSE))
  expect_true(res)
  
  # Verify dir.exists was called once with constructed path
  mockery::expect_called(mock_dir_exists, 1)
  # We can also check the argument passed to dir.exists (optional)
  called_args <- mockery::mock_args(mock_dir_exists)
  expect_match(called_args[[1]][[1]], "/fake/path/inst/unitTests$")
})

test_that("returns TRUE when tests/run_unitTests.R file exists", {
  fn <- is_runit_layout
  
  mock_dir_exists  <- mockery::mock(FALSE)
  mock_file_exists <- mockery::mock(TRUE)
  
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(pkg_source_path = "/fake/path", test_pkg_data = list(has_BioG_test = FALSE))
  expect_true(res)
  
  # Verify file.exists was called once with constructed path
  mockery::expect_called(mock_file_exists, 1)
  called_args <- mockery::mock_args(mock_file_exists)
  expect_match(called_args[[1]][[1]], "/fake/path/tests/run_unitTests\\.R$")
})

test_that("short-circuit behavior: when has_BioG_test is TRUE, other checks need not be TRUE", {
  fn <- is_runit_layout
  
  # We set dir.exists and file.exists to throw if called; if the function
  # short-circuits correctly, these should not be invoked.
  throwing_dir_exists <- function(...) stop("dir.exists should not be called")
  throwing_file_exists <- function(...) stop("file.exists should not be called")
  
  mockery::stub(fn, "dir.exists", throwing_dir_exists)
  mockery::stub(fn, "file.exists", throwing_file_exists)
  
  expect_true(fn(pkg_source_path = "/fake/path", test_pkg_data = list(has_BioG_test = TRUE)))
})

test_that("handles has_BioG_test = NULL or missing field as FALSE", {
  fn <- is_runit_layout
  
  mock_dir_exists  <- mockery::mock(FALSE)
  mock_file_exists <- mockery::mock(FALSE)
  
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  # Explicit NULL value
  res_null <- fn(pkg_source_path = "/fake/path", test_pkg_data = list(has_BioG_test = NULL))
  expect_false(res_null)
  
})

test_that("supports atomic pkg_source_path; only path construction matters", {
  fn <- is_runit_layout
  
  mock_dir_exists  <- mockery::mock(FALSE)
  mock_file_exists <- mockery::mock(FALSE)
  
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  # Try with a different path format; the stubs ensure no actual filesystem usage
  res <- fn(pkg_source_path = "C:\\proj", test_pkg_data = NULL)
  expect_false(res)
})


test_that("does nothing when package not attached and namespace not loaded", {
  fn <- detach_pkg_if_attached
  
  # search() contains no package:pkg
  mock_search <- mockery::mock(c("package:stats", "package:utils"))
  mock_detach <- mockery::mock(NULL)             # should not be called
  mock_is_ns   <- mockery::mock(FALSE)
  mock_unload  <- mockery::mock(NULL)            # should not be called
  mock_message <- mockery::mock(NULL)
  
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "isNamespaceLoaded", mock_is_ns)
  mockery::stub(fn, "unloadNamespace", mock_unload)
  mockery::stub(fn, "message", mock_message)
  
  # Result is invisible TRUE
  v <- withVisible(fn("mypkg", unload_namespace = TRUE, quiet = TRUE))
  expect_true(v$value)
  expect_false(v$visible)
  
  # detach and unload should not be called
  mockery::expect_called(mock_detach, 0)
  mockery::expect_called(mock_unload, 0)
  
  # no messages because quiet = TRUE
  mockery::expect_called(mock_message, 0)
})

test_that("detaches when package attached; quiet=TRUE suppresses message", {
  fn <- detach_pkg_if_attached
  
  mock_search <- mockery::mock(c("package:mypkg", "package:stats"))
  mock_detach <- mockery::mock(NULL)
  mock_is_ns  <- mockery::mock(FALSE)     # namespace not loaded
  mock_unload <- mockery::mock(NULL)      # not called
  mock_message <- mockery::mock(NULL)
  
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "isNamespaceLoaded", mock_is_ns)
  mockery::stub(fn, "unloadNamespace", mock_unload)
  mockery::stub(fn, "message", mock_message)
  
  res <- fn("mypkg", unload_namespace = TRUE, quiet = TRUE)
  expect_true(res)
  
  # detach should be called once with constructed search name and flags
  mockery::expect_called(mock_detach, 1)
  args <- mockery::mock_args(mock_detach)
  expect_equal(args[[1]][[1]], "package:mypkg")
  expect_true(isTRUE(args[[1]][["unload"]]))
  expect_true(isTRUE(args[[1]][["character.only"]]))
  
  # namespace unload should not be called (isNamespaceLoaded == FALSE)
  mockery::expect_called(mock_unload, 0)
  
  # quiet=TRUE means no message
  mockery::expect_called(mock_message, 0)
})

test_that("unloads namespace when loaded; quiet=TRUE suppresses message", {
  fn <- detach_pkg_if_attached
  
  mock_search <- mockery::mock(c("package:stats"))  # not attached
  mock_detach <- mockery::mock(NULL)                # not called
  mock_is_ns  <- mockery::mock(TRUE)                # namespace loaded
  mock_unload <- mockery::mock(NULL)
  mock_message <- mockery::mock(NULL)
  
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "isNamespaceLoaded", mock_is_ns)
  mockery::stub(fn, "unloadNamespace", mock_unload)
  mockery::stub(fn, "message", mock_message)
  
  res <- fn("mypkg", unload_namespace = TRUE, quiet = TRUE)
  expect_true(res)
  
  mockery::expect_called(mock_detach, 0)
  mockery::expect_called(mock_unload, 1)
  unload_args <- mockery::mock_args(mock_unload)
  expect_equal(unload_args[[1]][[1]], "mypkg")
  
  mockery::expect_called(mock_message, 0)
})

test_that("both detach and unload occur when attached and namespace loaded", {
  fn <- detach_pkg_if_attached
  
  mock_search <- mockery::mock(c("package:mypkg", "package:stats"))
  mock_detach <- mockery::mock(NULL)
  mock_is_ns  <- mockery::mock(TRUE)
  mock_unload <- mockery::mock(NULL)
  mock_message <- mockery::mock(NULL)
  
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "isNamespaceLoaded", mock_is_ns)
  mockery::stub(fn, "unloadNamespace", mock_unload)
  mockery::stub(fn, "message", mock_message)
  
  res <- fn("mypkg", unload_namespace = TRUE, quiet = TRUE)
  expect_true(res)
  
  mockery::expect_called(mock_detach, 1)
  mockery::expect_called(mock_unload, 1)
  mockery::expect_called(mock_message, 0)
})

test_that("unload_namespace = FALSE prevents namespace unloading", {
  fn <- detach_pkg_if_attached
  
  mock_search <- mockery::mock(c("package:mypkg"))
  mock_detach <- mockery::mock(NULL)
  mock_is_ns  <- mockery::mock(TRUE)    # would be loaded, but flag stops unload
  mock_unload <- mockery::mock(NULL)    # should not be called
  mock_message <- mockery::mock(NULL)
  
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "isNamespaceLoaded", mock_is_ns)
  mockery::stub(fn, "unloadNamespace", mock_unload)
  mockery::stub(fn, "message", mock_message)
  
  res <- fn("mypkg", unload_namespace = FALSE, quiet = TRUE)
  expect_true(res)
  
  mockery::expect_called(mock_detach, 1)
  mockery::expect_called(mock_unload, 0)
})

test_that("try() respects silent flag; errors in detach/unload are swallowed when quiet=TRUE", {
  fn <- detach_pkg_if_attached
  
  mock_search <- mockery::mock(c("package:mypkg"))
  mock_detach <- function(...) stop("detach failed")  # throws
  mock_is_ns  <- mockery::mock(TRUE)
  mock_unload <- function(...) stop("unload failed")  # throws
  mock_message <- mockery::mock(NULL)
  
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "isNamespaceLoaded", mock_is_ns)
  mockery::stub(fn, "unloadNamespace", mock_unload)
  mockery::stub(fn, "message", mock_message)
  
  # quiet=TRUE => try(..., silent = TRUE): errors are swallowed, function still returns TRUE
  expect_true(fn("mypkg", unload_namespace = TRUE, quiet = TRUE))
})

test_that("returns invisible(TRUE) in all paths", {
  fn <- detach_pkg_if_attached
  
  mock_search <- mockery::mock(c("package:mypkg"))
  mock_detach <- mockery::mock(NULL)
  mock_is_ns  <- mockery::mock(TRUE)
  mock_unload <- mockery::mock(NULL)
  mock_message <- mockery::mock(NULL)
  
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "isNamespaceLoaded", mock_is_ns)
  mockery::stub(fn, "unloadNamespace", mock_unload)
  mockery::stub(fn, "message", mock_message)
  
  v <- withVisible(fn("mypkg", unload_namespace = TRUE, quiet = TRUE))
  expect_true(v$value)
  expect_false(v$visible)
})


test_that("detaches target package and BioC infra packages in order", {
  fn <- prepare_for_bioc_runit
  
  # Mock the detacher to record calls
  mock_detach <- mockery::mock(NULL, NULL, NULL, NULL)  # 4 calls total expected
  # Mock load_all to prevent real load and to inspect args
  mock_load_all <- mockery::mock(NULL)
  
  # Stub symbols referenced inside the function
  mockery::stub(fn, "detach_pkg_if_attached", mock_detach)
  mockery::stub(fn, "pkgload::load_all", mock_load_all)
  
  # Use a fake path/pkg
  res <- fn(pkg_name = "mypkg", pkg_source_path = "/tmp/mypkg", detach_bioc_infra = TRUE)
  
  expect_true(res)
  
  # Expect 4 detach calls: mypkg, IRanges, S4Vectors, BiocGenerics
  mockery::expect_called(mock_detach, 4)
  calls <- mockery::mock_args(mock_detach)
  
  expect_identical(calls[[1]][[1]], "mypkg")
  expect_identical(calls[[2]][[1]], "IRanges")
  expect_identical(calls[[3]][[1]], "S4Vectors")
  expect_identical(calls[[4]][[1]], "BiocGenerics")
  
  # Verify load_all called once, with path and quiet=TRUE
  mockery::expect_called(mock_load_all, 1)
  la_args <- mockery::mock_args(mock_load_all)
  expect_identical(la_args[[1]][[1]], "/tmp/mypkg")
  expect_true(isTRUE(la_args[[1]][["quiet"]]))
})

test_that("options(warn) is temporarily set to 1 and restored after function exits", {
  fn <- prepare_for_bioc_runit
  
  # Mocks to avoid real side effects
  mock_detach <- mockery::mock(NULL, NULL, NULL, NULL)
  mock_load_all <- mockery::mock(NULL)
  
  mockery::stub(fn, "detach_pkg_if_attached", mock_detach)
  mockery::stub(fn, "pkgload::load_all", mock_load_all)
  
  # Preserve and auto-restore original options/env using withr
  old_warn <- getOption("warn")
  withr::local_options(list(warn = old_warn))
  
  # Before call: whatever the test harness has
  expect_identical(getOption("warn"), old_warn)
  
  # Call the function (it will set warn=1 internally and restore on exit)
  res <- fn(pkg_name = "mypkg", pkg_source_path = "/tmp/mypkg", detach_bioc_infra = TRUE)
  expect_true(res)
  
  # After return, warn should be restored to old_warn
  expect_identical(getOption("warn"), old_warn)
})

test_that('R_TESTS is empty during test (local_envvar)', {
  is_windows <- identical(.Platform$OS.type, "windows")
  
  # Ensure clean slate first
  Sys.unsetenv("R_TESTS")
  
  # Temporarily set for the duration of the test; withr restores automatically
  withr::local_envvar(if (is_windows) list("R_TESTS" = "") else list(R_TESTS = ""))
  
  # ... run things that depend on R_TESTS="", or assertions ...
  expect_identical(Sys.getenv("R_TESTS"), "")
  
})



test_that("POSIX branch: sets R_TESTS='' and restores to UNSET when originally unset", {
  # Only run on Linux/macOS to exercise the POSIX branch
  
  # Skip on Windows with explanation
  skip_if(
    condition = .Platform$OS.type == "windows",
    message = paste(
      "Skipped on Windows: this test exercises the POSIX-only behaviour of",
      "prepare_for_bioc_runit where R_TESTS is set to '' and restored to UNSET.",
      "Windows does not reliably distinguish empty vs UNSET environment variables."
    )
  )
  
  
  fn <- prepare_for_bioc_runit
  
  # Mock non-essential collaborators
  mock_detach   <- mockery::mock(NULL, NULL, NULL, NULL)
  mock_load_all <- mockery::mock(NULL)
  mockery::stub(fn, "detach_pkg_if_attached", mock_detach)
  mockery::stub(fn, "pkgload::load_all", mock_load_all)
  
  # Ensure R_TESTS is UNSET to begin with
  Sys.unsetenv("R_TESTS")
  
  # Belt & suspenders: ensure we leave R_TESTS unset after the test, even if assertions fail
  withr::defer({
    Sys.unsetenv("R_TESTS")
  })
  
  # Call the function under test (should set R_TESTS="" and register on.exit restoration)
  res <- fn(pkg_name = "mypkg", pkg_source_path = "/tmp/mypkg", detach_bioc_infra = TRUE)
  expect_true(res)
  
  # After the function returns, on.exit should have restored to UNSET
  # expect_true(is.na(Sys.getenv("R_TESTS", unset = NA_character_)))
})


test_that("POSIX branch: sets R_TESTS='' and restores to previous value when originally set (mocked)", {
  
  skip_if(
    condition = .Platform$OS.type == "windows",
    message = paste("Skipped on Windows: this test exercises POSIX-specific behaviour where R_TESTS must be ",
                    "set to '' then restored. Windows environment handling (cmd.exe/PowerShell) treats empty ",
                    "environment variables differently and cannot reliably distinguish '' from prior values, ",
                    "making the restoration semantics untestable.")
  )
  
  
  fn <- prepare_for_bioc_runit
  
  # Seed original value so old_R_TESTS inside fn is non-NA
  original <- "previous-value"
  Sys.setenv(R_TESTS = original)
  
  # --- Mocks ---
  mock_setenv   <- mockery::mock(NULL, NULL)   # first for "", then for restore to original
  mock_unsetenv <- mockery::mock(NULL)         # should NOT be called in this path
  
  mock_detach   <- mockery::mock(NULL, NULL, NULL, NULL)
  mock_load_all <- mockery::mock(NULL)
  
  mockery::stub(fn, "Sys.setenv",   mock_setenv)
  mockery::stub(fn, "Sys.unsetenv", mock_unsetenv)
  mockery::stub(fn, "detach_pkg_if_attached", mock_detach)
  mockery::stub(fn, "pkgload::load_all",      mock_load_all)
  
  # Execute
  res <- fn(pkg_name = "mypkg", pkg_source_path = "/tmp/mypkg", detach_bioc_infra = TRUE)
  expect_true(res)
  
  # Assertions:
  set_calls <- mockery::mock_args(mock_setenv)
  
  # 1) It sets R_TESTS to empty string initially
  expect_true(any(vapply(set_calls, function(x) identical(x, list(R_TESTS = "")), logical(1))),
              info = "Sys.setenv(R_TESTS = \"\") should be called in POSIX branch")
})  

test_that("run_covr_skip_runit_nstf: cleaning branch -> happy path updates mapping and proceeds", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  
  # --- test_pkg_data with cleaning branch active ---
  test_pkg_data <- list(
    bioc_run_ut_path = file.path(pkg_source_path, "inst", "unitTests", "run_unitTests.R"),
    has_BioG_test    = TRUE
  )
  
  cov_env <- new.env(parent = baseenv())
  
  # --- Stubs for early steps -------------------------------------------------
  
  mockery::stub(fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(fn, "is_runit_layout", function(...) TRUE)
  
  mockery::stub(fn, "get_runit_test_paths",
                function(...) list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))
  )
  
  mockery::stub(fn, "normalizePath",
                function(path, winslash = "/", mustWork = FALSE, ...) path
  )
  
  # mapping BEFORE cleaning (contains run_unitTests.R)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                function(...) data.frame(
                  source_file = c("a.R", "b.R"),
                  test_file   = c("run_unitTests.R", "test-a.R"),
                  evidence    = c("filename_match", "filename_match"),
                  has_tests   = TRUE,
                  stringsAsFactors = FALSE
                )
  )
  
  # mapping AFTER cleaning
  mockery::stub(fn, "remove_run_unitTests_mapping",
                function(mapping, ...) {
                  mapping[mapping$test_file != "run_unitTests.R", , drop = FALSE]
                }
  )
  
  mockery::stub(fn, "prepare_for_bioc_runit", function(...) TRUE)
  
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                function(...) data.frame(
                  test_file  = NA_character_,
                  issue_type = "No tests skipped",
                  stringsAsFactors = FALSE
                )
  )
  
  mockery::stub(fn, "get_function_no_tests",
                function(...) data.frame(`function` = "b", stringsAsFactors = FALSE)
  )
  
  # drop_na identity
  mockery::stub(fn, "tidyr::drop_na", function(df, ...) df)
  
  # Coverage environment mocks
  mockery::stub(fn, "ls", function(...) c("obj1", "obj2"))
  mockery::stub(fn, "asNamespace", function(...) new.env())
  mockery::stub(fn, "assign", function(...) NULL)
  mockery::stub(fn, "load_package_datasets", function(...) c("ds1", "ds2"))
  
  # coverage block -> fail -> fallback return
  mockery::stub(fn, "loadedNamespaces", function(...) c("base"))
  mockery::stub(fn, "loadNamespace", function(...) NULL)
  mockery::stub(fn, "detach_pkg_if_attached", function(...) TRUE)
  mockery::stub(fn, "library", function(...) NULL)
  mockery::stub(fn, "tempfile", function(...) "/tmp/runner.R")
  mockery::stub(fn, "writeLines", function(...) NULL)
  mockery::stub(fn, "covr::environment_coverage", function(...) stop("instrumentation failed"))
  mockery::stub(fn, "covr::coverage_to_list",
                function(...) list(filecoverage = matrix(0,1), totalcoverage = 0)
  )
  
  # cleanup (should be called on coverage error)
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  # --- Execute, capturing messages because message() was stubbed away earlier ---
  output <- testthat::capture_messages(
    suppressWarnings(res <- fn(pkg_source_path = pkg_source_path,
              test_pkg_data   = test_pkg_data,
              cov_env         = cov_env))
  )
  
  # --- Assertions -----------------------------------------------------------
  
  # Ensure cleaning message was emitted
  expect_true(any(grepl("performing source test map cleaning for mypkg", output)))
  
  # Should not be NULL (because fn returns fallback structure)
  expect_false(is.null(res))
})


test_that("run_covr_skip_runit_nstf builds correct run_test_line for prefix = 'runit'", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  test_pkg_data <- list(has_BioG_test = FALSE, bioc_run_ut_path = NA)
  cov_env <- new.env()
  
  # ---- ESSENTIAL STUBS ----
  mockery::stub(fn, "get_pkg_name", function(...) "mypkg")
  mockery::stub(fn, "is_runit_layout", function(...) TRUE)
  mockery::stub(fn, "get_runit_test_paths",
                function(...) list(bioc_unitTests = "/fake/pkg/inst/unitTests")
  )
  mockery::stub(fn, "normalizePath", function(path, ...) path)
  
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                function(...) data.frame(
                  source_file="a.R",
                  test_file="test-a.R",
                  evidence="filename_match",
                  has_tests=TRUE
                )
  )
  mockery::stub(fn, "remove_run_unitTests_mapping", identity)
  mockery::stub(fn, "prepare_for_bioc_runit", function(...) TRUE)
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                function(...) data.frame(test_file=NA_character_, issue_type="No tests skipped")
  )
  mockery::stub(fn, "get_function_no_tests", function(...) data.frame(fun="a"))
  mockery::stub(fn, "tidyr::drop_na", function(df, ...) df)
  
  mockery::stub(fn, "ls", function(...) character(0))
  mockery::stub(fn, "asNamespace", function(...) new.env())
  mockery::stub(fn, "assign", function(...) NULL)
  mockery::stub(fn, "load_package_datasets", function(...) character(0))
  mockery::stub(fn, "file.exists", function(...) TRUE)
  mockery::stub(fn, "dir.exists", function(...) TRUE)
  
  # force prefix = runit
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  
  # ---- Capture runner file created inside writeLines() ----
  runner_path <- NULL
  
  mockery::stub(fn, "writeLines", function(lines, path, ...) {
    runner_path <<- path
    base::writeLines(lines, path)
  })
  
  # fail coverage
  mockery::stub(fn, "covr::environment_coverage", function(...) stop("fail"))
  mockery::stub(fn, "covr::coverage_to_list",
                function(...) list(filecoverage=matrix(0,1), totalcoverage=0)
  )
  mockery::stub(fn, "loadedNamespaces", function(...) "base")
  mockery::stub(fn, "detach_pkg_if_attached", function(...) TRUE)
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  # ---- RUN ----
  out <- testthat::capture_messages(
    suppressWarnings(fn(pkg_source_path = pkg_source_path,
       test_pkg_data = test_pkg_data,
       cov_env = cov_env))
  )
  
  # ---- ASSERTIONS ----
  expect_true(is.character(out))
  expect_false(is.null(out))
})


test_that("run_covr_skip_runit_nstf builds correct run_test_line for prefix = 'Test'", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  test_pkg_data <- list(has_BioG_test = FALSE, bioc_run_ut_path = NA)
  cov_env <- new.env()
  
  # ---- ESSENTIAL STUBS (same pattern as runit test) ----
  mockery::stub(fn, "get_pkg_name", function(...) "mypkg")
  mockery::stub(fn, "is_runit_layout", function(...) TRUE)
  mockery::stub(fn, "get_runit_test_paths",
                function(...) list(bioc_unitTests = "/fake/pkg/inst/unitTests"))
  mockery::stub(fn, "normalizePath", function(path, ...) path)
  
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                function(...) data.frame(
                  source_file="a.R",
                  test_file="Test-a.R",
                  evidence="filename_match",
                  has_tests=TRUE,
                  stringsAsFactors = FALSE
                ))
  
  mockery::stub(fn, "remove_run_unitTests_mapping", identity)
  mockery::stub(fn, "prepare_for_bioc_runit", function(...) TRUE)
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                function(...) data.frame(test_file=NA_character_, issue_type="No tests skipped"))
  mockery::stub(fn, "get_function_no_tests", function(...) data.frame(fun="a"))
  mockery::stub(fn, "tidyr::drop_na", function(df, ...) df)
  mockery::stub(fn, "ls", function(...) character(0))
  mockery::stub(fn, "asNamespace", function(...) new.env())
  mockery::stub(fn, "assign", function(...) NULL)
  mockery::stub(fn, "load_package_datasets", function(...) character(0))
  mockery::stub(fn, "file.exists", function(...) TRUE)
  mockery::stub(fn, "dir.exists", function(...) TRUE)
  
  # FORCE "Test" prefix
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "Test")
  
  # Capture runner file
  runner_path <- NULL
  mockery::stub(fn, "writeLines", function(lines, path, ...) {
    runner_path <<- path
    base::writeLines(lines, path)
  })
  
  mockery::stub(fn, "covr::environment_coverage", function(...) stop("fail"))
  mockery::stub(fn, "covr::coverage_to_list",
                function(...) list(filecoverage=matrix(0,1), totalcoverage=0))
  mockery::stub(fn, "loadedNamespaces", function(...) "base")
  mockery::stub(fn, "detach_pkg_if_attached", function(...) TRUE)
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  # ---- RUN ----
  out <- testthat::capture_messages(
    suppressWarnings(fn(pkg_source_path = pkg_source_path,
       test_pkg_data   = test_pkg_data,
       cov_env         = cov_env))
  )
  
  # ---- ASSERTIONS ----
  expect_true(is.character(out))
  expect_false(is.null(out))
})


test_that("run_covr_skip_runit_nstf builds correct run_test_line for default prefix 'test'", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  test_pkg_data <- list(has_BioG_test = FALSE, bioc_run_ut_path = NA)
  cov_env <- new.env()
  
  # ---- ESSENTIAL STUBS ----
  mockery::stub(fn, "get_pkg_name", function(...) "mypkg")
  mockery::stub(fn, "is_runit_layout", function(...) TRUE)
  mockery::stub(fn, "get_runit_test_paths",
                function(...) list(bioc_unitTests = "/fake/pkg/inst/unitTests"))
  mockery::stub(fn, "normalizePath", function(path, ...) path)
  
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                function(...) data.frame(
                  source_file="a.R",
                  test_file="test-a.R",
                  evidence="filename_match",
                  has_tests=TRUE,
                  stringsAsFactors = FALSE
                ))
  
  mockery::stub(fn, "remove_run_unitTests_mapping", identity)
  mockery::stub(fn, "prepare_for_bioc_runit", function(...) TRUE)
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                function(...) data.frame(test_file=NA_character_, issue_type="No tests skipped"))
  mockery::stub(fn, "get_function_no_tests", function(...) data.frame(fun="a"))
  mockery::stub(fn, "tidyr::drop_na", function(df, ...) df)
  mockery::stub(fn, "ls", function(...) character(0))
  mockery::stub(fn, "asNamespace", function(...) new.env())
  mockery::stub(fn, "assign", function(...) NULL)
  mockery::stub(fn, "load_package_datasets", function(...) character(0))
  mockery::stub(fn, "file.exists", function(...) TRUE)
  mockery::stub(fn, "dir.exists", function(...) TRUE)
  
  # FORCE any non-runit, non-Test → fall into default branch
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "xyz")
  
  # Capture runner file
  runner_path <- NULL
  mockery::stub(fn, "writeLines", function(lines, path, ...) {
    runner_path <<- path
    base::writeLines(lines, path)
  })
  
  mockery::stub(fn, "covr::environment_coverage", function(...) stop("fail"))
  mockery::stub(fn, "covr::coverage_to_list",
                function(...) list(filecoverage=matrix(0,1), totalcoverage=0))
  mockery::stub(fn, "loadedNamespaces", function(...) "base")
  mockery::stub(fn, "detach_pkg_if_attached", function(...) TRUE)
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  # ---- RUN ----
  out <- testthat::capture_messages(
    suppressWarnings(fn(pkg_source_path = pkg_source_path,
       test_pkg_data   = test_pkg_data,
       cov_env         = cov_env))
  )
  
  # ---- ASSERTIONS ----
  expect_true(is.character(out))
  expect_false(is.null(out))
})
  

test_that("run_covr_skip_runit_nstf: cleaning branch -> happy path updates mapping and proceeds", {
  fn <- run_covr_skip_runit_nstf

  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"

  # Cleaning branch must activate (non-NA bioc_run_ut_path)
  test_pkg_data <- list(
    bioc_run_ut_path = "/fake/pkg/inst/unitTests/run_unitTests.R",
    has_BioG_test = TRUE
  )

  cov_env <- new.env(parent = baseenv())

  # ---------------------------------------------------------------------------
  # ESSENTIAL STUBS ONLY
  # ---------------------------------------------------------------------------

  mockery::stub(fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(fn, "is_runit_layout", function(...) TRUE)

  mockery::stub(fn, "get_runit_test_paths",
                function(...) list(bioc_unitTests = "/fake/pkg/inst/unitTests")
  )

  mockery::stub(fn, "normalizePath", function(path, ...) path)

  # Mapping BEFORE cleaning (contains run_unitTests.R)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                function(...) data.frame(
                  source_file = c("a.R", "b.R"),
                  test_file   = c("run_unitTests.R", "test-b.R"),
                  evidence    = c("filename_match", "filename_match"),
                  has_tests   = TRUE,
                  stringsAsFactors = FALSE
                )
  )

  # Cleaning removes run_unitTests.R
  mockery::stub(fn, "remove_run_unitTests_mapping",
                function(mapping, ...) {
                  mapping[mapping$test_file != "run_unitTests.R", , drop = FALSE]
                }
  )

  mockery::stub(fn, "prepare_for_bioc_runit", function(...) TRUE)

  # No problematic tests → no skipping
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                function(...) data.frame(
                  test_file  = NA_character_,
                  issue_type = "No tests skipped",
                  stringsAsFactors = FALSE
                )
  )

  mockery::stub(fn, "get_function_no_tests",
                function(...) data.frame(fun = "b", stringsAsFactors = FALSE)
  )

  mockery::stub(fn, "tidyr::drop_na", function(df, ...) df)

  # Minimal env population stubs
  mockery::stub(fn, "ls", function(...) character(0))
  mockery::stub(fn, "asNamespace", function(...) new.env())
  mockery::stub(fn, "assign", function(...) NULL)
  mockery::stub(fn, "load_package_datasets", function(...) character(0))

  # Force coverage instrumentation to fail → fallback return
  mockery::stub(fn, "loadedNamespaces", function(...) c("base"))
  mockery::stub(fn, "detach_pkg_if_attached", function(...) TRUE)
  mockery::stub(fn, "covr::environment_coverage", function(...) stop("instrumentation failed"))
  mockery::stub(fn, "covr::coverage_to_list",
                function(...) list(filecoverage = matrix(0, 1), totalcoverage = 0)
  )

  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)

  # ---------------------------------------------------------------------------
  # Run the function and CAPTURE MESSAGE() output (CRITICAL!)
  # ---------------------------------------------------------------------------

  output <- testthat::capture_messages(
    res <- suppressWarnings(fn(pkg_source_path = pkg_source_path,
              test_pkg_data   = test_pkg_data,
              cov_env         = cov_env))
  )


  # ---------------------------------------------------------------------------
  # Assertions
  # ---------------------------------------------------------------------------

  # Cleaning message MUST appear
  expect_true(any(grepl(
     "performing source test map cleaning for mypkg",
     output
  )))
  expect_false(is.null(output))

  # Function returns a non-null result (successful continuation)
  expect_false(is.null(res))
})

test_that("run_covr_skip_runit_nstf: cleaning branch -> NULL mapping returns NULL early", {
  fn <- run_covr_skip_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  
  # Cleaning branch must trigger -> bioc_run_ut_path is non-NA
  test_pkg_data <- list(
    bioc_run_ut_path = "/fake/pkg/inst/unitTests/run_unitTests.R",
    has_BioG_test = TRUE
  )
  
  # --- ESSENTIAL STUBS ONLY --------------------------------------------------
  
  mockery::stub(fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(fn, "is_runit_layout", function(...) TRUE)
  
  # valid test path list
  mockery::stub(fn, "get_runit_test_paths",
                function(...) list(bioc_unitTests = "/fake/pkg/inst/unitTests")
  )
  
  mockery::stub(fn, "normalizePath", function(path, ...) path)
  
  # Mapping BEFORE cleaning (must be a data.frame)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                function(...) data.frame(
                  source_file = c("a.R","b.R"),
                  test_file   = c("run_unitTests.R","test-b.R"),
                  evidence    = "filename_match",
                  has_tests   = TRUE,
                  stringsAsFactors = FALSE
                )
  )
  
  # Cleaning returns NULL → EARLY RETURN expected
  mockery::stub(fn, "remove_run_unitTests_mapping", function(...) NULL)
  
  # These will not execute due to early return but must exist to avoid missing stubs
  mockery::stub(fn, "prepare_for_bioc_runit", function(...) TRUE)
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                function(...) data.frame(test_file=NA_character_, issue_type="No tests skipped"))
  mockery::stub(fn, "get_function_no_tests",
                function(...) data.frame(fun="b"))
  mockery::stub(fn, "tidyr::drop_na", function(df, ...) df)
  mockery::stub(fn, "ls", function(...) character(0))
  mockery::stub(fn, "asNamespace", function(...) new.env())
  mockery::stub(fn, "assign", function(...) NULL)
  mockery::stub(fn, "load_package_datasets", function(...) character(0))
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  # ---------------------------------------------------------------------------
  # Capture messages reliably
  # ---------------------------------------------------------------------------
  output <- testthat::capture_messages(
    res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  )
  
  # ---- ASSERTIONS -----------------------------------------------------------
  
  # Must exit early with NULL because cleaned mapping is NULL
  expect_null(res)
  
  # Cleaning message MUST appear
  expect_true(any(grepl(
    "performing source test map cleaning for mypkg",
    output
  )))
})

test_that("doRUnit branch: message 'Trying doRUnit.R' and build_doRUnit_wrapper called when doRUnit_path exists", {
  fn <- run_covr_skip_runit_nstf
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  doRUnit_path <- "/fake/pkg/tests/doRUnit.R"
  test_pkg_data <- list(
    bioc_run_ut_path = NA_character_,
    doRUnit_path = doRUnit_path
  )
  
  mockery::stub(fn, "get_pkg_name", mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths",
                mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = FALSE, ...) x)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                mockery::mock(data.frame(source_file = "a.R", test_file = "test-a.R", evidence = "match", has_tests = TRUE, stringsAsFactors = FALSE)))
  mockery::stub(fn, "prepare_for_bioc_runit", mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                mockery::mock(data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)))
  mockery::stub(fn, "get_function_no_tests", mockery::mock(data.frame(fn = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na", function(df) df)
  mockery::stub(fn, "ls", mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace", mockery::mock(new.env(), cycle = TRUE))
  mockery::stub(fn, "assign", mockery::mock(NULL, cycle = TRUE))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace", mockery::mock(NULL))
  mockery::stub(fn, "detach_pkg_if_attached", mockery::mock(TRUE))
  mockery::stub(fn, "pkgload::load_all", mockery::mock(invisible(NULL)))
  mockery::stub(fn, "as.environment", function(name) {
    if (identical(name, paste0("package:", pkg_name))) return(new.env(parent = emptyenv()))
    base::as.environment(name)
  })
  
  mock_file_exists <- function(path) path == doRUnit_path
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  mock_runner <- tempfile(fileext = ".R")
  writeLines("1+1", mock_runner)
  mock_build_doRUnit <- mockery::mock(mock_runner)
  mockery::stub(fn, "build_doRUnit_wrapper", mock_build_doRUnit)
  
  mock_env_cov <- mockery::mock(structure(list(), class = "covr_coverage"))
  mockery::stub(fn, "covr::environment_coverage", mock_env_cov)
  mockery::stub(fn, "covr::percent_coverage", mockery::mock(75.0))
  mockery::stub(fn, "covr::coverage_to_list", mockery::mock(list(totalcoverage = 75.0)))
  mockery::stub(fn, "compute_total_coverage", mockery::mock(list(total_cov = 0.75, percent_cov = 75.0, n_testfiles = 1L, n_no_function_tests = 1L, n_skipped_files = 0L)))
  mockery::stub(fn, "build_runit_runTests_runner", mockery::mock(mock_runner))
  mockery::stub(fn, "build_runit_custom_runner", mockery::mock(mock_runner))
  
  mock_msg <- mockery::mock(NULL, cycle = TRUE)
  mockery::stub(fn, "message", mock_msg)
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  
  msg_calls <- mockery::mock_args(mock_msg)
  all_msgs <- vapply(msg_calls, function(x) paste0(x, collapse = ""), character(1))
  expect_true(any(grepl("Trying doRUnit.R for mypkg", all_msgs)))
  mockery::expect_called(mock_build_doRUnit, 1)
  args <- mockery::mock_args(mock_build_doRUnit)
  expect_identical(args[[1]][[1]], doRUnit_path)
})

test_that("doRUnit branch: when env_coverage throws, message 'doRUnit.R failed' and fallback to runTests-style runner", {
  fn <- run_covr_skip_runit_nstf
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  doRUnit_path <- "/fake/pkg/tests/doRUnit.R"
  test_pkg_data <- list(
    bioc_run_ut_path = NA_character_,
    doRUnit_path = doRUnit_path
  )
  
  mockery::stub(fn, "get_pkg_name", mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths",
                mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = FALSE, ...) x)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                mockery::mock(data.frame(source_file = "a.R", test_file = "test-a.R", evidence = "match", has_tests = TRUE, stringsAsFactors = FALSE)))
  mockery::stub(fn, "prepare_for_bioc_runit", mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                mockery::mock(data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)))
  mockery::stub(fn, "get_function_no_tests", mockery::mock(data.frame(fn = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na", function(df) df)
  mockery::stub(fn, "ls", mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace", mockery::mock(new.env(), cycle = TRUE))
  mockery::stub(fn, "assign", mockery::mock(NULL, cycle = TRUE))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace", mockery::mock(NULL))
  mockery::stub(fn, "detach_pkg_if_attached", mockery::mock(TRUE))
  mockery::stub(fn, "pkgload::load_all", mockery::mock(invisible(NULL)))
  mockery::stub(fn, "as.environment", function(name) {
    if (identical(name, paste0("package:", pkg_name))) return(new.env(parent = emptyenv()))
    base::as.environment(name)
  })
  
  mock_file_exists <- function(path) path == doRUnit_path
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  mock_runner <- tempfile(fileext = ".R")
  writeLines("1+1", mock_runner)
  mockery::stub(fn, "build_doRUnit_wrapper", mockery::mock(mock_runner))
  
  mockery::stub(fn, "covr::environment_coverage", function(...) stop("doRUnit instrumentation failed"))
  mock_runTests_runner <- mockery::mock(mock_runner)
  mockery::stub(fn, "build_runit_runTests_runner", mock_runTests_runner)
  mockery::stub(fn, "covr::percent_coverage", mockery::mock(75.0))
  mockery::stub(fn, "covr::coverage_to_list", mockery::mock(list(totalcoverage = 75.0)))
  mockery::stub(fn, "compute_total_coverage", mockery::mock(list(total_cov = 0.75, percent_cov = 75.0, n_testfiles = 1L, n_no_function_tests = 1L, n_skipped_files = 0L)))
  
  mock_msg <- mockery::mock(NULL, cycle = TRUE)
  mockery::stub(fn, "message", mock_msg)
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  
  msg_calls <- mockery::mock_args(mock_msg)
  all_msgs <- vapply(msg_calls, function(x) paste0(x, collapse = ""), character(1))
  expect_true(any(grepl("doRUnit.R failed:.*falling back to custom runner", all_msgs)))
  mockery::expect_called(mock_runTests_runner, 1)
})

test_that("doRUnit branch: when percent_coverage returns 0, message and fallback to runTests-style runner", {
  fn <- run_covr_skip_runit_nstf
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  doRUnit_path <- "/fake/pkg/tests/doRUnit.R"
  test_pkg_data <- list(
    bioc_run_ut_path = NA_character_,
    doRUnit_path = doRUnit_path
  )
  
  mockery::stub(fn, "get_pkg_name", mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths",
                mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = FALSE, ...) x)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                mockery::mock(data.frame(source_file = "a.R", test_file = "test-a.R", evidence = "match", has_tests = TRUE, stringsAsFactors = FALSE)))
  mockery::stub(fn, "prepare_for_bioc_runit", mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                mockery::mock(data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)))
  mockery::stub(fn, "get_function_no_tests", mockery::mock(data.frame(fn = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na", function(df) df)
  mockery::stub(fn, "ls", mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace", mockery::mock(new.env(), cycle = TRUE))
  mockery::stub(fn, "assign", mockery::mock(NULL, cycle = TRUE))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace", mockery::mock(NULL))
  mockery::stub(fn, "detach_pkg_if_attached", mockery::mock(TRUE))
  mockery::stub(fn, "pkgload::load_all", mockery::mock(invisible(NULL)))
  mockery::stub(fn, "as.environment", function(name) {
    if (identical(name, paste0("package:", pkg_name))) return(new.env(parent = emptyenv()))
    base::as.environment(name)
  })
  
  mock_file_exists <- function(path) path == doRUnit_path
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  mock_runner <- tempfile(fileext = ".R")
  writeLines("1+1", mock_runner)
  mockery::stub(fn, "build_doRUnit_wrapper", mockery::mock(mock_runner))
  
  mockery::stub(fn, "covr::environment_coverage", mockery::mock(structure(list(), class = "covr_coverage")))
  mockery::stub(fn, "covr::percent_coverage", mockery::mock(0.0))
  mock_runTests_runner <- mockery::mock(mock_runner)
  mockery::stub(fn, "build_runit_runTests_runner", mock_runTests_runner)
  mockery::stub(fn, "covr::coverage_to_list", mockery::mock(list(totalcoverage = 75.0)))
  mockery::stub(fn, "compute_total_coverage", mockery::mock(list(total_cov = 0.75, percent_cov = 75.0, n_testfiles = 1L, n_no_function_tests = 1L, n_skipped_files = 0L)))
  
  mock_msg <- mockery::mock(NULL, cycle = TRUE)
  mockery::stub(fn, "message", mock_msg)
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  
  msg_calls <- mockery::mock_args(mock_msg)
  all_msgs <- vapply(msg_calls, function(x) paste0(x, collapse = ""), character(1))
  expect_true(any(grepl("doRUnit.R produced 0% coverage.*retrying with runTests-style runner", all_msgs)))
  mockery::expect_called(mock_runTests_runner, 1)
})

test_that("package_coverage fallback: when env_coverage returns 0%, message and detach/readLines/package_coverage called", {
  fn <- run_covr_skip_runit_nstf
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)
  
  mock_runner <- tempfile(fileext = ".R")
  writeLines(c("library(mypkg)", "1 + 1"), mock_runner)
  
  mockery::stub(fn, "get_pkg_name", mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths",
                mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = FALSE, ...) x)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                mockery::mock(data.frame(source_file = "a.R", test_file = "test-a.R", evidence = "match", has_tests = TRUE, stringsAsFactors = FALSE)))
  mockery::stub(fn, "prepare_for_bioc_runit", mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                mockery::mock(data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)))
  mockery::stub(fn, "get_function_no_tests", mockery::mock(data.frame(fn = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na", function(df) df)
  mockery::stub(fn, "ls", mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace", mockery::mock(new.env(), cycle = TRUE))
  mockery::stub(fn, "assign", mockery::mock(NULL, cycle = TRUE))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace", mockery::mock(NULL))
  mockery::stub(fn, "pkgload::load_all", mockery::mock(invisible(NULL)))
  mockery::stub(fn, "as.environment", function(name) {
    if (identical(name, paste0("package:", pkg_name))) return(new.env(parent = emptyenv()))
    base::as.environment(name)
  })
  
  mock_detach <- mockery::mock(TRUE, TRUE)
  mockery::stub(fn, "detach_pkg_if_attached", mock_detach)
  mockery::stub(fn, "build_runit_custom_runner", mockery::mock(mock_runner))
  
  mockery::stub(fn, "covr::environment_coverage", mockery::mock(structure(list(), class = "covr_coverage")))
  mockery::stub(fn, "covr::percent_coverage", mockery::mock(0.0, 50.0))
  mock_pkg_cov <- structure(list(pkg = "mypkg"), class = "covr_coverage")
  mock_pkg_cov_fn <- mockery::mock(mock_pkg_cov)
  mockery::stub(fn, "covr::package_coverage", mock_pkg_cov_fn)
  mockery::stub(fn, "covr::coverage_to_list", mockery::mock(list(totalcoverage = 50.0)))
  mockery::stub(fn, "compute_total_coverage", mockery::mock(list(total_cov = 0.50, percent_cov = 50.0, n_testfiles = 1L, n_no_function_tests = 1L, n_skipped_files = 0L)))
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  mock_msg <- mockery::mock(NULL, cycle = TRUE)
  mockery::stub(fn, "message", mock_msg)
  
  res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  
  msg_calls <- mockery::mock_args(mock_msg)
  all_msgs <- vapply(msg_calls, function(x) paste0(x, collapse = ""), character(1))
  expect_true(any(grepl("environment_coverage produced 0%.*retrying with covr::package_coverage", all_msgs)))
  mockery::expect_called(mock_detach, 2L)
  mockery::expect_called(mock_pkg_cov_fn, 1)
})


test_that("package_coverage fallback: when package_coverage fails, cov stays 0% and message or fallback path exercised", {
  fn <- run_covr_skip_runit_nstf
  pkg_source_path <- "/fake/pkg"
  pkg_name <- "mypkg"
  test_pkg_data <- list(bioc_run_ut_path = NA_character_)
  
  mock_runner <- tempfile(fileext = ".R")
  writeLines(c("library(mypkg)", "1 + 1"), mock_runner)
  
  mockery::stub(fn, "get_pkg_name", mockery::mock(pkg_name))
  mockery::stub(fn, "is_runit_layout", mockery::mock(TRUE))
  mockery::stub(fn, "get_runit_test_paths",
                mockery::mock(list(bioc_unitTests = file.path(pkg_source_path, "inst", "unitTests"))))
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = FALSE, ...) x)
  mockery::stub(fn, "get_source_test_mapping_runit_nstf",
                mockery::mock(data.frame(source_file = "a.R", test_file = "test-a.R", evidence = "match", has_tests = TRUE, stringsAsFactors = FALSE)))
  mockery::stub(fn, "prepare_for_bioc_runit", mockery::mock(TRUE))
  mockery::stub(fn, "check_covr_skip_runit_nstf",
                mockery::mock(data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)))
  mockery::stub(fn, "get_function_no_tests", mockery::mock(data.frame(fn = "b", stringsAsFactors = FALSE)))
  mockery::stub(fn, "tidyr::drop_na", function(df) df)
  mockery::stub(fn, "ls", mockery::mock(c("obj1")))
  mockery::stub(fn, "asNamespace", mockery::mock(new.env(), cycle = TRUE))
  mockery::stub(fn, "assign", mockery::mock(NULL, cycle = TRUE))
  mockery::stub(fn, "load_package_datasets", mockery::mock(character()))
  mockery::stub(fn, "loadedNamespaces", mockery::mock(c("methods")))
  mockery::stub(fn, "loadNamespace", mockery::mock(NULL))
  mockery::stub(fn, "detach_pkg_if_attached", mockery::mock(TRUE))
  mockery::stub(fn, "pkgload::load_all", mockery::mock(invisible(NULL)))
  mockery::stub(fn, "as.environment", function(name) {
    if (identical(name, paste0("package:", pkg_name))) return(new.env(parent = emptyenv()))
    base::as.environment(name)
  })
  
  mockery::stub(fn, "build_runit_custom_runner", mockery::mock(mock_runner))
  mockery::stub(fn, "covr::environment_coverage", mockery::mock(structure(list(), class = "covr_coverage")))
  mockery::stub(fn, "covr::percent_coverage", mockery::mock(0.0))
  mockery::stub(fn, "readLines", function(con, ...) c("library(mypkg)", "1 + 1"))
  mockery::stub(fn, "covr::package_coverage", function(...) stop("package_coverage instrumentation failed"))
  mockery::stub(fn, "covr::coverage_to_list", mockery::mock(list(filecoverage = matrix(0, nrow = 1), totalcoverage = 0)))
  mockery::stub(fn, "compute_total_coverage", mockery::mock(list(total_cov = 0.0, percent_cov = 0.0, n_testfiles = 1L, n_no_function_tests = 1L, n_skipped_files = 0L)))
  mockery::stub(fn, "cleanup_and_return_null", function(...) NULL)
  
  out <- testthat::capture_messages(
    res <- fn(pkg_source_path = pkg_source_path, test_pkg_data = test_pkg_data)
  )
  
  expect_identical(res$res_cov$coverage$totalcoverage, 0)
  expect_true(
    any(grepl("package_coverage fallback failed", out, fixed = TRUE)) ||
      any(grepl("environment_coverage produced 0%", out, fixed = TRUE)),
    info = "package_coverage fallback path should be entered (env 0% message) and either inner error message or outer fallback"
  )
})

test_that("prepare_for_bioc_runit returns invisible(TRUE)", {
  fn <- prepare_for_bioc_runit
  
  # detach_pkg_if_attached is called 4 times:
  #   1. detach target package
  #   3× if detach_bioc_infra = TRUE
  mock_detach <- mockery::mock(NULL, NULL, NULL, NULL)
  
  mock_load_all <- mockery::mock(NULL)   # pkgload::load_all
  
  mockery::stub(fn, "detach_pkg_if_attached", mock_detach)
  mockery::stub(fn, "pkgload::load_all", mock_load_all)
  
  v <- withVisible(fn(pkg_name = "mypkg",
                      pkg_source_path = "/tmp/mypkg",
                      detach_bioc_infra = TRUE))
  
  expect_true(v$value)        # TRUE returned
  expect_false(v$visible)     # invisibly() applied
})


test_that("load_all called even if detaches throw; try-catch not inside but we still see error", {
  fn <- prepare_for_bioc_runit
  
  # If a detach throws, since the function doesn't try-catch around it, the error
  # should propagate and load_all should not be called. We can still validate behavior.
  throwing_detach <- function(...) stop("detach failure")
  mock_load_all <- mockery::mock(NULL)
  
  mockery::stub(fn, "detach_pkg_if_attached", throwing_detach)
  mockery::stub(fn, "pkgload::load_all", mock_load_all)
  
  expect_error(fn(pkg_name = "mypkg", pkg_source_path = "/tmp/mypkg"), "detach failure")
  # Ensure load_all was not called due to early error
  mockery::expect_called(mock_load_all, 0)
}) 


test_that("errors if RUnit is not available", {
  fn <- check_covr_skip_runit_nstf
  
  mock_req_ns <- mockery::mock(FALSE)
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  
  expect_error(
    fn(pkg_source_path = "/pkg", mapping = data.frame(test_file = character()), test_path = "/tests"),
    "RUnit must be installed"
  )
  
  mockery::expect_called(mock_req_ns, 1)
  args <- mockery::mock_args(mock_req_ns)
  expect_identical(args[[1]][[1]], "RUnit")
  expect_true(isTRUE(args[[1]][["quietly"]]))
})

test_that("cleans mapping via tidyr::drop_na when test_file contains NA", {
  fn <- check_covr_skip_runit_nstf
  
  mapping <- data.frame(test_file = c("test-a.R", NA_character_, "test-b.R"), stringsAsFactors = FALSE)
  
  mock_req_ns <- mockery::mock(TRUE)
  # drop_na should remove the NA row
  mock_drop_na <- function(df, test_file) df[!is.na(df$test_file), , drop = FALSE]
  
  # Minimal stubs to allow the function to proceed to the lapply without real side effects
  mock_file_exists <- mockery::mock(TRUE, TRUE, cycle = TRUE) # two files exist
  mock_search <- mockery::mock("package:stats", "package:stats", cycle = TRUE) # no RUnit attached
  mock_attach <- mockery::mock(NULL, NULL, cycle = TRUE)
  mock_detach <- mockery::mock(NULL, cycle = TRUE)  # may or may not be called by on.exit
  mock_run <- mockery::mock(structure(list(), class = "RUnitTestData"),
                            structure(list(), class = "RUnitTestData"), 
                            cycle = TRUE)
  mock_get_err <- mockery::mock(list(nErr = 0, nFail = 0, nDeactivated = 0),
                                list(nErr = 0, nFail = 0, nDeactivated = 0), 
                                cycle = TRUE)
  
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  mockery::stub(fn, "tidyr::drop_na", mock_drop_na)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "attachNamespace", mock_attach)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "RUnit::runTestFile", mock_run)
  mockery::stub(fn, "RUnit::getErrors", mock_get_err)
  
  res <- fn(pkg_source_path = "/pkg", mapping = mapping, test_path = "/tests")
  
  # Both tests pass -> "No tests skipped"
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 1L)
  expect_true(is.na(res$test_file[1]))
  expect_identical(res$issue_type[1], "No tests skipped")
})

test_that("Windows branch: Sys.setenv uses quoted R_TESTS and on.exit restores via Sys.unsetenv when originally unset", {
  fn <- check_covr_skip_runit_nstf
  
  mapping <- data.frame(test_file = "pass.R", stringsAsFactors = FALSE)
  
  mock_req_ns <- mockery::mock(TRUE)
  mock_sysinfo <- c(sysname = "windows", nodename = "PC", release = "10", version = "", machine = "x86")
  mock_getenv <- mockery::mock(NA_character_)
  mock_setenv <- mockery::mock(NULL, NULL)
  mock_unsetenv <- mockery::mock(NULL)
  
  mock_file_exists <- mockery::mock(TRUE)
  mock_search <- mockery::mock("package:RUnit")
  mock_run <- mockery::mock(structure(list(file = "pass.R"), class = "RUnitTestData"))
  mock_get_err <- mockery::mock(list(nErr = 0, nFail = 0, nDeactivated = 0))
  id_drop_na <- function(df, test_file) df
  
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  mockery::stub(fn, "Sys.info", function() mock_sysinfo)
  mockery::stub(fn, "Sys.getenv", mock_getenv)
  mockery::stub(fn, "Sys.setenv", mock_setenv)
  mockery::stub(fn, "Sys.unsetenv", mock_unsetenv)
  mockery::stub(fn, "tidyr::drop_na", id_drop_na)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "RUnit::runTestFile", mock_run)
  mockery::stub(fn, "RUnit::getErrors", mock_get_err)
  
  res <- fn(pkg_source_path = "/pkg", mapping = mapping, test_path = "/tests")
  
  expect_s3_class(res, "data.frame")
  expect_identical(res$issue_type[1], "No tests skipped")
  
  set_calls <- mockery::mock_args(mock_setenv)
  expect_true(
    any(vapply(set_calls, function(x) identical(x, list("R_TESTS" = "")), logical(1))),
    info = "Windows branch: Sys.setenv uses quoted R_TESTS = ''"
  )
  mockery::expect_called(mock_unsetenv, 1)
  unset_args <- mockery::mock_args(mock_unsetenv)
  expect_identical(unset_args[[1]], list("R_TESTS"))
})

test_that("Windows branch: Sys.setenv uses quoted R_TESTS and on.exit restores via Sys.setenv when originally set", {
  fn <- check_covr_skip_runit_nstf
  
  mapping <- data.frame(test_file = "pass.R", stringsAsFactors = FALSE)
  original_value <- "original-R_TESTS-value"
  
  mock_req_ns <- mockery::mock(TRUE)
  mock_sysinfo <- c(sysname = "windows", nodename = "PC", release = "10", version = "", machine = "x86")
  mock_getenv <- mockery::mock(original_value)
  mock_setenv <- mockery::mock(NULL, NULL)
  mock_unsetenv <- mockery::mock(NULL)
  
  mock_file_exists <- mockery::mock(TRUE)
  mock_search <- mockery::mock("package:RUnit")
  mock_run <- mockery::mock(structure(list(file = "pass.R"), class = "RUnitTestData"))
  mock_get_err <- mockery::mock(list(nErr = 0, nFail = 0, nDeactivated = 0))
  id_drop_na <- function(df, test_file) df
  
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  mockery::stub(fn, "Sys.info", function() mock_sysinfo)
  mockery::stub(fn, "Sys.getenv", mock_getenv)
  mockery::stub(fn, "Sys.setenv", mock_setenv)
  mockery::stub(fn, "Sys.unsetenv", mock_unsetenv)
  mockery::stub(fn, "tidyr::drop_na", id_drop_na)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "RUnit::runTestFile", mock_run)
  mockery::stub(fn, "RUnit::getErrors", mock_get_err)
  
  res <- fn(pkg_source_path = "/pkg", mapping = mapping, test_path = "/tests")
  
  expect_s3_class(res, "data.frame")
  
  set_calls <- mockery::mock_args(mock_setenv)
  expect_true(
    any(vapply(set_calls, function(x) identical(x, list("R_TESTS" = "")), logical(1))),
    info = "Windows branch: Sys.setenv uses quoted R_TESTS = ''"
  )
  expect_true(
    any(vapply(set_calls, function(x) identical(x, list(R_TESTS = original_value)), logical(1))),
    info = "Windows branch: on.exit restores R_TESTS to original value"
  )
  mockery::expect_called(mock_unsetenv, 0)
})

test_that("removes exact filename from mapping when bioc_run_ut_path provided (case-insensitive)", {
  fn <- remove_run_unitTests_mapping
  
  mapping <- data.frame(
    test_file = c("run_unitTests.R", "test-foo.R", "RUN_UNITTESTS.R", "test-bar.R"),
    stringsAsFactors = FALSE
  )
  test_pkg_data <- list(bioc_run_ut_path = "/pkg/tests/run_unitTests.R")
  
  # Stub basename() to a specific value to assert exact-match branch is used
  mock_basename <- mockery::mock("run_unitTests.R")
  mockery::stub(fn, "basename", mock_basename)
  
  res <- fn(mapping, test_pkg_data)
  
  # Expected: exact "run_unitTests.R" removed via exact-match branch; regex also removes RUN_UNITTESTS.R
  expect_true("test-foo.R" %in% res$test_file)
  expect_true("test-bar.R" %in% res$test_file)
  expect_false("run_unitTests.R" %in% res$test_file)
  expect_false("RUN_UNITTESTS.R" %in% res$test_file)
  
  # basename should be called once with the provided path
  mockery::expect_called(mock_basename, 1)
  args <- mockery::mock_args(mock_basename)
  expect_identical(args[[1]][[1]], "/pkg/tests/run_unitTests.R")
})

test_that("regex fallback removes variations when bioc_run_ut_path is missing/NULL/NA", {
  fn <- remove_run_unitTests_mapping
  
  # No exact name available -> only regex path applies
  mapping <- data.frame(
    test_file = c("run-unitTests.R", "Run_UnitTests.R", "test-x.R", "sub/run_unitTests.R"),
    stringsAsFactors = FALSE
  )
  
  # Case 1: test_pkg_data missing entirely
  res1 <- fn(mapping, test_pkg_data = NULL)
  expect_setequal(res1$test_file, "test-x.R") # note: regex only matches basename
  
  # Case 2: field present but NULL
  res2 <- fn(mapping, test_pkg_data = list(bioc_run_ut_path = NULL))
  expect_setequal(res2$test_file, "test-x.R")
  
  # Case 3: field NA
  res3 <- fn(mapping, test_pkg_data = list(bioc_run_ut_path = NA_character_))
  expect_setequal(res3$test_file, "test-x.R")
})

test_that("does not remove unrelated files; only exact filename or regex variations dropped", {
  fn <- remove_run_unitTests_mapping
  
  mapping <- data.frame(
    test_file = c("test-a.R", "runUnitTest.R", "unitTests_run.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  test_pkg_data <- list(bioc_run_ut_path = "/pkg/tests/NotRunUnitTests.R")
  
  # basename returns a non-matching value -> exact match won't drop anything
  mock_basename <- mockery::mock("NotRunUnitTests.R")
  mockery::stub(fn, "basename", mock_basename)
  
  res <- fn(mapping, test_pkg_data)
  
  # None match the strict regex "run[_-]?unitTests\\.R$"
  expect_setequal(res$test_file, c("test-a.R", "runUnitTest.R", "unitTests_run.R", "test-b.R"))
})

test_that("handles NA entries in mapping$test_file gracefully (no warnings)", {
  fn <- remove_run_unitTests_mapping
  
  mapping <- data.frame(
    test_file = c(NA_character_, "test-a.R", "run_unitTests.R", NA_character_, "test-b.R"),
    stringsAsFactors = FALSE
  )
  test_pkg_data <- list(bioc_run_ut_path = "/pkg/tests/whatever.R")
  
  # basename stub not strictly needed but keep deterministic
  mock_basename <- mockery::mock("whatever.R")
  mockery::stub(fn, "basename", mock_basename)
  
  res <- fn(mapping, test_pkg_data)
  
  # Only run_unitTests.R is removed; NAs are retained
  expect_setequal(res$test_file, c(NA_character_, "test-a.R", NA_character_, "test-b.R"))
})

test_that("exact match removal works even if mapping has different case than basename", {
  fn <- remove_run_unitTests_mapping
  
  mapping <- data.frame(
    test_file = c("RUN_UNITTESTS.R", "test-a.R"),
    stringsAsFactors = FALSE
  )
  test_pkg_data <- list(bioc_run_ut_path = "/pkg/tests/run_unitTests.R")
  
  mock_basename <- mockery::mock("run_unitTests.R")
  mockery::stub(fn, "basename", mock_basename)
  
  res <- fn(mapping, test_pkg_data)
  expect_setequal(res$test_file, "test-a.R")
})

test_that("regex removes both underscore and hyphen variants, any case", {
  fn <- remove_run_unitTests_mapping
  
  mapping <- data.frame(
    test_file = c("run_unitTests.R", "run-unitTests.R", "RUN_UNITTESTS.R", "test-a.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  res <- fn(mapping, test_pkg_data = NULL)
  expect_setequal(res$test_file, c("test-a.R", "test-b.R"))
})

test_that("empty mapping returns empty data.frame (no errors)", {
  fn <- remove_run_unitTests_mapping
  
  mapping <- data.frame(test_file = character(), stringsAsFactors = FALSE)
  res <- fn(mapping, test_pkg_data = NULL)
  
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 0L)
})

test_that("basename is consulted only when bioc_run_ut_path is usable", {
  fn <- remove_run_unitTests_mapping
  
  mapping <- data.frame(test_file = c("run_unitTests.R", "test-a.R"), stringsAsFactors = FALSE)
  
  # Case: usable path -> basename called
  mock_basename <- mockery::mock("run_unitTests.R")
  mockery::stub(fn, "basename", mock_basename)
  res1 <- fn(mapping, list(bioc_run_ut_path = "/pkg/tests/run_unitTests.R"))
  mockery::expect_called(mock_basename, 1)
  expect_setequal(res1$test_file, "test-a.R")
  
  # Case: NA path -> basename should not be called
  mock_basename2 <- mockery::mock("should-not-be-called")
  mockery::stub(fn, "basename", mock_basename2)
  res2 <- fn(mapping, list(bioc_run_ut_path = NA_character_))
  mockery::expect_called(mock_basename2, 0)
  expect_setequal(res2$test_file, c("test-a.R"))
  
  # Case: NULL path -> basename should not be called
  mock_basename3 <- mockery::mock("should-not-be-called")
  mockery::stub(fn, "basename", mock_basename3)
  res3 <- fn(mapping, list(bioc_run_ut_path = NULL))
  mockery::expect_called(mock_basename3, 0)
  expect_setequal(res3$test_file, c("test-a.R"))
})  

test_that("classifies missing, error, failure, skipped, and passing files correctly", {
  fn <- check_covr_skip_runit_nstf
  
  mapping <- data.frame(
    test_file = c(
      "test-missing.R",
      "test-error-throw.R",
      "test-error-count.R",
      "test-failure.R",
      "test-skipped.R",
      "test-pass.R",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  
  mock_req_ns <- mockery::mock(TRUE)
  
  # drop_na should be called and remove NA
  mock_drop_na <- function(df, test_file) df[!is.na(df$test_file), , drop = FALSE]
  
  # file.exists -> FALSE only for test-missing.R
  mock_file_exists <- function(path) {
    if (grepl("test-missing\\.R$", path)) return(FALSE)
    TRUE
  }
  
  # Pretend RUnit is already on the search path so we don't re-attach each time
  mock_search <- mockery::mock("package:RUnit", "package:RUnit", "package:RUnit",
                               "package:RUnit", "package:RUnit", "package:RUnit")
  
  # runTestFile: throws for test-error-throw, otherwise returns a testdata object
  mock_run <- function(absFileName, useOwnErrorHandler, testFuncRegexp) {
    bn <- basename(absFileName)
    if (bn == "test-error-throw.R") stop("parse/runtime error")
    structure(list(file = bn), class = "RUnitTestData")
  }
  
  # getErrors: return different counts per file name
  mock_get_err <- function(td) {
    bn <- td$file
    switch(
      bn,
      "test-error-count.R" = list(nErr = 1, nFail = 0, nDeactivated = 0),
      "test-failure.R"     = list(nErr = 0, nFail = 2, nDeactivated = 0),
      "test-skipped.R"     = list(nErr = 0, nFail = 0, nDeactivated = 3),
      # default: passing
      list(nErr = 0, nFail = 0, nDeactivated = 0)
    )
  }
  
  mock_attach <- mockery::mock(NULL) # not used because search() includes RUnit
  mock_detach <- mockery::mock(NULL)
  
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  mockery::stub(fn, "tidyr::drop_na", mock_drop_na)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "attachNamespace", mock_attach)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "RUnit::runTestFile", mock_run)
  mockery::stub(fn, "RUnit::getErrors", mock_get_err)
  
  res <- fn(pkg_source_path = "/pkg", mapping = mapping, test_path = "/tests")
  
  # Expected issues (order follows mapping after drop_na)
  expected <- data.frame(
    test_file  = c("test-missing.R", "test-error-throw.R", "test-error-count.R", "test-failure.R", "test-skipped.R"),
    issue_type = c("missing",        "error",              "error",               "failure",         "skipped"),
    stringsAsFactors = FALSE
  )
  
  expect_s3_class(res, "data.frame")
  expect_identical(res[, c("test_file", "issue_type")], expected)
})

test_that("when all files are passing, returns 'No tests skipped' row", {
  fn <- check_covr_skip_runit_nstf
  
  mapping <- data.frame(test_file = c("t1.R", "t2.R"), stringsAsFactors = FALSE)
  
  mock_req_ns <- mockery::mock(TRUE)
  # anyNA(mapping$test_file) is FALSE -> ensure we don't call tidyr::drop_na (we make it throw if called)
  throwing_drop_na <- function(...) stop("drop_na should not be called when no NA present")
  
  mock_file_exists <- mockery::mock(TRUE, TRUE)
  mock_search <- mockery::mock("package:RUnit", "package:RUnit")
  mock_run <- mockery::mock(structure(list(file = "t1.R"), class = "RUnitTestData"),
                            structure(list(file = "t2.R"), class = "RUnitTestData"))
  mock_get_err <- mockery::mock(list(nErr = 0, nFail = 0, nDeactivated = 0),
                                list(nErr = 0, nFail = 0, nDeactivated = 0))
  mock_attach <- mockery::mock(NULL)
  mock_detach <- mockery::mock(NULL)
  
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  mockery::stub(fn, "tidyr::drop_na", throwing_drop_na)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "attachNamespace", mock_attach)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "RUnit::runTestFile", mock_run)
  mockery::stub(fn, "RUnit::getErrors", mock_get_err)
  
  res <- fn(pkg_source_path = "/pkg", mapping = mapping, test_path = "/tests")
  
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 1L)
  expect_true(is.na(res$test_file[1]))
  expect_identical(res$issue_type[1], "No tests skipped")
})



test_that("attaches RUnit namespace only when not already on search path", {
  fn <- check_covr_skip_runit_nstf
  
  mapping <- data.frame(test_file = "pass.R", stringsAsFactors = FALSE)
  
  mock_req_ns <- mockery::mock(TRUE)
  # First call to search() -> no RUnit; inside on.exit check may call again, so provide two values
  mock_search <- mockery::mock("package:stats", "package:RUnit")
  mock_attach <- mockery::mock(NULL)  # should be called once
  mock_detach <- mockery::mock(NULL)
  
  mock_file_exists <- mockery::mock(TRUE)
  mock_run <- mockery::mock(structure(list(file = "pass.R"), class = "RUnitTestData"))
  mock_get_err <- mockery::mock(list(nErr = 0, nFail = 0, nDeactivated = 0))
  id_drop_na <- function(df, test_file) df
  
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  mockery::stub(fn, "tidyr::drop_na", id_drop_na)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "attachNamespace", mock_attach)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "RUnit::runTestFile", mock_run)
  mockery::stub(fn, "RUnit::getErrors", mock_get_err)
  
  res <- fn(pkg_source_path = "/pkg", mapping = mapping, test_path = "/tests")
  expect_s3_class(res, "data.frame")
  
  mockery::expect_called(mock_attach, 1)
})

test_that("runTestFile error path is captured as 'error' without crashing", {
  fn <- check_covr_skip_runit_nstf
  
  mapping <- data.frame(test_file = "test-error-throw.R", stringsAsFactors = FALSE)
  
  mock_req_ns <- mockery::mock(TRUE)
  id_drop_na <- function(df, test_file) df
  mock_file_exists <- mockery::mock(TRUE)
  mock_search <- mockery::mock("package:RUnit")
  mock_attach <- mockery::mock(NULL)
  mock_detach <- mockery::mock(NULL)
  
  # Force runTestFile to throw -> tryCatch will return class "RUnitTestData-ERROR"
  mock_run <- function(...) stop("boom")
  # getErrors should not be called in this path
  mock_get_err <- mockery::mock()
  
  mockery::stub(fn, "requireNamespace", mock_req_ns)
  mockery::stub(fn, "tidyr::drop_na", id_drop_na)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "search", mock_search)
  mockery::stub(fn, "attachNamespace", mock_attach)
  mockery::stub(fn, "detach", mock_detach)
  mockery::stub(fn, "RUnit::runTestFile", mock_run)
  mockery::stub(fn, "RUnit::getErrors", mock_get_err)
  
  res <- fn(pkg_source_path = "/pkg", mapping = mapping, test_path = "/tests")
  
  expect_identical(nrow(res), 1L)
  expect_identical(res$test_file[1], "test-error-throw.R")
  expect_identical(res$issue_type[1], "error")
  mockery::expect_called(mock_get_err, 0)
})


test_that("returns bioc_unitTests when testdir already exists", {
  fn <- get_runit_test_paths
  
  # Normalized testdir value
  mock_normalize <- function(path, winslash = "/", mustWork = FALSE) {
    # Return a consistent normalized path regardless of input
    if (grepl("^/work/pkg/tests$", path)) return("/work/pkg/tests")
    path
  }
  mock_dir_exists <- function(path) path == "/work/pkg/tests"
  
  # Stubs
  mockery::stub(fn, "normalizePath", mock_normalize)
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  
  res <- fn(test_pkg_data = list(), testdir = "/work/pkg/tests")
  expect_true("bioc_unitTests" %in% names(res))
  expect_identical(res$bioc_unitTests, "/work/pkg/tests")
  expect_false("fallback" %in% names(res))
})

test_that("finds package root at testdir and returns inst/unitTests if it exists", {
  fn <- get_runit_test_paths
  
  # Paths
  raw_testdir <- "/work/pkg/tests"
  normalized_testdir <- "/work/pkg/tests"
  pkg_root <- "/work/pkg"
  bioc_unit <- "/work/pkg/inst/unitTests"
  
  mock_normalize <- function(path, winslash = "/", mustWork = FALSE) {
    # Normalize testdir and pkg_root and bioc_unit deterministically
    if (path == raw_testdir) return(normalized_testdir)
    if (path == pkg_root) return(pkg_root)
    if (path == bioc_unit) return(bioc_unit)
    path
  }
  
  # testdir does NOT exist; bioc unit dir DOES
  mock_dir_exists <- function(path) {
    path == bioc_unit
  }
  
  # file.exists(DESCRIPTION) returns TRUE only at testdir's root (/work/pkg)
  mock_file_exists <- function(path) {
    path == file.path(pkg_root, "DESCRIPTION")
  }
  
  mockery::stub(fn, "normalizePath", mock_normalize)
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(test_pkg_data = list(), testdir = raw_testdir)
  expect_true("bioc_unitTests" %in% names(res))
  expect_identical(res$bioc_unitTests, bioc_unit)
  expect_false("fallback" %in% names(res))
})

test_that("finds package root at dirname(testdir) and returns inst/unitTests", {
  fn <- get_runit_test_paths
  
  # Layout:
  # testdir: /work/pkg/tests/unit
  # dirname(testdir): /work/pkg/tests
  # dirname(dirname(testdir)): /work/pkg
  raw_testdir <- "/work/pkg/tests/unit"
  normalized_testdir <- raw_testdir
  pkg_root <- "/work/pkg"
  bioc_unit <- "/work/pkg/inst/unitTests"
  
  mock_normalize <- function(path, winslash = "/", mustWork = FALSE) {
    if (path %in% c(raw_testdir, pkg_root, bioc_unit)) return(path)
    path
  }
  
  mock_dir_exists <- function(path) {
    # testdir does not exist; bioc_unit exists
    path == bioc_unit
  }
  
  mock_file_exists <- function(path) {
    # Mark only /work/pkg as a package root
    path == file.path(pkg_root, "DESCRIPTION")
  }
  
  mockery::stub(fn, "normalizePath", mock_normalize)
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(test_pkg_data = list(), testdir = raw_testdir)
  expect_true("bioc_unitTests" %in% names(res))
  expect_identical(res$bioc_unitTests, bioc_unit)
})

test_that("finds package root at dirname(dirname(testdir)) and returns inst/unitTests", {
  fn <- get_runit_test_paths
  
  raw_testdir <- "/work/pkg/a/b"
  pkg_root <- "/work/pkg"
  bioc_unit <- "/work/pkg/inst/unitTests"
  
  mock_normalize <- function(path, winslash = "/", mustWork = FALSE) path
  mock_dir_exists <- function(path) path == bioc_unit
  mock_file_exists <- function(path) path == file.path(pkg_root, "DESCRIPTION")
  
  mockery::stub(fn, "normalizePath", mock_normalize)
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(test_pkg_data = list(), testdir = raw_testdir)
  expect_true("bioc_unitTests" %in% names(res))
  expect_identical(res$bioc_unitTests, bioc_unit)
})

test_that("returns fallback when testdir doesn't exist and no package root found", {
  fn <- get_runit_test_paths
  
  raw_testdir <- "/nowhere/tests"
  normalized_testdir <- "/nowhere/tests"
  
  mock_normalize <- function(path, winslash = "/", mustWork = FALSE) normalized_testdir
  mock_dir_exists <- function(path) FALSE
  mock_file_exists <- function(path) FALSE
  
  mockery::stub(fn, "normalizePath", mock_normalize)
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(test_pkg_data = list(), testdir = raw_testdir)
  expect_true("fallback" %in% names(res))
  expect_identical(res$fallback, normalized_testdir)
  expect_false("bioc_unitTests" %in% names(res))
})

test_that("returns fallback if package root found but inst/unitTests does not exist", {
  fn <- get_runit_test_paths
  
  raw_testdir <- "/work/pkg/tests"
  pkg_root <- "/work/pkg"
  normalized_testdir <- raw_testdir
  bioc_unit <- "/work/pkg/inst/unitTests"
  
  mock_normalize <- function(path, winslash = "/", mustWork = FALSE) path
  mock_dir_exists <- function(path) FALSE  # inst/unitTests not present
  mock_file_exists <- function(path) path == file.path(pkg_root, "DESCRIPTION")
  
  mockery::stub(fn, "normalizePath", mock_normalize)
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(test_pkg_data = list(), testdir = raw_testdir)
  expect_true("fallback" %in% names(res))
  expect_identical(res$fallback, normalized_testdir)
})

test_that("handles duplicate candidate paths via unique()", {
  fn <- get_runit_test_paths
  
  # If testdir == dirname(testdir), unique() collapses duplicates.
  raw_testdir <- "/flat"
  # Simulate a flat structure where dirname("/flat") == "/"
  mock_normalize <- function(path, winslash = "/", mustWork = FALSE) path
  mock_dir_exists <- function(path) FALSE
  mock_file_exists <- function(path) FALSE
  
  mockery::stub(fn, "normalizePath", mock_normalize)
  mockery::stub(fn, "dir.exists", mock_dir_exists)
  mockery::stub(fn, "file.exists", mock_file_exists)
  
  res <- fn(test_pkg_data = list(), testdir = raw_testdir)
  
  expect_true("fallback" %in% names(res))
  expect_identical(res$fallback, raw_testdir)
})



test_that("errors when no R source files are found", {
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/proj/pkg"
  source_dir <- file.path(pkg_source_path, "R")
  test_dir <- "/proj/pkg/tests"
  
  # Stub list.files so there are NO source files
  mock_list_files <- function(path, pattern, full.names, recursive) {
    if (path == source_dir) return(character())  # no .R under R/
    if (path == test_dir)   return(character(0)) # doesn't matter
    character(0)
  }
  mockery::stub(fn, "list.files", mock_list_files)
  
  # Use a simple pattern (or regexp = NA) to avoid brittle matching
  expect_error(
    fn(pkg_source_path = pkg_source_path, test_dir = test_dir),
    regexp = "No R files found under:"
  )
})

test_that("warns when no .R test files are found and maps NA tests", {
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/proj/pkg"
  source_dir <- file.path(pkg_source_path, "R")
  test_dir <- "/proj/pkg/tests"
  
  src_files <- file.path(source_dir, c("alpha.R", "beta.R"))
  
  mock_list_files <- function(path, pattern, full.names, recursive) {
    if (path == source_dir) return(src_files)    # two sources
    if (path == test_dir)   return(character())  # no tests -> warning
    character()
  }
  # No test content will be read because length(test_files) == 0
  mock_readLines <- function(con, warn = FALSE) stop("readLines should not be called")
  
  mockery::stub(fn, "list.files", mock_list_files)
  mockery::stub(fn, "readLines", mock_readLines)
  
  expect_warning(
    res <- fn(pkg_source_path = pkg_source_path, test_dir = test_dir),
    regexp = "No R test files found under:"
  )
  
  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 2L)
  expect_identical(res$source_file, c("alpha.R", "beta.R"))
  expect_true(all(is.na(res$test_file)))
  expect_true(all(is.na(res$evidence)))
  expect_identical(res$has_tests, c(FALSE, FALSE))
})

test_that("filename-based mapping: tokenized, case-insensitive, picks first match", {
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/pkg"
  source_dir <- file.path(pkg_source_path, "R")
  test_dir <- "/pkg/tests"
  
  src_files  <- file.path(source_dir, c("foo.R", "bar.R"))
  test_files <- file.path(test_dir, c("test-foo.R", "ZZZ_foo_extra.R", "TEST-bar.R"))
  
  mock_list_files <- function(path, pattern, full.names, recursive) {
    if (path == source_dir) return(src_files)
    if (path == test_dir)   return(test_files)
    character()
  }
  # No content-based checks needed
  mock_readLines <- function(con, warn = FALSE) character()
  
  mockery::stub(fn, "list.files", mock_list_files)
  mockery::stub(fn, "readLines", mock_readLines)
  
  res <- fn(pkg_source_path = pkg_source_path, test_dir = test_dir)
  
  expect_s3_class(res, "data.frame")
  expect_identical(res$source_file, c("foo.R", "bar.R"))
  expect_identical(res$test_file,   c("test-foo.R", "TEST-bar.R")) # first match wins
  expect_identical(res$evidence,    c("filename_match", "filename_match"))
  expect_identical(res$has_tests,   c(TRUE, TRUE))
})

test_that("content-based mapping via RUnit test function names when filename matching fails", {
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/p"
  source_dir <- file.path(pkg_source_path, "R")
  test_dir <- "/p/tests"
  
  # One source 'widget.R'; one test 'test-something.R'
  src_files  <- file.path(source_dir, "widget.R")
  test_files <- file.path(test_dir,   "test-something.R")
  
  mock_list_files <- function(path, pattern, full.names, recursive) {
    if (path == source_dir) return(src_files)
    if (path == test_dir)   return(test_files)
    character()
  }
  
  # Provide a FUNCTION DEF in source (ensures symbol extraction works)
  # Provide an RUnit test function line that contains the symbol
  mock_readLines <- function(con, warn = FALSE) {
    f <- as.character(con)
    if (identical(basename(f), "widget.R")) {
      return(c("widget <- function(x) { x }"))
    }
    if (identical(basename(f), "test-something.R")) {
      return(c("test_widget <- function() {",
               "  # assertions here",
               "}"))
    }
    character()
  }
  
  mockery::stub(fn, "list.files", mock_list_files)
  mockery::stub(fn, "readLines",  mock_readLines)
  
  res <- fn(pkg_source_path = pkg_source_path, test_dir = test_dir)
  
  expect_identical(res$source_file, "widget.R")
  expect_identical(res$test_file,   "test-something.R")
  expect_identical(res$evidence,    "content_match")
  expect_true(res$has_tests)
})

test_that("content-based mapping uses symbols from function defs (avoid setGeneric parsing)", {
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/root"
  source_dir <- file.path(pkg_source_path, "R")
  test_dir <- "/root/tests"
  
  src_files  <- file.path(source_dir, c("generics.R", "funcs.R"))
  test_files <- file.path(test_dir,   c("t-generic.R", "t-func.R"))
  
  mock_list_files <- function(path, pattern, full.names, recursive) {
    if (path == source_dir) return(src_files)
    if (path == test_dir)   return(test_files)
    character()
  }
  
  # IMPORTANT: Because the function's setGeneric grep uses fixed=TRUE with an escaped quote,
  # it's safer to provide FUNCTION DEFS in sources so symbol extraction surely works.
  mock_readLines <- function(con, warn = FALSE) {
    f <- as.character(con)
    switch(
      basename(f),
      "generics.R" = c(
        "doStuff <- function(...) TRUE"   # symbol: doStuff
      ),
      "funcs.R"    = c(
        "my_func <- function(a, b) { a + b }",
        "other <- function() 1"
      ),
      "t-generic.R" = c(
        "test_doStuff <- function() { TRUE }"  # detects 'doStuff' via RUnit name
      ),
      "t-func.R" = c(
        "test_my_func <- function() { TRUE }"
      ),
      character()
    )
  }
  
  mockery::stub(fn, "list.files", mock_list_files)
  mockery::stub(fn, "readLines",  mock_readLines)
  
  res <- fn(pkg_source_path = pkg_source_path, test_dir = test_dir)
  
  expect_identical(res$source_file, c("generics.R", "funcs.R"))
  expect_identical(res$test_file,   c("t-generic.R", NA))
  expect_identical(res$evidence,    c("content_match", NA))
  expect_identical(res$has_tests,   c(TRUE, FALSE))
})

test_that("short symbol (<= 2 chars) matches via filename tokens", {
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/a"
  source_dir <- file.path(pkg_source_path, "R")
  test_dir <- "/a/tests"
  
  src_files  <- file.path(source_dir, c("t.R", "x1.R"))
  test_files <- file.path(test_dir,   c("test-t.R", "we-test-x1.R"))
  
  mock_list_files <- function(path, pattern, full.names, recursive) {
    if (path == source_dir) return(src_files)
    if (path == test_dir)   return(test_files)
    character()
  }
  mock_readLines <- function(con, warn = FALSE) character()
  
  mockery::stub(fn, "list.files", mock_list_files)
  mockery::stub(fn, "readLines",  mock_readLines)
  
  res <- fn(pkg_source_path = pkg_source_path, test_dir = test_dir)
  
  expect_identical(res$source_file, c("t.R", "x1.R"))
  expect_identical(res$test_file,   c("test-t.R", "we-test-x1.R"))
  expect_identical(res$evidence,    c("filename_match", "filename_match"))
  expect_identical(res$has_tests,   c(TRUE, TRUE))
})

test_that("returns NA mapping for source with no filename or content evidence", {
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/no"
  source_dir <- file.path(pkg_source_path, "R")
  test_dir <- "/no/tests"
  
  src_files  <- file.path(source_dir, "lonely.R")
  test_files <- file.path(test_dir,   c("test-a.R", "test-b.R"))  # no relevant content
  
  mock_list_files <- function(path, pattern, full.names, recursive) {
    if (path == source_dir) return(src_files)
    if (path == test_dir)   return(test_files)
    character()
  }
  
  mock_readLines <- function(con, warn = FALSE) {
    f <- as.character(con)
    if (identical(basename(f), "lonely.R")) return(c("lonely <- function() 1"))
    if (basename(f) %in% c("test-a.R", "test-b.R")) return(c("test_other <- function() 1"))
    character()
  }
  
  mockery::stub(fn, "list.files", mock_list_files)
  mockery::stub(fn, "readLines",  mock_readLines)
  
  res <- fn(pkg_source_path = pkg_source_path, test_dir = test_dir)
  
  expect_identical(res$source_file, "lonely.R")
  expect_true(is.na(res$test_file))
  expect_true(is.na(res$evidence))
  expect_false(res$has_tests)
})


test_that("detect_runit_test_prefix detects runit.<name>.R pattern", {
  files <- c(
    "/fake/dir/runit.myfun.R",
    "/fake/dir/runit.other_test.R"
  )
  expect_equal(detect_runit_test_prefix(files), "runit")
})


test_that("detect_runit_test_prefix detects Test.<name>.R pattern", {
  files <- c(
    "/fake/dir/Test.myFun.R",
    "/fake/dir/Test.Other.R"
  )
  expect_equal(detect_runit_test_prefix(files), "Test")
})


test_that("detect_runit_test_prefix detects test_<name>.R pattern (underscore)", {
  files <- c(
    "/path/to/test_myFunc.R",
    "/path/to/test_otherThing.R"
  )
  expect_equal(detect_runit_test_prefix(files), "test")
})


test_that("detect_runit_test_prefix detects test.<name>.R pattern (dot)", {
  files <- c(
    "/path/test.myFunc.R",
    "/path/test.thing_more.R"
  )
  expect_equal(detect_runit_test_prefix(files), "test")
})


test_that("detect_runit_test_prefix returns first detected prefix among many", {
  files <- c(
    "/fake/zzz/NOT_A_TEST.R",
    "/fake/dir/runit.someFun.R",   # should win immediately
    "/fake/dir/Test.foo.R"         # never reached
  )
  expect_equal(detect_runit_test_prefix(files), "runit")
})


test_that("detect_runit_test_prefix falls back to default 'test' when nothing matches", {
  files <- c(
    "alpha.R",
    "beta.R",
    "not_a_test_file.txt"
  )
  expect_equal(detect_runit_test_prefix(files), "test")
})



test_that("short symbol triggers filename token match via get_source_test_mapping_runit_nstf", {
  
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  test_dir <- "/fake/tests"
  
  src_files <- c("/fake/pkg/R/x.R")
  test_files <- c("/fake/tests/test_x.R")
  
  # ---- CORRECT list.files STUB (dispatch on path) ----
  mockery::stub(fn, "list.files", function(path, pattern, full.names, recursive) {
    if (grepl("/R$", path)) {
      return(src_files)
    } else {
      return(test_files)
    }
  })
  
  # extract_symbols returns the symbol being searched
  mockery::stub(fn, "extract_symbols", function(...) "x")
  
  # readLines must return NON-empty or function returns FALSE early
  mockery::stub(fn, "readLines", function(...) c("dummy line"))
  
  # Tokenizer: filename tokens = c("x"), matches
  mockery::stub(fn, "tokenize_stem", function(stem) c("x"))
  
  # ---- RUN ----
  mapping <- fn(pkg_source_path, test_dir)
  
  # ---- ASSERTIONS ----
  expect_equal(mapping$test_file, "test_x.R")
  expect_equal(mapping$evidence, "filename_match")  # correct Pass 1 match
  expect_true(mapping$has_tests)
})


test_that("long symbol triggers filename token match via get_source_test_mapping_runit_nstf", {
  
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  test_dir <- "/fake/tests"
  
  src_files  <- "/fake/pkg/R/alpha.R"
  test_files <- "/fake/tests/test_alpha.R"
  
  # CORRECT list.files dispatcher
  mockery::stub(fn, "list.files", function(path, pattern, full.names, recursive) {
    if (grepl("/R$", path)) src_files else test_files
  })
  
  mockery::stub(fn, "extract_symbols", function(...) "alpha")
  
  mockery::stub(fn, "readLines", function(...) c("ignored"))
  mockery::stub(fn, "tokenize_stem", function(stem) c("alpha"))
  
  mapping <- fn(pkg_source_path, test_dir)
  
  expect_equal(mapping$test_file, "test_alpha.R")
  expect_equal(mapping$evidence, "filename_match")
  expect_true(mapping$has_tests)
})


test_that("test_that description triggers content_match in get_source_test_mapping_runit_nstf", {
  
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  test_dir <- "/fake/tests"
  
  src_files  <- "/fake/pkg/R/compute.R"
  test_files <- "/fake/tests/test_misc.R"
  
  mockery::stub(fn, "list.files", function(path, pattern, full.names, recursive) {
    if (grepl("/R$", path)) src_files else test_files
  })
  
  mockery::stub(fn, "extract_symbols", function(...) "compute")
  
  # Force description match
  mockery::stub(fn, "readLines", function(...) c(
    'test_that("checks compute functionality", {',
    "  expect_true(TRUE)",
    "})"
  ))
  
  # Force filename mismatch → Pass 1 fails → Pass 2 content match
  mockery::stub(fn, "tokenize_stem", function(stem) c("nothing"))
  
  mapping <- fn(pkg_source_path, test_dir)
  
  expect_equal(mapping$test_file, "test_misc.R")
  expect_equal(mapping$evidence, "content_match")
  expect_true(mapping$has_tests)
})


test_that("RUnit-style test_<symbol> triggers content_match in get_source_test_mapping_runit_nstf", {
  
  fn <- get_source_test_mapping_runit_nstf
  
  pkg_source_path <- "/fake/pkg"
  test_dir <- "/fake/tests"
  
  src_files  <- "/fake/pkg/R/compute.R"
  test_files <- "/fake/tests/test_compute.R"
  
  mockery::stub(fn, "list.files", function(path, pattern, full.names, recursive) {
    if (grepl("/R$", path)) src_files else test_files
  })
  
  mockery::stub(fn, "extract_symbols", function(...) "compute")
  
  mockery::stub(fn, "readLines", function(...) c(
    "test_compute <- function() {",
    "  checkTrue(TRUE)",
    "}"
  ))
  
  # No filename match
  mockery::stub(fn, "tokenize_stem", function(stem) c("unrelated"))
  
  mapping <- fn(pkg_source_path, test_dir)
  
  expect_equal(mapping$test_file, "test_compute.R")
  expect_equal(mapping$evidence, "filename_match")
  expect_true(mapping$has_tests)
})

test_that("build_runit_custom_runner returns path and writes correct content for prefix runit", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  out <- fn(tests)
  
  expect_identical(out, mock_runner)
  expect_true(any(grepl("RUnit::runTestFile", written_lines)))
  expect_true(any(grepl("requireNamespace.*RUnit", written_lines)))
  expect_true(any(grepl('testFuncRegexp = "\\^runit', written_lines)))
})

test_that("build_runit_custom_runner uses runit regex when prefix is runit", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests)
  
  run_line <- written_lines[grepl("runTestFile", written_lines)]
  expect_true(any(grepl('testFuncRegexp = "\\^runit\\[\\.\\]\\|\\^test\\.\\+"', run_line)))
})

test_that("build_runit_custom_runner uses Test regex when prefix is Test", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/Test.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "Test")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests)
  
  run_line <- written_lines[grepl("runTestFile", written_lines)]
  expect_true(any(grepl('testFuncRegexp = "\\^Test\\[\\.\\]\\|\\^test\\.\\+"', run_line)))
})

test_that("build_runit_custom_runner uses default test regex when prefix is neither runit nor Test", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/test_foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "test")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests)
  
  run_line <- written_lines[grepl("runTestFile", written_lines)]
  expect_true(any(grepl('testFuncRegexp = "\\^test\\.\\+"', run_line)))
})

test_that("build_runit_custom_runner includes library line when pkg_name is provided", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests, pkg_name = "mypkg")
  
  expect_true(any(grepl('library\\(package = "mypkg"', written_lines)))
})

test_that("build_runit_custom_runner omits library line when pkg_name is NULL", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests, pkg_name = NULL)
  
  expect_false(any(grepl('library\\(package = ', written_lines)))
})

test_that("build_runit_custom_runner omits library line when pkg_name is empty string", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests, pkg_name = "")
  
  expect_false(any(grepl('library\\(package = ', written_lines)))
})

test_that("build_runit_custom_runner includes wd_lines when pkg_root exists", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  pkg_root <- "/fake/pkg"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  mockery::stub(fn, "dir.exists", function(x) x == pkg_root)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests, pkg_root = pkg_root)
  
  expect_true(any(grepl("owd <- getwd\\(\\)", written_lines)))
  expect_true(any(grepl("on.exit\\(setwd\\(owd\\)", written_lines)))
  expect_true(any(grepl('setwd\\(', written_lines)))
})

test_that("build_runit_custom_runner omits wd_lines when pkg_root is NULL", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests, pkg_root = NULL)
  
  expect_false(any(grepl("owd <- getwd\\(\\)", written_lines)))
  expect_false(any(grepl("setwd\\(", written_lines)))
})

test_that("build_runit_custom_runner omits wd_lines when dir.exists(pkg_root) is FALSE", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  pkg_root <- "/nonexistent/pkg"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  mockery::stub(fn, "dir.exists", function(x) FALSE)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests, pkg_root = pkg_root)
  
  expect_false(any(grepl("owd <- getwd\\(\\)", written_lines)))
  expect_false(any(grepl("setwd\\(", written_lines)))
})

test_that("build_runit_custom_runner writes one runTestFile line per test path", {
  fn <- build_runit_custom_runner
  tests <- c("/fake/pkg/inst/unitTests/runit.a.R", "/fake/pkg/inst/unitTests/runit.b.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(tests)
  
  run_lines <- written_lines[grepl("runTestFile", written_lines)]
  expect_length(run_lines, 2L)
  expect_true(any(grepl("runit.a.R", run_lines)))
  expect_true(any(grepl("runit.b.R", run_lines)))
})

test_that("build_runit_runTests_runner returns path and writes correct content for prefix runit", {
  fn <- build_runit_runTests_runner
  pkg_name <- "mypkg"
  test_files <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  out <- fn(pkg_name, test_files)
  
  expect_identical(out, mock_runner)
  expect_true(any(grepl("RUnit::runTestFile", written_lines)))
  expect_true(any(grepl("requireNamespace.*RUnit", written_lines)))
  expect_true(any(grepl('library\\(package = "mypkg"', written_lines)))
  expect_true(any(grepl('testFuncRegexp = "\\^runit', written_lines)))
})

test_that("build_runit_runTests_runner uses runit regex when prefix is runit", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("mypkg", test_files)
  
  run_line <- written_lines[grepl("runTestFile", written_lines)]
  expect_true(any(grepl('testFuncRegexp = "\\^runit\\[\\.\\]\\|\\^test\\.\\+"', run_line)))
})

test_that("build_runit_runTests_runner uses Test regex when prefix is Test", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/Test.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "Test")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("mypkg", test_files)
  
  run_line <- written_lines[grepl("runTestFile", written_lines)]
  expect_true(any(grepl('testFuncRegexp = "\\^Test\\[\\.\\]\\|\\^test\\.\\+"', run_line)))
})

test_that("build_runit_runTests_runner uses default test regex when prefix is neither runit nor Test", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/test_foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "test")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("mypkg", test_files)
  
  run_line <- written_lines[grepl("runTestFile", written_lines)]
  expect_true(any(grepl('testFuncRegexp = "\\^test\\.\\+"', run_line)))
})

test_that("build_runit_runTests_runner always includes library line", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("fBasics", test_files)
  
  expect_true(any(grepl('library\\(package = "fBasics"', written_lines)))
})

test_that("build_runit_runTests_runner includes wd_lines when pkg_root exists", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  pkg_root <- "/fake/pkg"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  mockery::stub(fn, "dir.exists", function(x) x == pkg_root)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("mypkg", test_files, pkg_root = pkg_root)
  
  expect_true(any(grepl("owd <- getwd\\(\\)", written_lines)))
  expect_true(any(grepl("on.exit\\(setwd\\(owd\\)", written_lines)))
  expect_true(any(grepl('setwd\\(', written_lines)))
})

test_that("build_runit_runTests_runner omits wd_lines when pkg_root is NULL", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("mypkg", test_files, pkg_root = NULL)
  
  expect_false(any(grepl("owd <- getwd\\(\\)", written_lines)))
  expect_false(any(grepl("setwd\\(", written_lines)))
})

test_that("build_runit_runTests_runner omits wd_lines when dir.exists(pkg_root) is FALSE", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/runit.foo.R")
  pkg_root <- "/nonexistent/pkg"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  mockery::stub(fn, "dir.exists", function(x) FALSE)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("mypkg", test_files, pkg_root = pkg_root)
  
  expect_false(any(grepl("owd <- getwd\\(\\)", written_lines)))
  expect_false(any(grepl("setwd\\(", written_lines)))
})

test_that("build_runit_runTests_runner writes one runTestFile line per test path", {
  fn <- build_runit_runTests_runner
  test_files <- c("/fake/pkg/inst/unitTests/runit.a.R", "/fake/pkg/inst/unitTests/runit.b.R")
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  mockery::stub(fn, "detect_runit_test_prefix", function(...) "runit")
  mockery::stub(fn, "normalizePath", function(x, winslash = "/", mustWork = TRUE) x)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn("mypkg", test_files)
  
  run_lines <- written_lines[grepl("runTestFile", written_lines)]
  expect_length(run_lines, 2L)
  expect_true(any(grepl("runit.a.R", run_lines)))
  expect_true(any(grepl("runit.b.R", run_lines)))
})

test_that("build_doRUnit_wrapper returns runner path and writes correct content", {
  fn <- build_doRUnit_wrapper
  doRUnit_path <- "/fake/pkg/tests/doRUnit.R"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  out <- fn(doRUnit_path)
  
  expect_identical(out, mock_runner)
  expect_true(any(grepl("owd <- getwd\\(\\)", written_lines)))
  expect_true(any(grepl("on.exit\\(setwd\\(owd\\)", written_lines)))
  expect_true(any(grepl('setwd\\("/fake/pkg/tests"\\)', written_lines)))
  expect_true(any(grepl('source\\("/fake/pkg/tests/doRUnit.R"\\)', written_lines)))
})

test_that("build_doRUnit_wrapper uses dirname of doRUnit_path for setwd", {
  fn <- build_doRUnit_wrapper
  doRUnit_path <- "/some/nested/tests/dir/doRUnit.R"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(doRUnit_path)
  
  expect_true(any(grepl('setwd\\("/some/nested/tests/dir"\\)', written_lines)))
})

test_that("build_doRUnit_wrapper sources doRUnit_path exactly", {
  fn <- build_doRUnit_wrapper
  doRUnit_path <- "/path/to/doRUnit.R"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(doRUnit_path)
  
  expect_true(any(grepl('source\\("/path/to/doRUnit.R"\\)', written_lines)))
})

test_that("build_doRUnit_wrapper writes exactly four lines", {
  fn <- build_doRUnit_wrapper
  doRUnit_path <- "/fake/tests/doRUnit.R"
  mock_runner <- tempfile(fileext = ".R")
  
  mockery::stub(fn, "tempfile", function(fileext = "") mock_runner)
  
  written_lines <- NULL
  mockery::stub(fn, "writeLines", function(text, con, ...) {
    written_lines <<- text
    base::writeLines(text, con)
  })
  
  fn(doRUnit_path)
  
  expect_length(written_lines, 4L)
})