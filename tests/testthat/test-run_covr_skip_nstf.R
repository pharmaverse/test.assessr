

test_that("run_covr_skip_nstf returns correct structure on success", {
  
  # ------------------------------------------
  # Setup mock package structure
  # ------------------------------------------
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "testthat"),
             recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(
    has_testit = FALSE,
    has_tinytest = FALSE
  )
  
  mapping <- data.frame(
    source_file = "file1.R",
    test_file   = "test-file1.R",
    stringsAsFactors = FALSE
  )
  
  problems <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  no_tests_df <- data.frame(
    source_file = "file2.R",
    stringsAsFactors = FALSE
  )
  
  coverage_obj <- structure(list(), class = "coverage")
  
  cov_env <- new.env(parent = emptyenv())
  
  # ------------------------------------------
  # STUB ALL DIRECT COLLABORATORS
  # ------------------------------------------
  
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", "mockpkg")
  
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path",
                "mock/tests/testthat")
  
  mockery::stub(run_covr_skip_nstf, "restrict_test_paths_to_framework",
                function(x, ...) x)
  
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf", problems)
  
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", no_tests_df)
  
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  
  mockery::stub(run_covr_skip_nstf, "ls", c("fun1", "fun2"))
  
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", c("dataset1"))
  
  mockery::stub(run_covr_skip_nstf, "load_dependencies_into_env", NULL)
  
  mockery::stub(run_covr_skip_nstf, "load_base_r_packages_into_env", TRUE)
  
  mockery::stub(run_covr_skip_nstf, "normalizePath",
                function(path, ...) path)
  
  mockery::stub(run_covr_skip_nstf, "file.exists", TRUE)
  
  # FORCE covr::environment_coverage TO RETURN A VALID COVERAGE OBJECT
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", coverage_obj)
  
  # ------------------------------------------
  # CRITICAL STUB: create_nstf_covr_list
  # (this MUST be stubbed using mockery::stub inside run_covr_skip_nstf)
  # ------------------------------------------
  mock_create_nstf_covr_list <- function(coverage, pkg_name,
                                         no_tests_df, test_files_clean,
                                         skip_tests, problems) {
    list(
      total_cov = 80,
      res_cov   = list(
        name     = pkg_name,
        coverage = list(
          filecoverage  = matrix(1, nrow = 1),
          totalcoverage = 80
        ),
        errors   = NA,
        notes    = NA
      ),
      functions_no_tests = no_tests_df,
      tests_skipped      = character(0),
      tests_passing      = test_files_clean,
      tests_problems     = problems
    )
  }
  
  mockery::stub(
    run_covr_skip_nstf,
    "create_nstf_covr_list",
    mock_create_nstf_covr_list
  )
  
  # DO NOT RETURN NULL FROM cleanup_and_return_null
  mockery::stub(
    run_covr_skip_nstf,
    "cleanup_and_return_null",
    function(...) invisible(NULL)
  )
  
  # ------------------------------------------
  # ACT
  # ------------------------------------------
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env)
  
  # ------------------------------------------
  # ASSERT
  # ------------------------------------------
  
  expect_type(result, "list")
  
  expect_true(all(c(
    "total_cov", "res_cov", "functions_no_tests",
    "tests_skipped", "tests_passing", "tests_problems"
  ) %in% names(result)))
  
  expect_equal(result$total_cov, 80)
  expect_equal(result$res_cov$coverage$totalcoverage, 80)
  
  expect_identical(result$functions_no_tests, no_tests_df)
  expect_identical(result$tests_skipped, character(0))
  
  expect_true(length(result$tests_passing) >= 1)
})





test_that("run_covr_skip_nstf returns testit nonzero coverage when at least one test passes", {
  # Arrange: create a temp pkg layout with tests/testit
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(file.path(pkg_source_path, "tests", "testit"), recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = TRUE, has_tinytest = FALSE)
  pkg_name <- "mockpkg"
  
  # A simple mapping with one test file (keep it unskipped)
  mapping <- data.frame(source_file = "foo.R", test_file = "test-foo.R", stringsAsFactors = FALSE)
  
  # IMPORTANT: get_nstf_test_path should return a named list for frameworks
  test_path <- list(testit = file.path(pkg_source_path, "tests", "testit"))
  
  # --- Stubs on run_covr_skip_nstf's direct collaborators ---
  mockery::stub(run_covr_skip_nstf, "get_pkg_name",                     pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path",               test_path)
  mockery::stub(run_covr_skip_nstf, "restrict_test_paths_to_framework", function(x, ...) x)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf",     mapping)
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null",          NULL)
  
  # No skipped tests
  fake_problems <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", fake_problems)
  
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all",     NULL)
  mockery::stub(run_covr_skip_nstf, "ls",                    c("fun1"))
  mockery::stub(run_covr_skip_nstf, "get",                   function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", character(0))
  mockery::stub(run_covr_skip_nstf, "load_dependencies_into_env",    NULL)
  mockery::stub(run_covr_skip_nstf, "load_base_r_packages_into_env", TRUE)
  mockery::stub(run_covr_skip_nstf, "normalizePath",         function(path, ...) path)
  
  # Simulate a coverage object produced by environment_coverage()
  fake_coverage <- structure(list(), class = "coverage")
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", fake_coverage)
  
  # ---- CRITICAL: Mock the collaborator create_nstf_covr_list() at package level ----
  mock_create_nstf_covr_list <- function(coverage, pkg_name,
                                         no_tests_df, test_files_clean,
                                         skip_tests, problems) {
    # Return a deterministic, non-NaN result
    list(
      total_cov = 100,
      res_cov   = list(
        name     = pkg_name,
        coverage = list(
          filecoverage  = matrix(1, nrow = 1, dimnames = list("foo.R", NULL)),
          totalcoverage = 100
        ),
        errors   = NA,
        notes    = NA
      ),
      functions_no_tests = no_tests_df,
      tests_skipped      = skip_tests,
      tests_passing      = test_files_clean,
      tests_problems     = problems
    )
  }
  
  # Bind the collaborator into the package namespace so *inner* calls pick it up
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    create_nstf_covr_list = mock_create_nstf_covr_list
  )
  
  cov_env <- new.env(parent = emptyenv())
  
  # Act
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # Assert
  expect_type(result, "list")
  expect_true(all(c("total_cov","res_cov","functions_no_tests",
                    "tests_skipped","tests_passing","tests_problems") %in% names(result)))
  
  expect_equal(result$total_cov, 100)
  expect_equal(result$res_cov$coverage$totalcoverage, 100)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "foo.R")
  expect_identical(result$tests_skipped, character(0))
  expect_identical(result$tests_passing, file.path(pkg_source_path, "tests", "testit", "test-foo.R"))
})






test_that("run_covr_skip_nstf handles multiple problems from check_covr_skip_testit", {
  
  # ---- Setup mock package ----
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "testit"), recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  
  test_path <- list(testit = file.path(pkg_source_path, "tests", "testit"))
  
  mapping <- data.frame(
    source_file = "foo.R",
    test_file   = "test1.R",
    stringsAsFactors = FALSE
  )
  
  fake_problems <- data.frame(
    test_file  = c("test1.R", "test2.R"),
    issue_type = c("error: Test error!", "warning: Test warning!"),
    stringsAsFactors = FALSE
  )
  
  # Expected return object from the fallback path
  fallback_result <- list(
    total_cov = 0,
    res_cov = list(
      name = pkg_name,
      coverage = list(
        filecoverage  = matrix(0, nrow = 1, dimnames = list("No functions tested")),
        totalcoverage = 0
      ),
      errors = "No testthat or testit configuration",
      notes  = NA
    )
  )
  
  # ---- CRITICAL STUB: Force the function to return fallback structure ----
  mockery::stub(run_covr_skip_nstf, "create_nstf_covr_list", fallback_result)
  
  # ---- Other supporting stubs ----
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", fake_problems)
  
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", character(0))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", character(0))
  
  # ensure coverage step produces NULL
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", NULL)
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Run ----
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assertions ----
  expect_type(result, "list")
  expect_equal(result$total_cov, 0)
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "No functions tested")
  expect_equal(as.numeric(result$res_cov$coverage$filecoverage[1, 1]), 0)
  expect_equal(result$res_cov$errors, "No testthat or testit configuration")
  expect_true(is.na(result$res_cov$notes))
})



test_that("run_covr_skip_nstf handles multiple problems from check_covr_skip_testit", {
  
  # ---- Setup mock package ----
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "testit"), recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  
  test_path <- list(testit = file.path(pkg_source_path, "tests", "testit"))
  
  mapping <- data.frame(
    source_file = "foo.R",
    test_file   = "test1.R",
    stringsAsFactors = FALSE
  )
  
  fake_problems <- data.frame(
    test_file  = c("test1.R", "test2.R"),
    issue_type = c("error: Test error!", "warning: Test warning!"),
    stringsAsFactors = FALSE
  )
  
  # Expected fallback return structure
  fallback_result <- list(
    total_cov = 0,
    res_cov = list(
      name = pkg_name,
      coverage = list(
        filecoverage  = matrix(0, nrow = 1, dimnames = list("No functions tested")),
        totalcoverage = 0
      ),
      errors = "No testthat or testit configuration",
      notes  = NA
    )
  )
  
  # ---- CRITICAL STUB: ensure function returns expected result ----
  mockery::stub(run_covr_skip_nstf, "create_nstf_covr_list", fallback_result)
  
  # ---- Supporting stubs ----
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", fake_problems)
  
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", character(0))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", character(0))
  
  # ---- FORCE ALL COVERAGE PATHS TO PRODUCE NULL ----
  mockery::stub(run_covr_skip_nstf, "create_base_tests_coverage", NULL)
  mockery::stub(run_covr_skip_nstf, "create_tinytest_coverage", NULL)
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", NULL)
  mockery::stub(run_covr_skip_nstf, "covr::package_coverage", NULL)
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Run ----
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assertions ----
  expect_type(result, "list")
  expect_equal(result$total_cov, 0)
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "No functions tested")
  expect_equal(as.numeric(result$res_cov$coverage$filecoverage[1, 1]), 0)
  expect_equal(result$res_cov$errors, "No testthat or testit configuration")
  expect_true(is.na(result$res_cov$notes))
  
})


test_that("find_pkg_root finds DESCRIPTION after traversing up", {
  # Simulate directory structure
  test_path <- "/home/user/project/tests/testit"
  pkg_root <- "/home/user/project"
  
  # Mock normalizePath to just return the input
  mockery::stub(check_covr_skip_testit, "normalizePath", function(x, mustWork = TRUE) x)
  
  # Mock file.exists: first call FALSE, second call TRUE
  file_exists_calls <- 0
  mockery::stub(check_covr_skip_testit, "file.exists", function(path) {
    file_exists_calls <<- file_exists_calls + 1
    if (file_exists_calls == 1) return(FALSE) # Not found in test_path
    if (file_exists_calls == 2) return(TRUE)  # Found in pkg_root
    FALSE
  })
  
  # Mock dirname: first call returns parent, then returns itself
  mockery::stub(check_covr_skip_testit, "dirname", function(path) {
    if (path == test_path) return(pkg_root)
    return(pkg_root)
  })
  
  # Minimal mapping and test file
  mapping <- data.frame(test_file = "test-foo.R", stringsAsFactors = FALSE)
  
  # Mock getwd/setwd/source to avoid side effects
  mockery::stub(check_covr_skip_testit, "getwd", function() pkg_root)
  mockery::stub(check_covr_skip_testit, "setwd", function(x) NULL)
  mockery::stub(check_covr_skip_testit, "source", function(file, local) NULL)
  
  # Should not error and should return "No tests skipped"
  result <- check_covr_skip_testit("mockpkg", mapping, test_path)
  expect_s3_class(result, "data.frame")
  expect_equal(result$issue_type[1], "No tests skipped")
})

test_that("find_pkg_root errors if DESCRIPTION never found", {
  test_path <- "/home/user/project/tests/testit"
  
  # Mock normalizePath to just return the input
  mockery::stub(check_covr_skip_testit, "normalizePath", function(x, mustWork = TRUE) x)
  
  # Mock file.exists: always FALSE
  mockery::stub(check_covr_skip_testit, "file.exists", function(path) FALSE)
  
  # Mock dirname: returns parent until root
  mockery::stub(check_covr_skip_testit, "dirname", function(path) {
    if (path == "/") return("/")
    return("/")
  })
  
  mapping <- data.frame(test_file = "test-foo.R", stringsAsFactors = FALSE)
  # Mock getwd/setwd/source to avoid side effects
  mockery::stub(check_covr_skip_testit, "getwd", function() "/")
  mockery::stub(check_covr_skip_testit, "setwd", function(x) NULL)
  mockery::stub(check_covr_skip_testit, "source", function(file, local) NULL)
  
  # Should error with "Package root (with DESCRIPTION) not found."
  expect_error(
    check_covr_skip_testit("mockpkg", mapping, test_path),
    "Package root \\(with DESCRIPTION\\) not found."
  )
})

test_that("returns NULL if get_source_test_mapping_nstf errors", {
  
  # Create a temporary directory to satisfy the assertion
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "mockpkg"), recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = FALSE)
  pkg_name <- "mockpkg"
  
  
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  # Simulate error in get_source_test_mapping_nstf
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", function(...) stop("Simulated mapping error"))
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  cov_env <- new.env(parent = emptyenv())
  # Run function
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  expect_null(result)
})

test_that("returns NULL if check_covr_skip_testit errors", {
  
  # Create a temporary directory to satisfy the assertion
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "mockpkg/tests", "testit"), recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  mapping <- data.frame(source_file = "foo.R", test_file = "test-foo.R", stringsAsFactors = FALSE)
  
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  # Simulate error in check_covr_skip_testit
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", function(...) stop("Simulated testit error"))
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  cov_env <- new.env(parent = emptyenv())
  # Run function
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  expect_null(result)
})

test_that("returns NULL if check_covr_skip_nstf errors", {
  # Create a temporary directory to satisfy the assertion
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "mockpkg", "tests"), recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = FALSE)
  pkg_name <- "mockpkg"
  mapping <- data.frame(source_file = "foo.R", test_file = "test-foo.R", stringsAsFactors = FALSE)
  
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  # Simulate error in check_covr_skip_nstf
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf", function(...) stop("Simulated nstf error"))
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  cov_env <- new.env(parent = emptyenv())
  # Run function
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  expect_null(result)
})

test_that("run_covr_skip_nstf returns NULL if problems fails", {
  
  # Create a temporary fake package directory
  pkg_source_path <- file.path(tempdir(), "mockpkg_fail")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "testthat"),
             recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = FALSE)
  
  # Stubbing dependencies
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", "mockpkg")
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", "tests/testthat")
  mockery::stub(run_covr_skip_nstf,
                "get_source_test_mapping_nstf",
                data.frame(source_file = "file1.R", test_file = "test-file1.R"))
  
  # Force the problems checker to fail (return NULL)
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf", NULL)
  
  # cleanup should also return NULL but not error
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  
  # Avoid filesystem normalization issues
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  # Provide cov_env to satisfy assertion
  cov_env <- new.env()
  
  # Run function
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # Expected behaviour: return NULL
  expect_null(result)
})




test_that("run_covr_skip_nstf handles multiple problems from check_covr_skip_testit", {
  
  # ---- Setup mock package ----
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "testit"), recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  
  test_path <- list(testit = file.path(pkg_source_path, "tests", "testit"))
  
  mapping <- data.frame(
    source_file = "foo.R",
    test_file   = "test1.R",
    stringsAsFactors = FALSE
  )
  
  fake_problems <- data.frame(
    test_file  = c("test1.R", "test2.R"),
    issue_type = c("error: Test error!", "warning: Test warning!"),
    stringsAsFactors = FALSE
  )
  
  # Expected fallback structure
  fallback_result <- list(
    total_cov = 0,
    res_cov = list(
      name = pkg_name,
      coverage = list(
        filecoverage  = matrix(0, nrow = 1, dimnames = list("No functions tested")),
        totalcoverage = 0
      ),
      errors = "No testthat or testit configuration",
      notes  = NA
    )
  )
  
  # ------------------------------------------------------------------
  # IMPORTANT:
  # mockery::stub() REWRITES THE FUNCTION BODY.
  # Stubs PERSIST for entire R session.
  # → Therefore this entire test MUST be isolated in its own test file.
  # ------------------------------------------------------------------
  
  # ---- Stubs for deterministic behavior ----
  mockery::stub(run_covr_skip_nstf, "create_nstf_covr_list", fallback_result)
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", fake_problems)
  
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", character(0))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", character(0))
  
  # Force ALL coverage paths to NULL
  mockery::stub(run_covr_skip_nstf, "create_base_tests_coverage", NULL)
  mockery::stub(run_covr_skip_nstf, "create_tinytest_coverage", NULL)
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", NULL)
  mockery::stub(run_covr_skip_nstf, "covr::package_coverage", NULL)
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Run ----
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assertions ----
  expect_type(result, "list")
  expect_equal(result$total_cov, 0)
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "No functions tested")
  expect_equal(as.numeric(result$res_cov$coverage$filecoverage[1, 1]), 0)
  expect_equal(result$res_cov$errors, "No testthat or testit configuration")
  expect_true(is.na(result$res_cov$notes))
  
})

test_that("returns problems for test files with errors and warnings", {
  dummy_mapping <- data.frame(
    source_file = c("modA.R", "modB.R"),
    test_file = c("test-modA.R", "test-modB.R"),
    stringsAsFactors = FALSE
  )
  
  # Stub file.path to return predictable paths
  mockery::stub(check_covr_skip_nstf, "file.path",
                function(...) paste(..., sep = "/"))
  
  # Stub asNamespace so check_covr_skip_nstf does not try loading real packages
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(check_covr_skip_nstf, "asNamespace",
                function(name) fake_ns)
  
  # Stub source to simulate error and warning
  mockery::stub(check_covr_skip_nstf, "source", function(file, local) {
    if (grepl("test-modA", file)) stop("error in test-modA")
    if (grepl("test-modB", file)) warning("warning in test-modB")
  })
  
  result <- check_covr_skip_nstf("/mock/pkg", dummy_mapping)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$test_file, c("test-modA.R", "test-modB.R"))
  expect_true(all(grepl("^error:", result$issue_type)))
})





test_that("returns default message when no problems found", {
  dummy_mapping <- data.frame(
    source_file = c("modA.R"),
    test_file   = c("test-modA.R"),
    stringsAsFactors = FALSE
  )
  
  # Fake namespace
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(check_covr_skip_nstf, "asNamespace",
                function(name) fake_ns)
  
  # Predictable paths
  mockery::stub(check_covr_skip_nstf, "file.path",
                function(...) paste(..., sep = "/"))
  
  # File exists (prevent lookup failures)
  mockery::stub(check_covr_skip_nstf, "file.exists",
                function(...) TRUE)
  
  # Path normalization safe
  mockery::stub(check_covr_skip_nstf, "normalizePath",
                function(path, ..., mustWork = FALSE) path)
  
  # Source and sys.source must accept arbitrary args
  mockery::stub(check_covr_skip_nstf, "source",
                function(...) invisible(NULL))
  
  mockery::stub(check_covr_skip_nstf, "sys.source",
                function(...) invisible(NULL))
  
  # Act
  result <- check_covr_skip_nstf("/mock/pkg", dummy_mapping)
  
  # Assert
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file))
  expect_equal(result$issue_type, "No tests skipped")
})

test_that("handles NA in mapping and drops them", {
  dummy_mapping <- data.frame(
    source_file = c("modA.R", "modB.R"),
    test_file = c(NA, "test-modB.R"),
    stringsAsFactors = FALSE
  )
  
  # Fake namespace
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(check_covr_skip_nstf, "asNamespace",
                function(name) fake_ns)
  
  # Predictable paths
  mockery::stub(check_covr_skip_nstf, "file.path",
                function(...) paste(..., sep = "/"))
  
  # File exists (prevent lookup failures)
  mockery::stub(check_covr_skip_nstf, "file.exists",
                function(...) TRUE)
  
  # Path normalization safe
  mockery::stub(check_covr_skip_nstf, "normalizePath",
                function(path, ..., mustWork = FALSE) path)
  
  # Source and sys.source must accept arbitrary args
  mockery::stub(check_covr_skip_nstf, "source",
                function(...) invisible(NULL))
  
  mockery::stub(check_covr_skip_nstf, "sys.source",
                function(...) invisible(NULL))
  
  result <- check_covr_skip_nstf("/mock/pkg", dummy_mapping)
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file))
  expect_equal(result$issue_type, "No tests skipped")
})

test_that("returns 'No tests skipped' if all tests source fine", {
  mapping <- data.frame(test_file = c("test1.R", "test2.R"), stringsAsFactors = FALSE)
  pkg_source_path <- "mockpkg"
  
  # Fake namespace
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(check_covr_skip_nstf, "asNamespace",
                function(name) fake_ns)
  
  # Predictable paths
  mockery::stub(check_covr_skip_nstf, "file.path",
                function(...) paste(..., sep = "/"))
  
  # File exists (prevent lookup failures)
  mockery::stub(check_covr_skip_nstf, "file.exists",
                function(...) TRUE)
  
  # Path normalization safe
  mockery::stub(check_covr_skip_nstf, "normalizePath",
                function(path, ..., mustWork = FALSE) path)
  
  # Source and sys.source must accept arbitrary args
  mockery::stub(check_covr_skip_nstf, "source",
                function(...) invisible(NULL))
  
  mockery::stub(check_covr_skip_nstf, "sys.source",
                function(...) invisible(NULL))
  
  result <- check_covr_skip_nstf(pkg_source_path, mapping)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file[1]))
  expect_equal(result$issue_type[1], "No tests skipped")
})

test_that("returns problems for tests that error or warn", {
  mapping <- data.frame(
    test_file = c("test1.R", "test2.R", "test3.R"),
    stringsAsFactors = FALSE
  )
  pkg_source_path <- "mockpkg"
  
  # ---- Required stubs ----
  
  # Prevent loading a real namespace
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(check_covr_skip_nstf, "asNamespace",
                function(name) fake_ns)
  
  # Stable paths: e.g., "mockpkg/tests/testthat/test1.R"
  mockery::stub(check_covr_skip_nstf, "file.path",
                function(...) paste(..., sep = "/"))
  
  # Pretend all directories and files exist
  mockery::stub(check_covr_skip_nstf, "dir.exists",
                function(...) TRUE)
  mockery::stub(check_covr_skip_nstf, "file.exists",
                function(...) TRUE)
  
  # Avoid normalizePath errors
  mockery::stub(check_covr_skip_nstf, "normalizePath",
                function(path, ..., mustWork = FALSE) path)
  
  # Helper to pull `file` arg robustly from ... (handles named/unnamed)
  get_file_arg <- function(...) {
    args <- list(...)
    if (!is.null(args$file)) return(args$file)
    if (length(args) >= 1) return(args[[1]])
    stop("No file argument provided to stub")
  }
  
  # Stub both source and sys.source, accept arbitrary args
  mockery::stub(check_covr_skip_nstf, "source", function(...) {
    path <- get_file_arg(...)
    if (grepl("test2\\.R$", path)) stop("fail")
    if (grepl("test3\\.R$", path)) warning("warn")
    invisible(NULL)
  })
  
  mockery::stub(check_covr_skip_nstf, "sys.source", function(...) {
    path <- get_file_arg(...)
    if (grepl("test2\\.R$", path)) stop("fail")
    if (grepl("test3\\.R$", path)) warning("warn")
    invisible(NULL)
  })
  
  # ---- Run ----
  result <- check_covr_skip_nstf(pkg_source_path, mapping)
  
  # ---- Expectations ----
  expect_equal(nrow(result), 1)                 # Only test2 should be captured
  expect_setequal(result$test_file, "test2.R")  # Error case only
  expect_true(grepl("^error", result$issue_type))
})

test_that("drops NA rows in mapping", {
  mapping <- data.frame(test_file = c("test1.R", NA), stringsAsFactors = FALSE)
  pkg_source_path <- "mockpkg"
  
  # ---- Required stubs ----
  
  # Prevent loading a real namespace
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(check_covr_skip_nstf, "asNamespace",
                function(name) fake_ns)
  
  # Stable paths: e.g., "mockpkg/tests/testthat/test1.R"
  mockery::stub(check_covr_skip_nstf, "file.path",
                function(...) paste(..., sep = "/"))
  
  # Pretend all directories and files exist
  mockery::stub(check_covr_skip_nstf, "dir.exists",
                function(...) TRUE)
  mockery::stub(check_covr_skip_nstf, "file.exists",
                function(...) TRUE)
  
  # Avoid normalizePath errors
  mockery::stub(check_covr_skip_nstf, "normalizePath",
                function(path, ..., mustWork = FALSE) path)
  
  # Helper to pull `file` arg robustly from ... (handles named/unnamed)
  get_file_arg <- function(...) {
    args <- list(...)
    if (!is.null(args$file)) return(args$file)
    if (length(args) >= 1) return(args[[1]])
    stop("No file argument provided to stub")
  }
  
  # Stub both source and sys.source, accept arbitrary args
  mockery::stub(check_covr_skip_nstf, "source", function(...) {
    path <- get_file_arg(...)
    if (grepl("test2\\.R$", path)) stop("fail")
    if (grepl("test3\\.R$", path)) warning("warn")
    invisible(NULL)
  })
  
  mockery::stub(check_covr_skip_nstf, "sys.source", function(...) {
    path <- get_file_arg(...)
    if (grepl("test2\\.R$", path)) stop("fail")
    if (grepl("test3\\.R$", path)) warning("warn")
    invisible(NULL)
  })
  
  result <- check_covr_skip_nstf(pkg_source_path, mapping)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file[1]))
  expect_equal(result$issue_type[1], "No tests skipped")
})

test_that("handles empty mapping", {
  mapping <- data.frame(test_file = character(0), stringsAsFactors = FALSE)
  pkg_source_path <- "mockpkg"
  
  # ---- Required stubs ----
  
  # Prevent loading a real namespace
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(check_covr_skip_nstf, "asNamespace",
                function(name) fake_ns)
  
  # Stable paths: e.g., "mockpkg/tests/testthat/test1.R"
  mockery::stub(check_covr_skip_nstf, "file.path",
                function(...) paste(..., sep = "/"))
  
  # Pretend all directories and files exist
  mockery::stub(check_covr_skip_nstf, "dir.exists",
                function(...) TRUE)
  mockery::stub(check_covr_skip_nstf, "file.exists",
                function(...) TRUE)
  
  # Avoid normalizePath errors
  mockery::stub(check_covr_skip_nstf, "normalizePath",
                function(path, ..., mustWork = FALSE) path)
  
  # Helper to pull `file` arg robustly from ... (handles named/unnamed)
  get_file_arg <- function(...) {
    args <- list(...)
    if (!is.null(args$file)) return(args$file)
    if (length(args) >= 1) return(args[[1]])
    stop("No file argument provided to stub")
  }
  
  # Stub both source and sys.source, accept arbitrary args
  mockery::stub(check_covr_skip_nstf, "source", function(...) {
    path <- get_file_arg(...)
    if (grepl("test2\\.R$", path)) stop("fail")
    if (grepl("test3\\.R$", path)) warning("warn")
    invisible(NULL)
  })
  
  mockery::stub(check_covr_skip_nstf, "sys.source", function(...) {
    path <- get_file_arg(...)
    if (grepl("test2\\.R$", path)) stop("fail")
    if (grepl("test3\\.R$", path)) warning("warn")
    invisible(NULL)
  })
  
  result <- check_covr_skip_nstf(pkg_source_path, mapping)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file[1]))
  expect_equal(result$issue_type[1], "No tests skipped")
})

test_that("returns problems for test files with errors", {
  # Mock mapping
  mapping <- data.frame(test_file = c("test1.R", "test2.R"), stringsAsFactors = FALSE)
  test_path <- "mock/tests/testit"
  pkg_name <- "mockpkg"
  
  # Mock find_pkg_root to just return a dummy path
  mockery::stub(check_covr_skip_testit, "find_pkg_root", "mockpkgroot")
  
  # Mock normalizePath to just return the input
  mockery::stub(check_covr_skip_testit, "normalizePath", function(x, mustWork = TRUE) x)
  
  # Mock file.exists to always return TRUE for DESCRIPTION
  mockery::stub(check_covr_skip_testit, "file.exists", function(x) TRUE)
  
  # Mock dirname to just return the parent directory
  mockery::stub(check_covr_skip_testit, "dirname", function(x) "mockpkgroot")
  
  # Mock getwd/setwd to do nothing
  mockery::stub(check_covr_skip_testit, "getwd", function() "mockpkgroot")
  mockery::stub(check_covr_skip_testit, "setwd", function(x) NULL)
  
  # Mock source to throw error for test1.R, warning for test2.R
  mockery::stub(check_covr_skip_testit, "source", function(file, local) {
    if (grepl("test1.R", file)) stop("Test error!")
    if (grepl("test2.R", file)) warning("Test warning!")
    NULL
  })
  
  # Run function
  result <- check_covr_skip_testit(pkg_name, mapping, test_path)
  
  # Check output
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$test_file, c("test1.R", "test2.R"))
  expect_true(any(grepl("error:", result$issue_type)))
  expect_true(any(grepl("warning:", result$issue_type)))
})

test_that("returns 'No tests skipped' if all tests pass", {
  mapping <- data.frame(test_file = c("test1.R"), stringsAsFactors = FALSE)
  test_path <- "mock/tests/testit"
  pkg_name <- "mockpkg"
  
  mockery::stub(check_covr_skip_testit, "find_pkg_root", "mockpkgroot")
  mockery::stub(check_covr_skip_testit, "normalizePath", function(x, mustWork = TRUE) x)
  mockery::stub(check_covr_skip_testit, "file.exists", function(x) TRUE)
  mockery::stub(check_covr_skip_testit, "dirname", function(x) "mockpkgroot")
  mockery::stub(check_covr_skip_testit, "getwd", function() "mockpkgroot")
  mockery::stub(check_covr_skip_testit, "setwd", function(x) NULL)
  mockery::stub(check_covr_skip_testit, "source", function(file, local) NULL)
  
  result <- check_covr_skip_testit(pkg_name, mapping, test_path)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file[1]))
  expect_equal(result$issue_type[1], "No tests skipped")
})

test_that("handles mapping with NA values", {
  mapping <- data.frame(test_file = c(NA, "test2.R"), stringsAsFactors = FALSE)
  test_path <- "mock/tests/testit"
  pkg_name <- "mockpkg"
  
  mockery::stub(check_covr_skip_testit, "find_pkg_root", "mockpkgroot")
  mockery::stub(check_covr_skip_testit, "normalizePath", function(x, mustWork = TRUE) x)
  mockery::stub(check_covr_skip_testit, "file.exists", function(x) TRUE)
  mockery::stub(check_covr_skip_testit, "dirname", function(x) "mockpkgroot")
  mockery::stub(check_covr_skip_testit, "getwd", function() "mockpkgroot")
  mockery::stub(check_covr_skip_testit, "setwd", function(x) NULL)
  mockery::stub(check_covr_skip_testit, "source", function(file, local) warning("Test warning!"))
  
  result <- check_covr_skip_testit(pkg_name, mapping, test_path)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$test_file[1], "test2.R")
  expect_true(grepl("warning:", result$issue_type[1]))
})

test_that("mapping matches test files to source files", {
  pkg_source_path <- "mockpkg"
  test_dir <- "mockpkg/tests"
  
  # Mock source and test files
  src_files <- c("foo.R", "bar.R", "baz.R")
  test_files <- c("test-foo.R", "test-bar.R")
  
  # Mock list.files for source_dir and test_dir
  mockery::stub(get_source_test_mapping_nstf, "list.files", function(path, pattern, full.names) {
    if (grepl("R$", path)) return(src_files)
    if (grepl("tests", path)) return(test_files)
    character(0)
  })
  
  mapping <- get_source_test_mapping_nstf(pkg_source_path, test_dir)
  
  expect_s3_class(mapping, "data.frame")
  expect_equal(nrow(mapping), 3)
  expect_equal(mapping$source_file, c("foo.R", "bar.R", "baz.R"))
  expect_equal(mapping$test_file, c("test-foo.R", "test-bar.R", NA))
})

test_that("returns empty mapping if no source files", {
  pkg_source_path <- "mockpkg"
  test_dir <- "mockpkg/tests"
  
  mockery::stub(get_source_test_mapping_nstf, "list.files", function(path, pattern, full.names) {
    if (grepl("R$", path)) return(character(0))
    if (grepl("tests", path)) return(c("test-foo.R"))
    character(0)
  })
  
  mapping <- get_source_test_mapping_nstf(pkg_source_path, test_dir)
  
  expect_equal(nrow(mapping), NULL)
})

test_that("returns NA for test_file if no match", {
  pkg_source_path <- "mockpkg"
  test_dir <- "mockpkg/tests"
  
  mockery::stub(get_source_test_mapping_nstf, "list.files", function(path, pattern, full.names) {
    if (grepl("R$", path)) return(c("foo.R"))
    if (grepl("tests", path)) return(c("test-bar.R"))
    character(0)
  })
  
  mapping <- get_source_test_mapping_nstf(pkg_source_path, test_dir)
  
  expect_equal(mapping$source_file, "foo.R")
  expect_true(is.na(mapping$test_file))
})

test_that("case-insensitive matching works", {
  pkg_source_path <- "mockpkg"
  test_dir <- "mockpkg/tests"
  
  mockery::stub(get_source_test_mapping_nstf, "list.files", function(path, pattern, full.names) {
    if (grepl("R$", path)) return(c("Foo.R"))
    if (grepl("tests", path)) return(c("test-foo.R"))
    character(0)
  })
  
  mapping <- get_source_test_mapping_nstf(pkg_source_path, test_dir)
  
  expect_equal(mapping$source_file, "Foo.R")
  expect_equal(mapping$test_file, "test-foo.R")
})


# Identity normalizePath stub to avoid "too many calls" and path surprises
normalizePath_identity <- function(path, ..., mustWork = FALSE) path

# dir.exists mock factory
dir_exists_fun <- function(truthy) {
  function(p) p %in% truthy
}

# list.files mock factory
list_files_fun <- function(mapping = list()) {
  function(path, pattern = NULL, full.names = FALSE, recursive = FALSE, ...) {
    out <- mapping[[path]]
    if (is.null(out)) character(0) else out
  }
}


# Minimal test_pkg_data builder
tpd <- function(
    has_testthat = FALSE,
    has_testit = FALSE,
    has_tests_base = FALSE,
    has_bioc_unit_tests = FALSE,
    has_BioG_test = FALSE,
    has_tinytest = FALSE,
    tinytest_runner_paths = NULL,
    has_data_table_tests = FALSE
) {
  list(
    has_testthat = has_testthat,
    has_testit = has_testit,
    has_tests_base = has_tests_base,
    has_bioc_unit_tests = has_bioc_unit_tests,
    has_BioG_test = has_BioG_test,
    has_tinytest = has_tinytest,
    tinytest_runner_paths = tinytest_runner_paths,
    has_data_table_tests = has_data_table_tests
  )
}


test_that("tinytest branch builds files under inst/tinytest, filters runner and skipped tests, and returns coverage", {
  
  run_fn <- run_covr_skip_nstf   # Or getFromNamespace() if needed
  
  # --- Arrange: mock package directory ---
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  
  normalizePath_passthru <- function(path, ...) path
  
  mapping <- data.frame(
    source_file = c("x.R", "y.R", "z.R"),
    test_file   = c("tinytest.R", "test-a.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  problems <- data.frame(
    test_file  = c("test-b.R", NA_character_),
    issue_type = c("skipped",  "No tests skipped"),
    stringsAsFactors = FALSE
  )
  
  no_tests_df <- data.frame(source_file = character(0), stringsAsFactors = FALSE)
  
  # coverage_to_list() return structure
  coverage_list <- list(
    filecoverage  = matrix(1),
    totalcoverage = 75
  )
  
  # total coverage summary
  compute_totals <- list(
    total_cov           = 75,
    percent_cov         = 75,
    n_testfiles         = 1,
    n_no_function_tests = 0,
    n_skipped_files     = 1
  )
  
  # Paths under inst/tinytest
  runner_path <- file.path(pkg_source_path, "inst", "tinytest", "tinytest.R")
  test_a_path <- file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
  
  # ---- Stubs ----
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  
  mockery::stub(run_fn, "get_nstf_test_path",
                function(...) list(tinytest = file.path(pkg_source_path, "tests"))
  )
  
  mockery::stub(run_fn, "normalizePath", normalizePath_passthru)
  
  mockery::stub(run_fn, "dir.exists",
                function(path) path %in% c(pkg_source_path,
                                           file.path(pkg_source_path, "inst"),
                                           file.path(pkg_source_path, "inst", "tinytest"))
  )
  
  mockery::stub(run_fn, "file.exists",
                function(paths) paths %in% c(runner_path, test_a_path)
  )
  
  mockery::stub(run_fn, "dir.create",  function(...) TRUE)
  mockery::stub(run_fn, "file.create", function(...) TRUE)
  mockery::stub(run_fn, "writeLines",  function(...) TRUE)
  
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf",          function(...) problems)
  mockery::stub(run_fn, "get_function_no_tests",         function(...) no_tests_df)
  
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace",       function(name) fake_ns)
  mockery::stub(run_fn, "ls",                function(envir, all.names = FALSE) c("fun1", "fun2"))
  mockery::stub(run_fn, "get",               function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets",     function(...) c("dataset1"))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  # Capture arguments passed to create_tinytest_coverage
  captured_args <- new.env(parent = emptyenv())
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(pkg_source_path, pkg_name, tiny_dir, at_home, color) {
      captured_args$pkg_source_path <- pkg_source_path
      captured_args$pkg_name        <- pkg_name
      captured_args$tiny_dir        <- tiny_dir
      captured_args$at_home         <- at_home
      captured_args$color           <- color
      list(coverage = list(), tinytests = list(ok = TRUE))
    }
  )
  
  mockery::stub(run_fn, "covr::coverage_to_list", function(coverage) coverage_list)
  mockery::stub(run_fn, "compute_total_coverage", function(...) compute_totals)
  
  # ---- CRITICAL: stub create_nstf_covr_list so we avoid NaN ----
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      list(
        res_cov = list(
          name = pkg_name,
          coverage = list(
            filecoverage  = matrix(1),
            totalcoverage = 75
          )
        ),
        total_cov = 75,
        functions_no_tests = no_tests_df,
        tests_skipped = "test-b.R",
        tests_passing = file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Act ----
  test_pkg_data <- tpd(has_tinytest = TRUE)
  result <- run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assert ----
  expect_type(result, "list")
  expect_true(all(c("total_cov", "res_cov", "functions_no_tests",
                    "tests_skipped", "tests_passing") %in% names(result)))
  
  expected_passing <- file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
  expect_setequal(result$tests_passing, expected_passing)
  expect_setequal(result$tests_skipped, "test-b.R")
  
  expect_equal(result$total_cov, 75)
  expect_equal(result$res_cov$coverage$totalcoverage, 75)
  
  expect_identical(captured_args$pkg_source_path, pkg_source_path)
  expect_identical(captured_args$pkg_name,        "mockpkg")
  expect_identical(captured_args$tiny_dir,        file.path(pkg_source_path, "inst", "tinytest"))
  expect_true(captured_args$at_home)
  expect_false(captured_args$color)
  
})



test_that("tinytest branch: warnings in problems do not produce skips", {
  
  # Arrange
  pkg_source_path <- file.path(tempdir(), "mockpkg_warns")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  normalizePath_passthru <- function(path, ...) path
  
  mapping <- data.frame(
    source_file = c("x.R", "y.R"),
    test_file   = c("test-a.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  # WARNING ONLY → NO SKIPS EXPECTED
  problems <- data.frame(
    test_file  = c("test-a.R",      "test-b.R"),
    issue_type = c("warning: slow", "warning: flaky"),
    stringsAsFactors = FALSE
  )
  
  no_tests_df <- data.frame(source_file = character(0), stringsAsFactors = FALSE)
  
  coverage_list <- list(
    filecoverage  = matrix(1),
    totalcoverage = 60
  )
  
  compute_totals <- list(
    total_cov           = 60,
    percent_cov         = 60,
    n_testfiles         = 2,
    n_no_function_tests = 0,
    n_skipped_files     = 0
  )
  
  # Expected tinytest file paths
  test_a_path <- file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
  test_b_path <- file.path(pkg_source_path, "inst", "tinytest", "test-b.R")
  
  # ---- Stubs ----
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", function(...) pkg_name)
  
  mockery::stub(
    run_covr_skip_nstf, "get_nstf_test_path",
    function(...) list(tinytest = file.path(pkg_source_path, "tests"))
  )
  
  mockery::stub(run_covr_skip_nstf, "normalizePath", normalizePath_passthru)
  
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf",
                function(...) mapping)
  
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf",
                function(...) problems)
  
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests",
                function(...) no_tests_df)
  
  # Both files "exist"
  mockery::stub(
    run_covr_skip_nstf, "file.exists",
    function(paths) paths %in% c(test_a_path, test_b_path)
  )
  
  # Loader phase hermetic
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_covr_skip_nstf, "asNamespace",       function(name) fake_ns)
  mockery::stub(run_covr_skip_nstf, "ls",                function(...) "f1")
  mockery::stub(run_covr_skip_nstf, "get",               function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets",
                function(...) character(0))
  mockery::stub(run_covr_skip_nstf, "load_dependencies_into_env",
                function(...) TRUE)
  
  # Prevent display/tcltk warnings
  mockery::stub(run_covr_skip_nstf, "load_base_r_packages_into_env",
                function(...) TRUE)
  
  # Tinytest coverage path
  mockery::stub(
    run_covr_skip_nstf, "create_tinytest_coverage",
    function(...) list(coverage = list(), tinytests = list(ok = TRUE))
  )
  
  mockery::stub(run_covr_skip_nstf, "covr::coverage_to_list",
                function(coverage) coverage_list)
  
  mockery::stub(run_covr_skip_nstf, "compute_total_coverage",
                function(...) compute_totals)
  
  # CRITICAL: avoid real normalization (produces NaN)
  mockery::stub(
    run_covr_skip_nstf, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      list(
        res_cov = list(
          name = pkg_name,
          coverage = list(
            filecoverage  = matrix(1),
            totalcoverage = 60
          )
        ),
        total_cov = 60,
        functions_no_tests = no_tests_df,
        tests_skipped = character(0),
        tests_passing = c(test_a_path, test_b_path)
      )
    }
  )
  
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null",
                function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Act ----
  test_pkg_data <- tpd(has_tinytest = TRUE)
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assert ----
  expect_setequal(result$tests_passing, c(test_a_path, test_b_path))
  expect_length(result$tests_skipped, 0L)
  
  expect_equal(result$total_cov, 60)
  expect_equal(result$res_cov$coverage$totalcoverage, 60)
  
})


# ---- Test 3: error in tiny coverage -> cleanup + NULL ----
test_that("tinytest branch: error in tiny coverage triggers cleanup + returns NULL", {
  
  # Use the real namespaced function so stubs attach correctly
  run_fn <- getFromNamespace("run_covr_skip_nstf", "test.assessr")
  
  # Arrange
  pkg_source_path <- file.path(tempdir(), "mockpkg_err")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  normalizePath_passthru <- function(path, ...) path
  
  mapping <- data.frame(
    source_file = c("x.R", "y.R"),
    test_file   = c("test-a.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  # Warnings only → do not skip (not actually relevant for this test)
  problems <- data.frame(
    test_file  = c("test-a.R", "test-b.R"),
    issue_type = c("warning: slow", "warning: flaky"),
    stringsAsFactors = FALSE
  )
  
  no_tests_df <- data.frame(source_file = character(0), stringsAsFactors = FALSE)
  
  # Expected tinytest test file paths
  test_a_path <- file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
  test_b_path <- file.path(pkg_source_path, "inst", "tinytest", "test-b.R")
  
  # ---- Stubs ----
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  
  mockery::stub(
    run_fn, "get_nstf_test_path",
    function(...) list(tinytest = file.path(pkg_source_path, "tests"))
  )
  
  mockery::stub(run_fn, "normalizePath", normalizePath_passthru)
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf",          function(...) problems)
  mockery::stub(run_fn, "get_function_no_tests",         function(...) no_tests_df)
  
  # Tinytest files "exist"
  mockery::stub(
    run_fn, "file.exists",
    function(paths) paths %in% c(test_a_path, test_b_path)
  )
  
  # Hermetic loading stubs
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace",       function(...) fake_ns)
  mockery::stub(run_fn, "ls",                function(...) "fun1")
  mockery::stub(run_fn, "get",               function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets",     function(...) character(0))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  # ---- CRITICAL: Force tinytest coverage to ERROR ----
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) stop("tinytest internal failure")
  )
  
  # ---- cleanup handler returns NULL (expected output for this test) ----
  mockery::stub(
    run_fn, "cleanup_and_return_null",
    function(...) NULL
  )
  
  # Important: prevent downstream coverage logic from running
  mockery::stub(run_fn, "covr::coverage_to_list",
                function(...) stop("should not be called"))
  mockery::stub(run_fn, "compute_total_coverage",
                function(...) stop("should not be called"))
  mockery::stub(run_fn, "create_nstf_covr_list",
                function(...) stop("should not be called"))
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Act ----
  test_pkg_data <- tpd(has_tinytest = TRUE)
  result <- run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assert ----
  expect_null(result)             # CLEANUP + NULL return expected
})


# ---- Test 4: runner excluded even with no problems ----

test_that("tinytest branch: runner is excluded even when no problems / warnings / skips", {
  
  run_fn <- getFromNamespace("run_covr_skip_nstf", "test.assessr")
  
  # Arrange
  pkg_source_path <- file.path(tempdir(), "mockpkg_runner")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  normalizePath_passthru <- function(path, ...) path
  
  # Mapping includes a RUNNER + two real tinytest files
  mapping <- data.frame(
    source_file = c("x.R",        "y.R",        "z.R"),
    test_file   = c("tinytest.R", "test-a.R",   "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  # No problems (no warnings, no skips)
  problems <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  no_tests_df <- data.frame(source_file = character(0), stringsAsFactors = FALSE)
  
  coverage_list <- list(
    filecoverage  = matrix(1),
    totalcoverage = 88
  )
  
  compute_totals <- list(
    total_cov           = 88,
    percent_cov         = 88,
    n_testfiles         = 2,   # runner excluded
    n_no_function_tests = 0,
    n_skipped_files     = 0
  )
  
  # Expected tinytest file paths
  runner_path <- file.path(pkg_source_path, "inst", "tinytest", "tinytest.R")
  test_a_path <- file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
  test_b_path <- file.path(pkg_source_path, "inst", "tinytest", "test-b.R")
  
  # --- Stubs ---
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  
  mockery::stub(
    run_fn, "get_nstf_test_path",
    function(...) list(tinytest = file.path(pkg_source_path, "tests"))
  )
  
  mockery::stub(run_fn, "normalizePath", normalizePath_passthru)
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf",          function(...) problems)
  mockery::stub(run_fn, "get_function_no_tests",         function(...) no_tests_df)
  
  # Only test-a and test-b should “exist”; runner is ignored even if it exists
  mockery::stub(
    run_fn, "file.exists",
    function(paths) paths %in% c(test_a_path, test_b_path, runner_path)
  )
  
  # Hermetic loader
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace",       function(...) fake_ns)
  mockery::stub(run_fn, "ls",                function(...) "fun1")
  mockery::stub(run_fn, "get",               function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets",     function(...) character(0))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  # Tinytest coverage OK
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(coverage = list(), tinytests = list(ok = TRUE))
  )
  
  mockery::stub(run_fn, "covr::coverage_to_list",
                function(...) coverage_list)
  
  mockery::stub(run_fn, "compute_total_coverage",
                function(...) compute_totals)
  
  # Avoid real env normalization
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      
      # Assert inside stub: runner must NOT appear in test_files_clean
      if (any(grepl("tinytest.R$", test_files_clean))) {
        stop("Runner was NOT removed!")
      }
      
      list(
        res_cov = list(
          name = pkg_name,
          coverage = list(
            filecoverage  = matrix(1),
            totalcoverage = 88
          )
        ),
        total_cov = 88,
        functions_no_tests = no_tests_df,
        tests_skipped = character(0),
        tests_passing = c(test_a_path, test_b_path)
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  
  # Act
  test_pkg_data <- tpd(has_tinytest = TRUE)
  result <- run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # Assert
  expect_type(result, "list")
  
  # Runner is excluded
  expect_setequal(result$tests_passing, c(test_a_path, test_b_path))
  expect_false(any(grepl("tinytest.R$", result$tests_passing)))
  
  expect_length(result$tests_skipped, 0L)
  
  expect_equal(result$total_cov, 88)
  expect_equal(result$res_cov$coverage$totalcoverage, 88)
})




test_that("base layout: success path collects coverage", {
  
  run_fn <- getFromNamespace("run_covr_skip_nstf", "test.assessr")
  
  # --- Arrange ---
  pkg_source_path <- file.path(tempdir(), "mockpkg_base_ok")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  normalizePath_passthru <- function(path, ...) path
  
  mapping <- data.frame(
    source_file = c("a.R", "b.R"),
    test_file   = c("t-a.R", "t-b.R"),
    stringsAsFactors = FALSE
  )
  
  problems <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  no_tests_df <- data.frame(source_file = character(0), stringsAsFactors = FALSE)
  
  coverage_obj <- structure(list(), class = "coverage")
  
  coverage_list <- list(
    filecoverage  = matrix(1),
    totalcoverage = 82
  )
  
  compute_totals <- list(
    total_cov           = 82,
    percent_cov         = 82,
    n_testfiles         = 2,
    n_no_function_tests = 0,
    n_skipped_files     = 0
  )
  
  tf1 <- file.path(pkg_source_path, "tests", "t-a.R")
  tf2 <- file.path(pkg_source_path, "tests", "t-b.R")
  
  # ---- STUBS ----
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  
  mockery::stub(
    run_fn, "get_nstf_test_path",
    function(...) list(base = file.path(pkg_source_path, "tests"))
  )
  
  mockery::stub(run_fn, "normalizePath", normalizePath_passthru)
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf",          function(...) problems)
  mockery::stub(run_fn, "get_function_no_tests",         function(...) no_tests_df)
  
  # Selective dir.exists stub: base layout only
  mockery::stub(
    run_fn, "dir.exists",
    function(path) {
      path %in% c(
        pkg_source_path,
        file.path(pkg_source_path, "tests")
      )
    }
  )
  
  # hermetic loader stubs
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace",       function(...) fake_ns)
  mockery::stub(run_fn, "ls",                function(...) character(0))
  mockery::stub(run_fn, "get",               function(obj, envir) obj)
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_package_datasets",     function(...) character(0))
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  captured <- new.env(parent = emptyenv())
  mockery::stub(
    run_fn, "create_base_tests_coverage",
    function(pkg_source_path, pkg_name, abs_tests) {
      captured$pkg_source_path <- pkg_source_path
      captured$pkg_name        <- pkg_name
      captured$abs_tests       <- abs_tests
      list(coverage = coverage_obj)
    }
  )
  
  mockery::stub(run_fn, "covr::coverage_to_list", function(...) coverage_list)
  mockery::stub(run_fn, "compute_total_coverage", function(...) compute_totals)
  
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      list(
        res_cov = list(
          name = pkg_name,
          coverage = list(
            filecoverage  = matrix(1),
            totalcoverage = 82
          )
        ),
        total_cov = 82,
        functions_no_tests = no_tests_df,
        tests_skipped = character(0),
        tests_passing = c(tf1, tf2)
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  
  # --- Act ---
  expect_message({
    test_pkg_data <- tpd(has_tinytest = FALSE, has_testit = FALSE)
    result <- run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  }, sprintf("running base-style tests coverage for %s", pkg_name))
  
  # --- Assert ---
  expect_identical(captured$pkg_source_path, pkg_source_path)
  expect_identical(captured$pkg_name, pkg_name)
  
  expect_type(result, "list")
  expect_setequal(result$tests_passing, c(tf1, tf2))
  expect_length(result$tests_skipped, 0)
  expect_equal(result$total_cov, 82)
  expect_equal(result$res_cov$coverage$totalcoverage, 82)
})


test_that("returns testthat path when tests/testthat exists", {
  # IMPORTANT: if your function gates on has_testthat, set it TRUE
  test_pkg_data <- tpd(has_testthat = TRUE)
  
  # Make path building predictable
  mockery::stub(get_nstf_test_path, "file.path",
                function(...) paste(..., sep = "/"))
  
  # Normalize path as identity (handles unlimited calls safely)
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  
  # The function should see these two directories as existing
  true_dirs <- c("/pkg/tests", "/pkg/tests/testthat")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  
  # No base .R files in /pkg/tests (so default_tests_root should be omitted)
  mockery::stub(
    get_nstf_test_path, "list.files",
    list_files_fun(list(
      "/pkg/tests" = character(0)
    ))
  )
  
  # Call with the tests/ directory as per the function contract
  res <- get_nstf_test_path(test_pkg_data, "/pkg/tests")
  
  # Expect only the frameworks you want to expose
  expect_named(res, "testthat")
  expect_identical(res$testthat, "/pkg/tests/testthat")
})

test_that("returns testit path when tests/testit exists", {
  test_pkg_data <- tpd(has_testit = TRUE)
  
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  true_dirs <- c("/pkg/tests", "/pkg/tests/testit")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, c("testit", "nonstandard_testit_root"))
  expect_identical(res$testit, "IGNORED/testit")
})

############################################





test_that("includes nonstandard testit root when test-ci and test-cran both exist", {
  test_pkg_data <- tpd(has_testit = TRUE)
  
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  
  true_dirs <- c(
    "/pkg/tests",
    "/pkg/tests/test-ci",
    "/pkg/tests/test-cran"
  )
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  
  # No base tests
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, c("testit", "nonstandard_testit_root"))
  expect_identical(res$nonstandard_testit_root, "IGNORED")
})

# list.files mock that respects pattern and full.names for one directory

# list.files mock that tolerates trailing slash, respects pattern/full.names/recursive,
# and returns filenames that satisfy common base-test patterns (e.g., ^test.*\\.[Rr]$)
list_files_base_tests <- function() {
  function(path, pattern = NULL, full.names = FALSE, recursive = FALSE, ...) {
    # Normalize trailing slash
    norm_path <- sub("/+$", "", path)
    
    # Only emulate for /pkg/tests
    if (!identical(norm_path, "/pkg/tests")) return(character(0))
    
    # Common base R test filenames often start with 'test-'
    files <- c("test-a.R", "test-b.R")
    
    # Apply pattern if provided
    if (!is.null(pattern)) {
      files <- files[grepl(pattern, files, perl = TRUE)]
    }
    
    # If recursive = TRUE, behavior is the same here because files are directly under tests/
    # (adjust if your production code expects nested files)
    
    # Apply full.names if requested
    if (full.names) {
      files <- file.path(path, files)
    }
    
    files
  }
}

test_that("detects base R tests when .R files are directly under tests/", {
  # If your implementation gates on has_tests_base, set TRUE here.
  # If detection is purely by file presence, leaving FALSE is fine.
  test_pkg_data <- tpd(has_tests_base = TRUE)
  
  # Stable paths
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  mockery::stub(get_nstf_test_path, "file.path",
                function(...) paste(..., sep = "/"))
  
  # The function may probe /pkg, /pkg/tests, and sometimes with a trailing slash
  true_dirs <- c("/pkg", "/pkg/tests", "/pkg/tests/")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  
  # Simulate base R tests directly in /pkg/tests
  mockery::stub(get_nstf_test_path, "list.files", list_files_base_tests())
  
  # IMPORTANT: pass "/pkg" (not "IGNORED") so file.path joins match your mocks
  res <- get_nstf_test_path(test_pkg_data, "/pkg")
  
  # Expected: base tests root is detected
  expect_named(res, "base_tests_root")
  expect_identical(res$base_tests_root, "/pkg")
})

test_that("detects Bioconductor unit tests under inst/unitTests", {
  # Use the correct capability flag expected by production
  test_pkg_data <- tpd(has_bioc_unit_tests = TRUE)
  
  # --- Pure/path-stable stubs ---
  # Collapse ".." and "." without touching the filesystem (so the test is deterministic)
  normalizePath_collapse <- function(path, winslash = "/", mustWork = FALSE) {
    collapse_one <- function(p) {
      # Keep leading slash if present
      abs <- startsWith(p, "/")
      parts <- strsplit(p, "/")[[1]]
      stack <- character()
      for (seg in parts) {
        if (seg == "" || seg == ".") next
        if (seg == "..") {
          if (length(stack) > 0) stack <- stack[-length(stack)]
        } else {
          stack <- c(stack, seg)
        }
      }
      out <- paste(stack, collapse = "/")
      if (abs) out <- paste0("/", out)
      if (out == "" && abs) out <- "/"  # root case
      out
    }
    # Support vectors if the SUT loops over paths
    if (length(path) > 1) return(vapply(path, collapse_one, character(1)))
    collapse_one(path)
  }
  
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_collapse)
  mockery::stub(get_nstf_test_path, "file.path", function(...) paste(..., sep = "/"))
  
  # Make only the Bioc unit tests path (and parents) exist.
  # IMPORTANT: Do NOT include "/pkg/tests" here, to avoid default_tests_root detection.
  true_dirs <- c("/pkg", "/pkg/inst", "/pkg/inst/unitTests", "/pkg/inst/unitTests/")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  
  # No base .R files anywhere
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  # Pass the tests directory (correct contract for 'testdir')
  res <- get_nstf_test_path(test_pkg_data, "/pkg/tests")
  
  expect_named(res, "bioc_unitTests")
  expect_identical(res$bioc_unitTests, "/pkg/inst/unitTests")
})

test_that("de-duplicates when both nonstandard_testit_root and base_tests_root resolve to tests/", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  true_dirs <- c("/pkg/tests", "/pkg/tests/test-ci", "/pkg/tests/test-cran")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  # base files present
  files_map <- list("/pkg/tests" = c("/pkg/tests/only_base.R"))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(files_map))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  # Should keep only the first appearance (tinytest)
  expect_length(res, 1L)
  expect_named(res, "default_tests_root")
  expect_identical(res$default_tests_root, "IGNORED")
})

test_that("falls back to default_tests_root when nothing detected", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  mockery::stub(get_nstf_test_path, "file.path",
                function(...) paste(..., sep = "/"))
  
  # No directories exist at all
  true_dirs <- character(0)
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  # Use a concrete root so default path resolves to /pkg/tests
  res <- get_nstf_test_path(test_pkg_data, "/pkg/tests")
  
  expect_named(res, "default_tests_root")
  expect_identical(res$default_tests_root, "/pkg/tests")
})

test_that("flag-driven inclusion: has_testthat = TRUE includes tests/testthat even if dir doesn't exist", {
  test_pkg_data <- tpd(has_testthat = TRUE)
  
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  # Simulate that no directories exist; list.files irrelevant here
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(character(0)))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  # Since flag is TRUE, testthat should be included regardless of FS state.
  expect_named(res, "testthat")
  expect_identical(res$testthat, "IGNORED/testthat")
})

test_that("flag-driven inclusion: has_bioc_unit_tests = TRUE includes inst/unitTests even if dir doesn't exist", {
  test_pkg_data <- tpd(has_bioc_unit_tests = TRUE)
  
  mockery::stub(get_nstf_test_path, "normalizePath", normalizePath_identity)
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(character(0)))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "bioc_unitTests")
  expect_identical(res$bioc_unitTests, "IGNORED/../inst/unitTests")
})

# Helpers ---------------------------------------------------------------
# A fake covr namespace that flips flags in a shared environment
make_fake_covr_ns <- function(flags_env, counters_value, coverage_value) {
  stopifnot(is.environment(flags_env))
  
  covr_ns <- new.env(parent = emptyenv())
  
  # When called, these will flip booleans in flags_env by reference
  assign("trace_environment", function(env) {
    flags_env$trace <- TRUE
    invisible(NULL)
  }, covr_ns)
  
  assign("reset_traces", function() {
    flags_env$reset <- TRUE
    invisible(NULL)
  }, covr_ns)
  
  assign("clear_counters", function() {
    flags_env$clear <- TRUE
    invisible(NULL)
  }, covr_ns)
  
  assign("as_coverage", function(x) coverage_value, covr_ns)
  assign(".counters", counters_value, covr_ns)
  
  covr_ns
}


# A safe dir.exists stub that only returns TRUE for whitelisted paths
dir_exists_only <- function(truthy) {
  function(p) p %in% truthy
}

# ----------------------------------------------------------------------


test_that("runs tinytest coverage successfully and returns structure", {
  pkg_source_path <- "/pkg"
  tiny_dir        <- "/pkg/inst/tinytest"
  pkg_name        <- "mypkg"
  
  # Shared flags environment (modified by reference)
  flags <- new.env(parent = emptyenv())
  flags$trace <- FALSE; flags$reset <- FALSE; flags$clear <- FALSE
  
  # Fake covr env & outputs
  fake_counters <- list(counter_data = 123)
  fake_coverage <- structure(list(kind = "coverage", from = "as_coverage"),
                             class = "coverage")
  fake_covr_ns  <- make_fake_covr_ns(flags,
                                     counters_value = fake_counters,
                                     coverage_value = fake_coverage)
  
  # Fake package namespace env (target to be traced)
  fake_pkg_ns <- new.env(parent = emptyenv())
  
  # ---- Stubs ----
  
  # Dirs exist
  mockery::stub(create_tinytest_coverage, "dir.exists",
                function(p) p %in% c(pkg_source_path, tiny_dir))
  
  # Namespace is "loaded"
  mockery::stub(create_tinytest_coverage, "loadedNamespaces",
                function() c(pkg_name, "covr", "tinytest"))
  
  # Return fake envs for covr & the target pkg
  mockery::stub(create_tinytest_coverage, "asNamespace", function(name) {
    if (identical(name, "covr"))    return(fake_covr_ns)
    if (identical(name, pkg_name))  return(fake_pkg_ns)
    stop("unexpected asNamespace(", name, ")")
  })
  
  # Per-file run: enumerate files, then run each in isolation.
  test_a <- file.path(tiny_dir, "test_a.R")
  test_b <- file.path(tiny_dir, "test_b.R")
  mockery::stub(create_tinytest_coverage, "list.files",
                function(path, pattern, full.names) c(test_a, test_b))
  
  fake_tt_a <- list(results = "a ok")
  fake_tt_b <- list(results = "b ok")
  mockery::stub(create_tinytest_coverage, "tinytest::run_test_file",
                function(file, at_home, color) {
                  expect_true(at_home)
                  expect_false(color)
                  if (identical(file, test_a)) fake_tt_a else fake_tt_b
                })
  
  # Ensure NOT_CRAN is restored (set to a known value before)
  Sys.setenv(NOT_CRAN = "OLD_VALUE")
  on.exit(Sys.setenv(NOT_CRAN = ""), add = TRUE)
  
  # ---- Run ----
  res <- create_tinytest_coverage(pkg_source_path, pkg_name, tiny_dir = tiny_dir)
  
  # ---- Assertions ----
  expect_type(res, "list")
  expect_true(all(c("coverage", "tinytests", "test_files", "file_status") %in% names(res)))
  expect_identical(res$coverage, fake_coverage)
  
  # Per-file results keyed by full path; both files ran OK.
  expect_identical(res$test_files, sort(c(test_a, test_b)))
  expect_identical(unname(res$file_status[c(test_a, test_b)]), c("ok", "ok"))
  expect_identical(res$tinytests[[test_a]], fake_tt_a)
  expect_identical(res$tinytests[[test_b]], fake_tt_b)
  
  # covr hooks invoked
  expect_true(isTRUE(flags$trace))
  expect_true(isTRUE(flags$reset))
  expect_true(isTRUE(flags$clear))
  
  # NOT_CRAN restored to prior value
  expect_identical(Sys.getenv("NOT_CRAN"), "OLD_VALUE")
})

test_that("per-file isolation: one file's fatal error does not stop the rest", {
  pkg_source_path <- "/pkg"
  tiny_dir        <- "/pkg/inst/tinytest"
  pkg_name        <- "mypkg"
  
  # Shared flags
  flags <- new.env(parent = emptyenv())
  flags$trace <- FALSE; flags$reset <- FALSE; flags$clear <- FALSE
  
  fake_counters <- list(counter_data = 999)
  fake_coverage <- structure(list(kind = "coverage"), class = "coverage")
  fake_covr_ns  <- make_fake_covr_ns(flags,
                                     counters_value = fake_counters,
                                     coverage_value = fake_coverage)
  fake_pkg_ns <- new.env(parent = emptyenv())
  
  # Dirs exist
  mockery::stub(create_tinytest_coverage, "dir.exists",
                function(p) p %in% c(pkg_source_path, tiny_dir))
  
  # Namespace is "loaded"
  mockery::stub(create_tinytest_coverage, "loadedNamespaces",
                function() c(pkg_name, "covr", "tinytest"))
  
  # Return fake envs
  mockery::stub(create_tinytest_coverage, "asNamespace", function(name) {
    if (identical(name, "covr"))    return(fake_covr_ns)
    if (identical(name, pkg_name))  return(fake_pkg_ns)
    stop("unexpected asNamespace(", name, ")")
  })
  
  # Two files; the first throws a fatal top-level error (mimics Rcpp
  # test_system.R), the second still runs.
  test_bad  <- file.path(tiny_dir, "test_bad.R")
  test_good <- file.path(tiny_dir, "test_good.R")
  mockery::stub(create_tinytest_coverage, "list.files",
                function(path, pattern, full.names) c(test_bad, test_good))
  
  fake_tt_good <- list(results = "good ok")
  mockery::stub(create_tinytest_coverage, "tinytest::run_test_file",
                function(file, at_home, color) {
                  if (identical(file, test_bad)) stop("tinytest boom")
                  fake_tt_good
                })
  
  # Preserve & check NOT_CRAN restoration
  Sys.unsetenv("NOT_CRAN")
  on.exit(Sys.unsetenv("NOT_CRAN"), add = TRUE)
  
  # The bad file's error is caught and the run continues.
  expect_message(
    res <- create_tinytest_coverage(pkg_source_path, pkg_name, tiny_dir = tiny_dir),
    regexp = "stopped early"
  )
  
  # Coverage is computed from accumulated counters; status reflects per file.
  expect_type(res, "list")
  expect_identical(res$coverage, fake_coverage)
  expect_true(startsWith(res$file_status[[test_bad]], "error:"))
  expect_identical(res$file_status[[test_good]], "ok")
  expect_identical(res$tinytests[[test_good]], fake_tt_good)
  expect_null(res$tinytests[[test_bad]])
  
  # Cleanup still ran (trace before, reset/clear via on.exit)
  expect_true(isTRUE(flags$trace))
  expect_true(isTRUE(flags$reset))
  expect_true(isTRUE(flags$clear))
  
  # NOT_CRAN restored to unset (since it was unset before)
  expect_identical(Sys.getenv("NOT_CRAN", unset = NA), NA_character_)
})


test_that("tinytest branch: runner basenames are removed from test files", {
  
  run_fn <- getFromNamespace("run_covr_skip_nstf", "test.assessr")
  
  # Arrange: real package dir and tests/
  pkg_source_path <- file.path(tempdir(), "mockpkg_tiny_runner_basenames")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  
  # Mapping contains three runner basenames + two real tests
  mapping <- data.frame(
    source_file = c("x.R",         "y.R",           "z.R",              "a.R",         "b.R"),
    test_file   = c("tinytest.R",  "runTinyTests.R","run_tinytest.R",   "test-a.R",    "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  problems <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  no_tests_df   <- data.frame(source_file = character(0), stringsAsFactors = FALSE)
  coverage_obj  <- structure(list(), class = "coverage")
  
  # Expected paths after runner filtering
  exp_a <- file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
  exp_b <- file.path(pkg_source_path, "inst", "tinytest", "test-b.R")
  
  # ---- Stubs ----
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  
  mockery::stub(
    run_fn, "get_nstf_test_path",
    function(...) list(tinytest = file.path(pkg_source_path, "tests"))
  )
  
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf",          function(...) problems)
  mockery::stub(run_fn, "get_function_no_tests",         function(...) no_tests_df)
  
  # All tinytest files "exist"
  mockery::stub(run_fn, "file.exists", function(paths) TRUE)
  
  # Loader stubs
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace",       function(name) fake_ns)
  mockery::stub(run_fn, "ls",                function(...) character(0))
  mockery::stub(run_fn, "get",               function(obj, envir) obj)
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_package_datasets",     function(...) character(0))
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  # Tinytest coverage path
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(coverage = coverage_obj, tinytests = list(ok = TRUE))
  )
  
  mockery::stub(
    run_fn, "covr::coverage_to_list",
    function(...) list(filecoverage = matrix(1), totalcoverage = 88)
  )
  
  # CRITICAL: Fake coverage normalization so result$total_cov == 88
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      
      # Validate inside stub: runner files must not remain
      if (any(basename(test_files_clean) %in% c("tinytest.R",
                                                "runTinyTests.R",
                                                "run_tinytest.R"))) {
        stop("Runner basename was NOT removed!")
      }
      
      list(
        res_cov = list(
          name = pkg_name,
          coverage = list(
            filecoverage  = matrix(1),
            totalcoverage = 88
          )
        ),
        total_cov           = 88,
        functions_no_tests  = no_tests_df,
        tests_skipped       = character(0),
        tests_passing       = c(exp_a, exp_b)
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Act ----
  test_pkg_data <- tpd(has_tinytest = TRUE)
  result <- run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assert ----
  expect_type(result, "list")
  
  expect_setequal(result$tests_passing, c(exp_a, exp_b))
  expect_length(result$tests_skipped, 0L)
  
  expect_equal(result$total_cov, 88)
  expect_equal(result$res_cov$coverage$totalcoverage, 88)
})



test_that("tinytest branch: runner basenames are removed from test files", {
  
  run_fn <- getFromNamespace("run_covr_skip_nstf", "test.assessr")
  
  # Arrange: real package dir
  pkg_source_path <- file.path(tempdir(), "mockpkg_tiny_runner_basenames")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  
  mapping <- data.frame(
    source_file = c("x.R",         "y.R",           "z.R",              "a.R",         "b.R"),
    test_file   = c("tinytest.R",  "runTinyTests.R","run_tinytest.R",   "test-a.R",    "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  problems <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  no_tests_df   <- data.frame(source_file = character(0), stringsAsFactors = FALSE)
  coverage_obj  <- structure(list(), class = "coverage")
  
  exp_a <- file.path(pkg_source_path, "inst", "tinytest", "test-a.R")
  exp_b <- file.path(pkg_source_path, "inst", "tinytest", "test-b.R")
  
  # ---- Stubs ----
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  
  mockery::stub(
    run_fn, "get_nstf_test_path",
    function(...) list(tinytest = file.path(pkg_source_path, "tests"))
  )
  
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf",          function(...) problems)
  mockery::stub(run_fn, "get_function_no_tests",         function(...) no_tests_df)
  
  mockery::stub(run_fn, "file.exists", function(paths) TRUE)
  
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace",       function(name) fake_ns)
  mockery::stub(run_fn, "ls",                function(...) character(0))
  mockery::stub(run_fn, "get",               function(obj, envir) obj)
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_package_datasets",     function(...) character(0))
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(coverage = coverage_obj, tinytests = list(ok = TRUE))
  )
  
  mockery::stub(
    run_fn, "covr::coverage_to_list",
    function(...) list(filecoverage = matrix(1), totalcoverage = 77)
  )
  
  # CRITICAL FIX — add this for your 77% case
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      
      # Verify runners are removed
      if (any(basename(test_files_clean) %in% c("tinytest.R",
                                                "runTinyTests.R",
                                                "run_tinytest.R"))) {
        stop("Runner basename was NOT removed!")
      }
      
      list(
        res_cov = list(
          name = pkg_name,
          coverage = list(
            filecoverage  = matrix(1),
            totalcoverage = 77
          )
        ),
        total_cov           = 77,
        functions_no_tests  = no_tests_df,
        tests_skipped       = character(0),
        tests_passing       = c(exp_a, exp_b)
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Act ----
  test_pkg_data <- tpd(has_tinytest = TRUE)
  result <- run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  
  # ---- Assert ----
  expect_type(result, "list")
  expect_setequal(result$tests_passing, c(exp_a, exp_b))
  expect_length(result$tests_skipped, 0L)
  expect_equal(result$total_cov, 77)
  expect_equal(result$res_cov$coverage$totalcoverage, 77)
})


# A fake 'covr' namespace for create_base_tests_coverage() tests.
# Uses a shared environment (flags_env) to flip flags by reference so tests can assert calls.
make_fake_covr_ns_base <- function(flags_env, counters_value, coverage_value) {
  stopifnot(is.environment(flags_env))
  covr_ns <- new.env(parent = emptyenv())
  
  assign("trace_environment", function(env) {
    flags_env$trace <- TRUE
    invisible(NULL)
  }, covr_ns)
  
  assign("reset_traces", function() {
    flags_env$reset <- TRUE
    invisible(NULL)
  }, covr_ns)
  
  assign("clear_counters", function() {
    flags_env$clear <- TRUE
    invisible(NULL)
  }, covr_ns)
  
  assign("as_coverage", function(x) coverage_value, covr_ns)
  assign(".counters", counters_value, covr_ns)
  
  covr_ns
}


# Helper: a normalizePath stub that doesn’t error with mustWork=TRUE
normalizePath_identity_base <- function(path, ..., winslash = "/", mustWork = FALSE) path


# Helper: extract argument from ... in sys.source/source stubs
get_arg <- function(..., name, default = NULL) {
  a <- list(...)
  if (!is.null(a[[name]])) return(a[[name]])
  default
}

test_that("errors if package namespace is not loaded", {
  pkg_source_path <- "/pkg"
  pkg_name <- "mypkg"
  test_files <- c("/pkg/tests/a.R", "/pkg/tests/b.R")
  
  mockery::stub(create_base_tests_coverage, "loadedNamespaces", function() c("covr"))
  
  expect_error(
    create_base_tests_coverage(pkg_source_path, pkg_name, test_files),
    regexp = "Namespace 'mypkg' is not loaded",
    fixed  = FALSE
  )
})

test_that("traces ns, sources each file, returns coverage (happy path)", {
  pkg_source_path <- "/pkg"
  pkg_name <- "mypkg"
  test_files <- c("/pkg/tests/a.R", "/pkg/tests/b.R")
  
  # Flags that flip when covr hooks are called
  flags <- new.env(parent = emptyenv())
  flags$trace <- FALSE; flags$reset <- FALSE; flags$clear <- FALSE
  
  fake_counters <- list(n = 2)
  fake_coverage <- structure(list(total = 100), class = "coverage")
  fake_covr_ns  <- make_fake_covr_ns_base(flags, fake_counters, fake_coverage)
  
  # A fake pkg namespace to trace
  fake_pkg_ns <- new.env(parent = emptyenv())
  
  # ---- Stubs ----
  mockery::stub(create_base_tests_coverage, "loadedNamespaces",
                function() c(pkg_name, "covr"))
  
  mockery::stub(create_base_tests_coverage, "asNamespace", function(name) {
    if (identical(name, "covr"))   return(fake_covr_ns)
    if (identical(name, pkg_name)) return(fake_pkg_ns)
    stop("unexpected asNamespace(", name, ")")
  })
  
  mockery::stub(create_base_tests_coverage, "normalizePath",
                function(path, ..., winslash = "/", mustWork = TRUE) path)
  
  # Robust sys.source stub: handle both named and positional args.
  sourced <- character(0)
  source_stub <- function(...) {
    args <- list(...)
    file  <- if (!is.null(args$file)) args$file else args[[1]]
    envir <- if (!is.null(args$envir)) args$envir else NULL
    chdir <- if (!is.null(args$chdir)) args$chdir else NULL
    
    expect_true(file %in% test_files)
    expect_true(is.environment(envir))
    expect_identical(parent.env(envir), fake_pkg_ns)
    expect_true(isTRUE(chdir))
    
    sourced <<- c(sourced, file)
    invisible(NULL)
  }
  
  # IMPORTANT: stub both unqualified and namespaced calls
  mockery::stub(create_base_tests_coverage, "sys.source",        source_stub)
  mockery::stub(create_base_tests_coverage, "base::sys.source",  source_stub)
  
  # ---- Run ----
  res <- create_base_tests_coverage(pkg_source_path, pkg_name, test_files)
  
  # ---- Assertions ----
  expect_type(res, "list")
  expect_true("coverage" %in% names(res))
  expect_identical(res$coverage, fake_coverage)
  
  # Now both files should have been captured
  expect_setequal(sourced, test_files)
  
  # covr hooks invoked
  expect_true(isTRUE(flags$trace))
  expect_true(isTRUE(flags$reset))
  expect_true(isTRUE(flags$clear))
})

test_that("continues if a test file errors (try silent), still returns coverage and runs cleanup", {
  pkg_source_path <- "/pkg"
  pkg_name <- "mypkg"
  test_files <- c("/pkg/tests/a.R", "/pkg/tests/b.R")
  
  flags <- new.env(parent = emptyenv())
  flags$trace <- FALSE; flags$reset <- FALSE; flags$clear <- FALSE
  
  fake_counters <- list(n = 1)
  fake_coverage <- structure(list(total = 80), class = "coverage")
  fake_covr_ns  <- make_fake_covr_ns_base(flags, fake_counters, fake_coverage)
  fake_pkg_ns   <- new.env(parent = emptyenv())
  
  mockery::stub(create_base_tests_coverage, "loadedNamespaces",
                function() c(pkg_name, "covr"))
  
  mockery::stub(create_base_tests_coverage, "asNamespace", function(name) {
    if (identical(name, "covr"))   return(fake_covr_ns)
    if (identical(name, pkg_name)) return(fake_pkg_ns)
    stop("unexpected asNamespace(", name, ")")
  })
  
  # Must accept winslash/mustWork to be safe
  mockery::stub(create_base_tests_coverage, "normalizePath",
                function(path, ..., winslash = "/", mustWork = TRUE) path)
  
  sourced <- character(0)
  
  # Robust stub: handle named or positional 'file', ensure we append the actual path
  source_stub <- function(...) {
    args <- list(...)
    file  <- if (!is.null(args$file)) args$file else args[[1]]
    # First file errors; second succeeds
    if (identical(file, test_files[[1]])) stop("boom from a.R")
    sourced <<- c(sourced, file)
    invisible(NULL)
  }
  
  # Stub both variants to be safe
  mockery::stub(create_base_tests_coverage, "sys.source",       source_stub)
  mockery::stub(create_base_tests_coverage, "base::sys.source", source_stub)
  
  res <- create_base_tests_coverage(pkg_source_path, pkg_name, test_files)
  
  expect_type(res, "list")
  expect_true("coverage" %in% names(res))
  expect_identical(res$coverage, fake_coverage)
  # Only the second file should have been recorded
  expect_identical(sourced, test_files[[2]])
  
  expect_true(isTRUE(flags$trace))
  expect_true(isTRUE(flags$reset))
  expect_true(isTRUE(flags$clear))
})

test_that("empty test_files still traces/cleans and returns coverage", {
  pkg_source_path <- "/pkg"
  pkg_name <- "mypkg"
  test_files <- character(0)
  
  flags <- new.env(parent = emptyenv())
  flags$trace <- FALSE; flags$reset <- FALSE; flags$clear <- FALSE
  
  fake_counters <- list(n = 0)
  fake_coverage <- structure(list(total = 0), class = "coverage")
  fake_covr_ns  <- make_fake_covr_ns_base(flags, fake_counters, fake_coverage)
  fake_pkg_ns   <- new.env(parent = emptyenv())
  
  mockery::stub(create_base_tests_coverage, "loadedNamespaces",
                function() c(pkg_name, "covr"))
  
  mockery::stub(create_base_tests_coverage, "asNamespace", function(name) {
    if (identical(name, "covr"))   return(fake_covr_ns)
    if (identical(name, pkg_name)) return(fake_pkg_ns)
    stop("unexpected asNamespace(", name, ")")
  })
  
  mockery::stub(create_base_tests_coverage, "normalizePath", normalizePath_identity_base )
  mockery::stub(create_base_tests_coverage, "sys.source", function(...) {
    stop("sys.source should not be called for empty test_files")
  })
  
  res <- create_base_tests_coverage(pkg_source_path, pkg_name, test_files)
  
  expect_type(res, "list")
  expect_true("coverage" %in% names(res))
  expect_identical(res$coverage, fake_coverage)
  
  expect_true(isTRUE(flags$trace))
  expect_true(isTRUE(flags$reset))
  expect_true(isTRUE(flags$clear))
})

test_that("restrict_test_paths_to_framework returns single path when framework matches", {
  # Mocked multi-framework path list (as from get_nstf_test_path)
  multi_path_list <- list(
    testthat = "/pkg/tests/testthat",
    testit = "/pkg/tests/testit",
    base_tests_root = "/pkg/tests",
    tinytest = "/pkg/inst/tinytest"
  )
  fn <- test.assessr:::restrict_test_paths_to_framework
  
  expect_equal(fn(multi_path_list, "testit"), list(testit = "/pkg/tests/testit"))
  expect_equal(fn(multi_path_list, "tests_base"), list(base_tests_root = "/pkg/tests"))
  expect_equal(fn(multi_path_list, "tinytest"), list(tinytest = "/pkg/inst/tinytest"))
})

test_that("restrict_test_paths_to_framework returns full list when framework is NULL or empty", {
  multi_path_list <- list(
    testthat = "/pkg/tests/testthat",
    testit = "/pkg/tests/testit"
  )
  fn <- test.assessr:::restrict_test_paths_to_framework
  
  expect_identical(fn(multi_path_list, NULL), multi_path_list)
  expect_identical(fn(multi_path_list, ""), multi_path_list)
})

test_that("restrict_test_paths_to_framework returns full list when framework is unknown", {
  multi_path_list <- list(testthat = "/pkg/tests/testthat", testit = "/pkg/tests/testit")
  fn <- test.assessr:::restrict_test_paths_to_framework
  
  expect_identical(fn(multi_path_list, "unknown"), multi_path_list)
  expect_identical(fn(multi_path_list, "runit"), multi_path_list)
})

test_that("restrict_test_paths_to_framework falls back to nonstandard_testit_root when testit missing", {
  # Nonstandard testit layout: test-ci + test-cran, no tests/testit
  # Mocked path list has nonstandard_testit_root but no testit (or testit absent)
  fn <- test.assessr:::restrict_test_paths_to_framework
  
  # Case: testit key absent, nonstandard_testit_root present
  path_list_no_testit <- list(
    nonstandard_testit_root = "/pkg/tests",
    testthat = "/pkg/tests/testthat"
  )
  expect_equal(
    fn(path_list_no_testit, "testit"),
    list(nonstandard_testit_root = "/pkg/tests")
  )
  
  # Case: testit explicitly NULL, nonstandard_testit_root present
  path_list_testit_null <- list(
    testit = NULL,
    nonstandard_testit_root = "/pkg/tests"
  )
  expect_equal(
    fn(path_list_testit_null, "testit"),
    list(nonstandard_testit_root = "/pkg/tests")
  )
})

test_that("restrict_test_paths_to_framework prefers testit over nonstandard_testit_root when both present", {
  # Standard testit: tests/testit exists, nonstandard also present
  path_list <- list(
    testit = "/pkg/tests/testit",
    nonstandard_testit_root = "/pkg/tests"
  )
  fn <- test.assessr:::restrict_test_paths_to_framework
  
  expect_equal(fn(path_list, "testit"), list(testit = "/pkg/tests/testit"))
})

test_that("restrict_test_paths_to_framework maps testthat framework", {
  multi_path_list <- list(
    testthat = "/pkg/tests/testthat",
    testit = "/pkg/tests/testit"
  )
  fn <- test.assessr:::restrict_test_paths_to_framework
  
  expect_equal(fn(multi_path_list, "testthat"), list(testthat = "/pkg/tests/testthat"))
})



test_that("restrict_test_paths_to_framework returns full list when framework path is missing", {
  # testit requested but not present in list
  path_list <- list(testthat = "/pkg/tests/testthat")
  fn <- test.assessr:::restrict_test_paths_to_framework
  
  expect_identical(fn(path_list, "testit"), path_list)
})


test_that("restrict_test_paths_to_framework works via run_covr_skip_nstf with stubbed get_nstf_test_path", {
  
  run_fn <- getFromNamespace("run_covr_skip_nstf", "test.assessr")
  
  # Prepare a real directory structure
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "testit"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  # Multi-path returned by get_nstf_test_path()
  multi_path_list <- list(
    testthat = file.path(pkg_source_path, "tests", "testthat"),
    testit   = file.path(pkg_source_path, "tests", "testit")
  )
  
  mapping <- data.frame(
    source_file = "foo.R",
    test_file   = "test-foo.R",
    stringsAsFactors = FALSE
  )
  
  problems <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  fake_coverage      <- structure(list(), class = "coverage")
  fake_coverage_list <- list(filecoverage = matrix(1), totalcoverage = 50)
  
  # ---- Required Stubs ----
  mockery::stub(run_fn, "get_pkg_name", "mockpkg")
  
  mockery::stub(run_fn, "get_nstf_test_path", function(...) multi_path_list)
  
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_testit", function(...) problems)
  mockery::stub(run_fn, "get_function_no_tests", data.frame())
  
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  
  # Loader stubs
  mockery::stub(run_fn, "pkgload::load_all", NULL)
  mockery::stub(run_fn, "asNamespace",       function(...) new.env(parent = emptyenv()))
  mockery::stub(run_fn, "ls",                function(...) character(0))
  mockery::stub(run_fn, "get",               function(obj, envir) obj)
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_package_datasets",     function(...) character(0))
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE) # Prevent tcltk failures
  
  # Force coverage path
  mockery::stub(run_fn, "covr::environment_coverage", function(...) fake_coverage)
  mockery::stub(run_fn, "covr::coverage_to_list",     function(...) fake_coverage_list)
  
  # ---- CRITICAL FIX: deterministic nstf coverage normalization ----
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      # Validate: only testit test path should remain
      expect_true(all(grepl("testit", test_files_clean)))
      list(
        res_cov = list(
          name = "mockpkg",
          coverage = list(
            filecoverage  = matrix(1),
            totalcoverage = 50
          )
        ),
        total_cov = 50,
        functions_no_tests = data.frame(),
        tests_skipped      = character(0),
        tests_passing      = test_files_clean
      )
    }
  )
  
  # cleanup no-op
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  test_pkg_data <- list(has_testit = TRUE)
  cov_env       <- new.env(parent = emptyenv())
  
  # ---- Act ----
  result <- run_fn(
    pkg_source_path,
    test_pkg_data,
    cov_env     = cov_env,
    framework   = "testit"  # restrict to testit
  )
  
  # ---- Assert ----
  expect_type(result, "list")
  expect_true("total_cov" %in% names(result))
  expect_equal(result$total_cov, 50)
})



test_that("create_nstf_covr_list returns structured list on happy path", {
  # Get the function from your package namespace
  fn <- getFromNamespace("create_nstf_covr_list", "test.assessr")
  
  # --- Inputs ---
  coverage <- list(package = "mockpkg")   # used by res_cov$name
  pkg_name <- "mockpkg"
  
  no_tests_df      <- data.frame(`function` = c("f1", "f2"), stringsAsFactors = FALSE)
  test_files_clean <- c("tests/testit/test-a.R", "tests/testit/test-b.R")
  skip_tests       <- c("tests/testit/test-skip.R")
  problems         <- data.frame(test_file = "tests/testit/test-a.R",
                                 issue_type = "none",
                                 stringsAsFactors = FALSE)
  
  # What covr::coverage_to_list should return (happy path)
  covr_list_mock <- list(
    filecoverage   = matrix(1, nrow = 1, dimnames = list("fileA.R", NULL)),
    totalcoverage  = 72.345
  )
  
  # mock coverage_to_list to return our fake list
  covr_to_list_mock <- function(coverage) covr_list_mock
  
  # mock compute_total_coverage and record calls
  # We'll assert on the args later.
  compute_total_mock <- mockery::mock(list(
    total_cov          = 88.8,
    percent_cov        = 72.35,
    n_testfiles        = 4L,
    n_no_function_tests= 1L,
    n_skipped_files    = 2L
  ))
  
  # Stub the symbols as referenced in the function body
  mockery::stub(fn, "covr::coverage_to_list", covr_to_list_mock)
  mockery::stub(fn, "compute_total_coverage", compute_total_mock)
  
  # Run and assert it emits a friendly top-line coverage message
  expect_message(
    result <- fn(
      coverage         = coverage,
      pkg_name         = pkg_name,
      no_tests_df      = no_tests_df,
      test_files_clean = test_files_clean,
      skip_tests       = skip_tests,
      problems         = problems
    ),
    regexp = "Total Coverage for mockpkg is:",
    fixed  = FALSE
  )
  
  # --- Assert compute_total_coverage was called with correct arguments ---
  mockery::expect_called(compute_total_mock, 1)
  # Argument 1 should be as.numeric(totalcoverage) from covr_list_mock
  mockery::expect_args(
    compute_total_mock, 1,
    as.numeric(covr_list_mock$totalcoverage),
    no_tests_df,
    test_files_clean,
    skip_tests
  )
  
  # --- Assert return structure and values ---
  expect_type(result, "list")
  expect_true(all(c("total_cov","res_cov","functions_no_tests",
                    "tests_skipped","tests_passing","tests_problems") %in% names(result)))
  
  # Values from the mocked compute_total_coverage()
  expect_equal(result$total_cov, 88.8)
  
  # res_cov fields
  expect_type(result$res_cov, "list")
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage, covr_list_mock)
  expect_true(is.na(result$res_cov$errors))
  expect_true(is.na(result$res_cov$notes))
  
  # Pass-throughs
  expect_identical(result$functions_no_tests, no_tests_df)
  expect_identical(result$tests_skipped,      skip_tests)
  expect_identical(result$tests_passing,      test_files_clean)
  expect_identical(result$tests_problems,     problems)
})





test_that("create_nstf_covr_list handles coverage_to_list() error branch", {
  
  # ------------------------------------------
  # Arrange mock inputs
  # ------------------------------------------
  fake_coverage <- list(package = "mockpkg")  # must have $package
  class(fake_coverage) <- "coverage"
  
  no_tests_df <- data.frame(
    source_file = "x.R",
    stringsAsFactors = FALSE
  )
  
  test_files_clean <- c("test-x.R")
  skip_tests <- character(0)
  problems <- data.frame(
    test_file = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  # ------------------------------------------
  # Stub covr::coverage_to_list to FORCE ERROR
  # ------------------------------------------
  mockery::stub(
    create_nstf_covr_list,
    "covr::coverage_to_list",
    function(...) stop("Synthetic coverage_to_list error")
  )
  
  # ------------------------------------------
  # Stub compute_total_coverage so test runs to completion
  # Returning predictable output
  # ------------------------------------------
  mockery::stub(
    create_nstf_covr_list,
    "compute_total_coverage",
    list(
      total_cov = 0,
      percent_cov = 0,
      n_testfiles = length(test_files_clean),
      n_no_function_tests = nrow(no_tests_df),
      n_skipped_files = length(skip_tests)
    )
  )
  
  # ------------------------------------------
  # Act: run the function under test
  # ------------------------------------------
  result <- create_nstf_covr_list(
    coverage        = fake_coverage,
    pkg_name        = "mockpkg",
    no_tests_df     = no_tests_df,
    test_files_clean = test_files_clean,
    skip_tests      = skip_tests,
    problems        = problems
  )
  
  # ------------------------------------------
  # Assert: verify that error branch executed
  # ------------------------------------------
  
  # Top level structure
  expect_type(result, "list")
  expect_true("res_cov" %in% names(result))
  expect_true("total_cov" %in% names(result))
  
  # Check error-handling coverage list
  expect_equal(result$res_cov$name, "mockpkg")
  
  # coverage_list should contain NA values (error block)
  expect_true(is.na(result$res_cov$coverage$filecoverage))
  expect_true(is.na(result$res_cov$coverage$totalcoverage))
  
  # errors must contain the thrown condition
  expect_true(inherits(result$res_cov$errors, "error"))
  expect_match(conditionMessage(result$res_cov$errors),
               "Synthetic coverage_to_list error")
  
  # notes = NA (as in error branch)
  expect_true(is.na(result$res_cov$notes))
})


# ---------------------------------------------------------------------------
# run_covr_skip_nstf(): per-file `file_status` refinement (tinytest branch)
#
# These tests exercise the bookkeeping block that runs when
# create_tinytest_coverage() returns a `file_status` vector keyed by full file
# path. The block re-derives `test_files_clean`, `skip_tests`, and `problems`
# from the actual per-file run results, so we capture the arguments that flow
# into create_nstf_covr_list() to assert the refinement happened.
#
# All inputs are mocked (mockery::stub), no real package is loaded, the global
# environment is never touched, and temp dirs are removed on exit.
# ---------------------------------------------------------------------------

test_that("file_status with all 'ok' sets test_files_clean to ok paths and leaves problems untouched", {
  
  run_fn <- run_covr_skip_nstf
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_fs_ok")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  tt_dir      <- file.path(pkg_source_path, "inst", "tinytest")
  test_a_path <- file.path(tt_dir, "test-a.R")
  test_b_path <- file.path(tt_dir, "test-b.R")
  
  mapping <- data.frame(
    source_file = c("x.R", "y.R"),
    test_file   = c("test-a.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  problems_in <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  file_status <- stats::setNames(c("ok", "ok"), c(test_a_path, test_b_path))
  
  # ---- Stubs (hermetic) ----
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(run_fn, "get_nstf_test_path",
                function(...) list(tinytest = file.path(pkg_source_path, "tests")))
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  mockery::stub(run_fn, "file.exists", function(paths) rep(TRUE, length(paths)))
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf", function(...) problems_in)
  mockery::stub(run_fn, "get_function_no_tests",
                function(...) data.frame(source_file = character(0), stringsAsFactors = FALSE))
  
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace", function(name) fake_ns)
  mockery::stub(run_fn, "ls", function(...) character(0))
  mockery::stub(run_fn, "get", function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets", function(...) character(0))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(
      coverage    = list(x = 1),
      tinytests   = list(),
      test_files  = c(test_a_path, test_b_path),
      file_status = file_status
    )
  )
  
  captured <- new.env(parent = emptyenv())
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      captured$test_files_clean <- test_files_clean
      captured$skip_tests       <- skip_tests
      captured$problems         <- problems
      list(
        res_cov   = list(name = pkg_name,
                         coverage = list(filecoverage = matrix(1), totalcoverage = 50)),
        total_cov = 50
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  test_pkg_data <- tpd(has_tinytest = TRUE)
  
  result <- suppressMessages(
    run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  )
  
  # ok paths become test_files_clean; no errors => no skips; problems unchanged
  expect_setequal(captured$test_files_clean, c(test_a_path, test_b_path))
  expect_length(captured$skip_tests, 0L)
  expect_identical(captured$problems, problems_in)
  expect_equal(result$total_cov, 50)
})


test_that("file_status with an error and only 'No tests skipped' problems replaces problems with the error df", {
  
  run_fn <- run_covr_skip_nstf
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_fs_err_noskip")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  tt_dir      <- file.path(pkg_source_path, "inst", "tinytest")
  test_a_path <- file.path(tt_dir, "test-a.R")
  test_b_path <- file.path(tt_dir, "test-b.R")
  
  mapping <- data.frame(
    source_file = c("x.R", "y.R"),
    test_file   = c("test-a.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  # Only the placeholder "No tests skipped" row => no_skip is TRUE
  problems_in <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  file_status <- stats::setNames(c("ok", "error: boom"), c(test_a_path, test_b_path))
  
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(run_fn, "get_nstf_test_path",
                function(...) list(tinytest = file.path(pkg_source_path, "tests")))
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  mockery::stub(run_fn, "file.exists", function(paths) rep(TRUE, length(paths)))
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf", function(...) problems_in)
  mockery::stub(run_fn, "get_function_no_tests",
                function(...) data.frame(source_file = character(0), stringsAsFactors = FALSE))
  
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace", function(name) fake_ns)
  mockery::stub(run_fn, "ls", function(...) character(0))
  mockery::stub(run_fn, "get", function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets", function(...) character(0))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(
      coverage    = list(x = 1),
      tinytests   = list(),
      test_files  = c(test_a_path),
      file_status = file_status
    )
  )
  
  captured <- new.env(parent = emptyenv())
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      captured$test_files_clean <- test_files_clean
      captured$skip_tests       <- skip_tests
      captured$problems         <- problems
      list(
        res_cov   = list(name = pkg_name,
                         coverage = list(filecoverage = matrix(1), totalcoverage = 40)),
        total_cov = 40
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  test_pkg_data <- tpd(has_tinytest = TRUE)
  
  result <- suppressMessages(
    run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  )
  
  expect_identical(captured$test_files_clean, test_a_path)
  expect_identical(captured$skip_tests, "test-b.R")
  
  # problems is replaced wholesale by the per-file error data frame
  expect_s3_class(captured$problems, "data.frame")
  expect_equal(nrow(captured$problems), 1L)
  expect_identical(captured$problems$test_file, "test-b.R")
  expect_identical(captured$problems$issue_type, "error: boom")
  expect_equal(result$total_cov, 40)
})


test_that("file_status with an error and pre-existing skips appends the error df to problems", {
  
  run_fn <- run_covr_skip_nstf
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_fs_err_rbind")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  tt_dir      <- file.path(pkg_source_path, "inst", "tinytest")
  test_a_path <- file.path(tt_dir, "test-a.R")
  test_b_path <- file.path(tt_dir, "test-b.R")
  
  mapping <- data.frame(
    source_file = c("x.R", "y.R"),
    test_file   = c("test-a.R", "test-b.R"),
    stringsAsFactors = FALSE
  )
  
  # A genuine skip already present => no_skip is FALSE => rbind path
  problems_in <- data.frame(
    test_file  = "test-c.R",
    issue_type = "skipped",
    stringsAsFactors = FALSE
  )
  
  file_status <- stats::setNames(c("ok", "error: kaboom"), c(test_a_path, test_b_path))
  
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(run_fn, "get_nstf_test_path",
                function(...) list(tinytest = file.path(pkg_source_path, "tests")))
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  mockery::stub(run_fn, "file.exists", function(paths) rep(TRUE, length(paths)))
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf", function(...) problems_in)
  mockery::stub(run_fn, "get_function_no_tests",
                function(...) data.frame(source_file = character(0), stringsAsFactors = FALSE))
  
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace", function(name) fake_ns)
  mockery::stub(run_fn, "ls", function(...) character(0))
  mockery::stub(run_fn, "get", function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets", function(...) character(0))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(
      coverage    = list(x = 1),
      tinytests   = list(),
      test_files  = c(test_a_path),
      file_status = file_status
    )
  )
  
  captured <- new.env(parent = emptyenv())
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      captured$test_files_clean <- test_files_clean
      captured$skip_tests       <- skip_tests
      captured$problems         <- problems
      list(
        res_cov   = list(name = pkg_name,
                         coverage = list(filecoverage = matrix(1), totalcoverage = 30)),
        total_cov = 30
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  test_pkg_data <- tpd(has_tinytest = TRUE)
  
  result <- suppressMessages(
    run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  )
  
  expect_identical(captured$test_files_clean, test_a_path)
  expect_identical(captured$skip_tests, "test-b.R")
  
  # error df is appended to the existing skips (test-c.R first, then test-b.R)
  expect_s3_class(captured$problems, "data.frame")
  expect_equal(nrow(captured$problems), 2L)
  expect_identical(captured$problems$test_file, c("test-c.R", "test-b.R"))
  expect_identical(captured$problems$issue_type, c("skipped", "error: kaboom"))
  expect_equal(result$total_cov, 30)
})


# ---------------------------------------------------------------------------
# prepare_tinytest_dev_includes(): full-coverage suite
#
# These tests cover every branch of the helper without touching the real
# process environment or filesystem. dir.exists()/normalizePath() and the
# Sys.* env accessors are stubbed (mockery::stub) against a local in-memory
# store, so nothing leaks into .GlobalEnv or the user's environment and there
# is nothing to clean up on disk.
# ---------------------------------------------------------------------------

test_that("prepare_tinytest_dev_includes returns a no-op for invalid pkg_source_path", {
  
  prep <- prepare_tinytest_dev_includes
  
  invalids <- list(
    non_character = 123,
    multi_length  = c("a", "b"),
    na_value      = NA_character_,
    empty_string  = ""
  )
  
  for (val in invalids) {
    res <- prep(val, tiny_dir = "x")
    expect_type(res, "list")
    expect_identical(res$include_dirs, character(0))
    expect_type(res$teardown, "closure")
    expect_null(res$teardown())
  }
})


test_that("prepare_tinytest_dev_includes returns a no-op when inst/include is absent", {
  
  prep <- prepare_tinytest_dev_includes
  
  mockery::stub(prep, "dir.exists", function(path) FALSE)
  
  res <- prep("/some/pkg", tiny_dir = "/some/pkg/inst/tinytest")
  
  expect_identical(res$include_dirs, character(0))
  expect_type(res$teardown, "closure")
  expect_null(res$teardown())
})


test_that("prepare_tinytest_dev_includes adds inst/include and sets flags when env was unset", {
  
  prep <- prepare_tinytest_dev_includes
  
  pkg_source_path <- file.path(tempdir(), "devinc_unset")
  tiny_dir        <- file.path(pkg_source_path, "inst", "tinytest")
  inst_include    <- file.path(pkg_source_path, "inst", "include")
  cpp_dir         <- file.path(tiny_dir, "cpp")
  
  # Local in-memory environment store (no real env touched)
  fake_env <- new.env(parent = emptyenv())
  
  # inst/include exists, but the tinytest cpp/ dir does not
  mockery::stub(prep, "dir.exists",
                function(path) path %in% inst_include)
  mockery::stub(prep, "normalizePath", function(path, ...) path)
  mockery::stub(prep, "Sys.getenv",
                function(x, unset = NA_character_, names = TRUE) {
                  stats::setNames(c(NA_character_, NA_character_),
                                  c("PKG_CPPFLAGS", "CLINK_CPPFLAGS"))
                })
  mockery::stub(prep, "Sys.setenv", function(...) {
    a <- list(...)
    for (nm in names(a)) assign(nm, a[[nm]], envir = fake_env)
    TRUE
  })
  mockery::stub(prep, "Sys.unsetenv", function(x) {
    for (nm in x) if (exists(nm, envir = fake_env, inherits = FALSE)) {
      rm(list = nm, envir = fake_env)
    }
    TRUE
  })
  
  res <- prep(pkg_source_path, tiny_dir = tiny_dir)
  
  expect_identical(res$include_dirs, inst_include)
  expect_false(cpp_dir %in% res$include_dirs)
  
  expected_i <- paste(sprintf("-I\"%s\"", inst_include), collapse = " ")
  expect_identical(get("PKG_CPPFLAGS", envir = fake_env), expected_i)
  expect_identical(get("CLINK_CPPFLAGS", envir = fake_env), expected_i)
  
  # teardown unsets both (they were previously unset / NA)
  res$teardown()
  expect_false(exists("PKG_CPPFLAGS", envir = fake_env, inherits = FALSE))
  expect_false(exists("CLINK_CPPFLAGS", envir = fake_env, inherits = FALSE))
})


test_that("prepare_tinytest_dev_includes appends cpp/ dir and prepends to pre-set flags", {
  
  prep <- prepare_tinytest_dev_includes
  
  pkg_source_path <- file.path(tempdir(), "devinc_preset")
  tiny_dir        <- file.path(pkg_source_path, "inst", "tinytest")
  inst_include    <- file.path(pkg_source_path, "inst", "include")
  cpp_dir         <- file.path(tiny_dir, "cpp")
  
  fake_env <- new.env(parent = emptyenv())
  
  # both inst/include and tinytest cpp/ exist
  mockery::stub(prep, "dir.exists",
                function(path) path %in% c(inst_include, cpp_dir))
  mockery::stub(prep, "normalizePath", function(path, ...) path)
  # PKG_CPPFLAGS already set; CLINK_CPPFLAGS unset
  mockery::stub(prep, "Sys.getenv",
                function(x, unset = NA_character_, names = TRUE) {
                  stats::setNames(c("-DEXISTING", NA_character_),
                                  c("PKG_CPPFLAGS", "CLINK_CPPFLAGS"))
                })
  mockery::stub(prep, "Sys.setenv", function(...) {
    a <- list(...)
    for (nm in names(a)) assign(nm, a[[nm]], envir = fake_env)
    TRUE
  })
  mockery::stub(prep, "Sys.unsetenv", function(x) {
    for (nm in x) if (exists(nm, envir = fake_env, inherits = FALSE)) {
      rm(list = nm, envir = fake_env)
    }
    TRUE
  })
  
  res <- prep(pkg_source_path, tiny_dir = tiny_dir)
  
  expect_identical(res$include_dirs, c(inst_include, cpp_dir))
  
  expected_i <- paste(sprintf("-I\"%s\"", c(inst_include, cpp_dir)), collapse = " ")
  # pre-set var is prepended (i_flags first, old value second)
  expect_identical(get("PKG_CPPFLAGS", envir = fake_env),
                   paste(expected_i, "-DEXISTING"))
  # previously-unset var becomes just the flags
  expect_identical(get("CLINK_CPPFLAGS", envir = fake_env), expected_i)
  
  # teardown restores the prior value and unsets the previously-NA var
  res$teardown()
  expect_identical(get("PKG_CPPFLAGS", envir = fake_env), "-DEXISTING")
  expect_false(exists("CLINK_CPPFLAGS", envir = fake_env, inherits = FALSE))
})


test_that("prepare_tinytest_dev_includes ignores cpp/ when tiny_dir is invalid", {
  
  prep <- prepare_tinytest_dev_includes
  
  pkg_source_path <- file.path(tempdir(), "devinc_badtiny")
  inst_include    <- file.path(pkg_source_path, "inst", "include")
  
  fake_env <- new.env(parent = emptyenv())
  
  # dir.exists must only ever be consulted for inst/include here; if the code
  # tried to probe a cpp dir built from an invalid tiny_dir, this would flag it.
  mockery::stub(prep, "dir.exists", function(path) {
    expect_identical(path, inst_include)
    TRUE
  })
  mockery::stub(prep, "normalizePath", function(path, ...) path)
  mockery::stub(prep, "Sys.getenv",
                function(x, unset = NA_character_, names = TRUE) {
                  stats::setNames(c(NA_character_, NA_character_),
                                  c("PKG_CPPFLAGS", "CLINK_CPPFLAGS"))
                })
  mockery::stub(prep, "Sys.setenv", function(...) {
    a <- list(...)
    for (nm in names(a)) assign(nm, a[[nm]], envir = fake_env)
    TRUE
  })
  mockery::stub(prep, "Sys.unsetenv", function(x) TRUE)
  
  res <- prep(pkg_source_path, tiny_dir = NA_character_)
  
  expect_identical(res$include_dirs, inst_include)
})


test_that("prepare_tinytest_dev_includes returns a no-op and warns on internal error", {
  
  prep <- prepare_tinytest_dev_includes
  
  # Force the tryCatch error branch from inside the protected block
  mockery::stub(prep, "dir.exists", function(path) stop("boom"))
  
  expect_message(
    res <- prep("/some/pkg", tiny_dir = "/some/pkg/inst/tinytest"),
    "skipping dev include setup"
  )
  
  expect_identical(res$include_dirs, character(0))
  expect_type(res$teardown, "closure")
  expect_null(res$teardown())
})


# ---------------------------------------------------------------------------
# prepare_tinytest_run_env(): full-coverage suite
#
# Covers every branch without touching the real process environment or
# filesystem. dir.exists()/list.files()/readLines() and the Sys.* env
# accessors are stubbed (mockery::stub) against a local in-memory store, so
# nothing leaks into .GlobalEnv or the user's environment and there is nothing
# to clean up on disk.
# ---------------------------------------------------------------------------

test_that("prepare_tinytest_run_env returns a no-op for invalid tiny_dir", {
  
  prep <- prepare_tinytest_run_env
  
  invalids <- list(
    non_character = 123,
    multi_length  = c("a", "b"),
    na_value      = NA_character_,
    empty_string  = ""
  )
  
  for (val in invalids) {
    res <- prep(val)
    expect_type(res, "list")
    expect_identical(res$set_vars, character(0))
    expect_type(res$teardown, "closure")
    expect_null(res$teardown())
  }
})


test_that("prepare_tinytest_run_env returns a no-op when the directory does not exist", {
  
  prep <- prepare_tinytest_run_env
  
  mockery::stub(prep, "dir.exists", function(path) FALSE)
  
  res <- prep("/no/such/tinytest")
  
  expect_identical(res$set_vars, character(0))
  expect_null(res$teardown())
})


test_that("prepare_tinytest_run_env returns a no-op when only runner files are present", {
  
  prep <- prepare_tinytest_run_env
  
  tiny_dir <- file.path(tempdir(), "tte_runners", "inst", "tinytest")
  
  mockery::stub(prep, "dir.exists", function(path) TRUE)
  # Only runner scripts -> all filtered out -> no scannable files
  mockery::stub(prep, "list.files", function(path, ...) {
    file.path(tiny_dir, c("tinytest.R", "runTinyTests.R", "run_tinytest.R"))
  })
  
  res <- prep(tiny_dir)
  
  expect_identical(res$set_vars, character(0))
  expect_null(res$teardown())
})


test_that("prepare_tinytest_run_env returns a no-op when no gates are found", {
  
  prep <- prepare_tinytest_run_env
  
  tiny_dir <- file.path(tempdir(), "tte_nogate", "inst", "tinytest")
  f_a <- file.path(tiny_dir, "test-a.R")
  
  mockery::stub(prep, "dir.exists", function(path) TRUE)
  mockery::stub(prep, "list.files", function(path, ...) f_a)
  mockery::stub(prep, "readLines",
                function(con, n, warn = FALSE, ...) c("x <- 1", "y <- 2"))
  
  res <- prep(tiny_dir)
  
  expect_identical(res$set_vars, character(0))
  expect_null(res$teardown())
})


test_that("prepare_tinytest_run_env detects != and == gates, sets vars, and teardown restores", {
  
  prep <- prepare_tinytest_run_env
  
  tiny_dir <- file.path(tempdir(), "tte_gates", "inst", "tinytest")
  f_err   <- file.path(tiny_dir, "test-err.R")    # readLines throws
  f_empty <- file.path(tiny_dir, "test-empty.R")  # readLines -> character(0)
  f_ne    <- file.path(tiny_dir, "test-ne.R")     # != gate
  f_eq    <- file.path(tiny_dir, "test-eq.R")     # == gate + two gates on one line
  
  fake_env <- new.env(parent = emptyenv())
  
  mockery::stub(prep, "dir.exists", function(path) TRUE)
  mockery::stub(prep, "list.files",
                function(path, ...) c(f_err, f_empty, f_ne, f_eq))
  
  mockery::stub(prep, "readLines", function(con, n, warn = FALSE, ...) {
    if (identical(con, f_err))   stop("io failure")
    if (identical(con, f_empty)) return(character(0))
    if (identical(con, f_ne)) {
      return('if (Sys.getenv("RunAllRcppTests") != "yes") exit_file("skip")')
    }
    if (identical(con, f_eq)) {
      return(c(
        'ok <- Sys.getenv("RunVerboseRcppTests") == "yes"',
        'Sys.getenv("A") == "1" && Sys.getenv("B") != "2"'
      ))
    }
    character(0)
  })
  
  # A was previously set; the rest were unset (NA) -> exercises both teardown paths
  mockery::stub(prep, "Sys.getenv",
                function(x, unset = NA_character_, names = TRUE) {
                  stats::setNames(
                    vapply(x, function(v) if (identical(v, "A")) "OLD" else NA_character_,
                           character(1)),
                    x
                  )
                })
  mockery::stub(prep, "Sys.setenv", function(...) {
    a <- list(...)
    for (nm in names(a)) assign(nm, a[[nm]], envir = fake_env)
    TRUE
  })
  mockery::stub(prep, "Sys.unsetenv", function(x) {
    for (nm in x) if (exists(nm, envir = fake_env, inherits = FALSE)) {
      rm(list = nm, envir = fake_env)
    }
    TRUE
  })
  
  res <- prep(tiny_dir)
  
  # all four gates discovered (across != and == idioms, incl. two on one line)
  expect_setequal(names(res$set_vars),
                  c("RunAllRcppTests", "RunVerboseRcppTests", "A", "B"))
  expect_identical(
    unname(res$set_vars[c("RunAllRcppTests", "RunVerboseRcppTests", "A", "B")]),
    c("yes", "yes", "1", "2")
  )
  
  # vars were set to required values in the (fake) environment
  expect_identical(get("RunAllRcppTests", envir = fake_env), "yes")
  expect_identical(get("RunVerboseRcppTests", envir = fake_env), "yes")
  expect_identical(get("A", envir = fake_env), "1")
  expect_identical(get("B", envir = fake_env), "2")
  
  # teardown: A restored to its prior value; the previously-unset vars removed
  res$teardown()
  expect_identical(get("A", envir = fake_env), "OLD")
  expect_false(exists("RunAllRcppTests", envir = fake_env, inherits = FALSE))
  expect_false(exists("RunVerboseRcppTests", envir = fake_env, inherits = FALSE))
  expect_false(exists("B", envir = fake_env, inherits = FALSE))
})


test_that("prepare_tinytest_run_env returns a no-op and warns on internal error", {
  
  prep <- prepare_tinytest_run_env
  
  mockery::stub(prep, "dir.exists", function(path) TRUE)
  mockery::stub(prep, "list.files", function(path, ...) stop("boom"))
  
  expect_message(
    res <- prep(file.path(tempdir(), "tte_error")),
    "skipping env gate setup"
  )
  
  expect_identical(res$set_vars, character(0))
  expect_null(res$teardown())
})


# ---------------------------------------------------------------------------
# run_covr_skip_nstf(): tinytest_runner_paths filtering (tinytest branch)
#
# Covers the block that drops explicitly-declared tinytest runner paths from
# test_files_clean. The runner here uses a basename that is NOT in the default
# runner list, so its removal is attributable solely to the
# tinytest_runner_paths block. All inputs are mocked, no global env is used,
# and the temp dir is removed on exit.
# ---------------------------------------------------------------------------

test_that("tinytest branch removes declared tinytest_runner_paths from test_files_clean", {
  
  run_fn <- run_covr_skip_nstf
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_runner_paths")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  tt_dir        <- file.path(pkg_source_path, "inst", "tinytest")
  test_a_path   <- file.path(tt_dir, "test-a.R")
  test_b_path   <- file.path(tt_dir, "test-b.R")
  # custom runner: basename not in c("tinytest.R","runTinyTests.R","run_tinytest.R")
  runner_path   <- file.path(tt_dir, "custom_runner.R")
  
  mapping <- data.frame(
    source_file = c("x.R", "y.R", "z.R"),
    test_file   = c("test-a.R", "test-b.R", "custom_runner.R"),
    stringsAsFactors = FALSE
  )
  
  # No skips, so the skip_tests setdiff cannot account for the removal
  problems_in <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(run_fn, "get_nstf_test_path",
                function(...) list(tinytest = file.path(pkg_source_path, "tests")))
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  # the surviving (non-runner) files exist; runner is removed before this filter
  mockery::stub(run_fn, "file.exists",
                function(paths) paths %in% c(test_a_path, test_b_path))
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf", function(...) problems_in)
  mockery::stub(run_fn, "get_function_no_tests",
                function(...) data.frame(source_file = character(0), stringsAsFactors = FALSE))
  
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace", function(name) fake_ns)
  mockery::stub(run_fn, "ls", function(...) character(0))
  mockery::stub(run_fn, "get", function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets", function(...) character(0))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  # No file_status -> the per-file refinement block is skipped, so the value
  # built by the runner-path filtering flows straight to create_nstf_covr_list.
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(coverage = list(x = 1), tinytests = list())
  )
  
  captured <- new.env(parent = emptyenv())
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(coverage, pkg_name, no_tests_df, test_files_clean, skip_tests, problems) {
      captured$test_files_clean <- test_files_clean
      list(
        res_cov   = list(name = pkg_name,
                         coverage = list(filecoverage = matrix(1), totalcoverage = 70)),
        total_cov = 70
      )
    }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  test_pkg_data <- tpd(has_tinytest = TRUE, tinytest_runner_paths = runner_path)
  
  result <- suppressMessages(
    run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  )
  
  # the declared runner path is removed; the two real test files remain
  expect_setequal(captured$test_files_clean, c(test_a_path, test_b_path))
  expect_false(runner_path %in% captured$test_files_clean)
  expect_equal(result$total_cov, 70)
})


# ---------------------------------------------------------------------------
# run_covr_skip_nstf(): empty-coverage fallback when res_cov is NULL
#
# Covers the block that builds a placeholder covr_list when
# create_nstf_covr_list() yields a NULL res_cov. All inputs are mocked, no
# global env is used, and the temp dir is removed on exit.
# ---------------------------------------------------------------------------

test_that("run_covr_skip_nstf returns the empty-coverage fallback when res_cov is NULL", {
  
  run_fn <- run_covr_skip_nstf
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_null_rescov")
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE, showWarnings = FALSE)
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  pkg_name <- "mockpkg"
  tt_dir      <- file.path(pkg_source_path, "inst", "tinytest")
  test_a_path <- file.path(tt_dir, "test-a.R")
  
  mapping <- data.frame(
    source_file = "x.R",
    test_file   = "test-a.R",
    stringsAsFactors = FALSE
  )
  
  problems_in <- data.frame(
    test_file  = NA_character_,
    issue_type = "No tests skipped",
    stringsAsFactors = FALSE
  )
  
  mockery::stub(run_fn, "get_pkg_name", function(...) pkg_name)
  mockery::stub(run_fn, "get_nstf_test_path",
                function(...) list(tinytest = file.path(pkg_source_path, "tests")))
  mockery::stub(run_fn, "normalizePath", function(path, ...) path)
  mockery::stub(run_fn, "file.exists", function(paths) paths %in% test_a_path)
  mockery::stub(run_fn, "get_source_test_mapping_nstf", function(...) mapping)
  mockery::stub(run_fn, "check_covr_skip_nstf", function(...) problems_in)
  mockery::stub(run_fn, "get_function_no_tests",
                function(...) data.frame(source_file = character(0), stringsAsFactors = FALSE))
  
  fake_ns <- new.env(parent = emptyenv())
  mockery::stub(run_fn, "pkgload::load_all", function(...) NULL)
  mockery::stub(run_fn, "asNamespace", function(name) fake_ns)
  mockery::stub(run_fn, "ls", function(...) character(0))
  mockery::stub(run_fn, "get", function(obj, envir) obj)
  mockery::stub(run_fn, "load_package_datasets", function(...) character(0))
  mockery::stub(run_fn, "load_dependencies_into_env", function(...) TRUE)
  mockery::stub(run_fn, "load_base_r_packages_into_env", function(...) TRUE)
  
  mockery::stub(
    run_fn, "create_tinytest_coverage",
    function(...) list(coverage = list(x = 1), tinytests = list())
  )
  
  # Force the NULL-res_cov branch: a non-NULL total_cov shows the fallback
  # fully replaces the returned list.
  mockery::stub(
    run_fn, "create_nstf_covr_list",
    function(...) list(res_cov = NULL, total_cov = 999)
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  cov_env <- new.env(parent = emptyenv())
  test_pkg_data <- tpd(has_tinytest = TRUE)
  
  result <- suppressMessages(
    run_fn(pkg_source_path, test_pkg_data, cov_env = cov_env)
  )
  
  # Placeholder coverage object fully replaces the NULL-res_cov result
  expect_equal(result$total_cov, 0)
  expect_identical(result$res_cov$name, pkg_name)
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  expect_identical(result$res_cov$errors, "No testthat or testit configuration")
  expect_true(is.na(result$res_cov$notes))
  
  fc <- result$res_cov$coverage$filecoverage
  expect_true(is.matrix(fc))
  expect_equal(dim(fc), c(1L, 1L))
  expect_equal(unname(fc[1, 1]), 0)
  expect_identical(rownames(fc), "No functions tested")
})


# ---------------------------------------------------------------------------
# check_covr_skip_nstf(): tinytest probe branch (is_tinytest)
#
# Covers the per-file probe that runs tinytest::run_test_file() and flags files
# whose expectations fail. base_dir ends in inst/tinytest so is_tinytest is
# TRUE; tinytest::run_test_file / any_fail and the path helpers are mocked, so
# no real tests run, no global env is used, and nothing is written to disk.
# ---------------------------------------------------------------------------

test_that("check_covr_skip_nstf flags tinytest files with failed expectations", {
  
  fn <- check_covr_skip_nstf
  
  pkg_source_path <- file.path(tempdir(), "ccsn_tt_fail")
  base_dir        <- file.path(pkg_source_path, "inst", "tinytest")
  
  mapping <- data.frame(
    source_file = "x.R",
    test_file   = "test-a.R",
    stringsAsFactors = FALSE
  )
  
  mockery::stub(fn, "get_pkg_name", function(...) "mockpkg")
  mockery::stub(fn, "normalizePath", function(path, ...) path)
  mockery::stub(fn, "file.exists", function(...) TRUE)
  mockery::stub(fn, "tinytest::run_test_file", function(...) list(fake = TRUE))
  mockery::stub(fn, "tinytest::any_fail", function(tt) TRUE)
  
  result <- fn(pkg_source_path, mapping, base_dir = base_dir)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_identical(result$test_file, "test-a.R")
  expect_identical(result$issue_type, "failed expectation")
})


test_that("check_covr_skip_nstf reports no skips when tinytest expectations pass", {
  
  fn <- check_covr_skip_nstf
  
  pkg_source_path <- file.path(tempdir(), "ccsn_tt_pass")
  base_dir        <- file.path(pkg_source_path, "inst", "tinytest")
  
  mapping <- data.frame(
    source_file = "x.R",
    test_file   = "test-a.R",
    stringsAsFactors = FALSE
  )
  
  mockery::stub(fn, "get_pkg_name", function(...) "mockpkg")
  mockery::stub(fn, "normalizePath", function(path, ...) path)
  mockery::stub(fn, "file.exists", function(...) TRUE)
  mockery::stub(fn, "tinytest::run_test_file", function(...) list(fake = TRUE))
  mockery::stub(fn, "tinytest::any_fail", function(tt) FALSE)
  
  result <- fn(pkg_source_path, mapping, base_dir = base_dir)
  
  # no problems -> placeholder "No tests skipped" row
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$test_file))
  expect_identical(result$issue_type, "No tests skipped")
})


# ---------------------------------------------------------------------------
# create_tinytest_coverage(): informational messages
#
# Covers the three message blocks that fire when (a) gated env vars are set,
# (b) dev-tree include paths are added, and (c) objects created in .GlobalEnv
# are cleaned up. globalenv() is stubbed to a *fake* environment that the
# mocked run_test_file pollutes, so the real global environment is never
# touched. NOT_CRAN is snapshotted and restored on exit.
# ---------------------------------------------------------------------------

test_that("create_tinytest_coverage emits gate/include/global-cleanup messages", {
  
  pkg_source_path <- "/pkg"
  tiny_dir        <- "/pkg/inst/tinytest"
  pkg_name        <- "mypkg"
  
  # covr internals
  flags <- new.env(parent = emptyenv())
  flags$trace <- FALSE; flags$reset <- FALSE; flags$clear <- FALSE
  fake_counters <- list(counter_data = 1)
  fake_coverage <- structure(list(kind = "coverage"), class = "coverage")
  fake_covr_ns  <- make_fake_covr_ns(flags,
                                     counters_value = fake_counters,
                                     coverage_value = fake_coverage)
  fake_pkg_ns <- new.env(parent = emptyenv())
  
  # Fake "global" environment so the cleanup branch can run without touching
  # the real .GlobalEnv.
  fake_global <- new.env(parent = emptyenv())
  
  mockery::stub(create_tinytest_coverage, "asNamespace", function(name) {
    if (identical(name, "covr"))   return(fake_covr_ns)
    if (identical(name, pkg_name)) return(fake_pkg_ns)
    stop("unexpected asNamespace(", name, ")")
  })
  mockery::stub(create_tinytest_coverage, "globalenv", function() fake_global)
  
  # Non-empty gate vars -> "Enabling gated tinytest variable(s)" message
  mockery::stub(create_tinytest_coverage, "prepare_tinytest_run_env",
                function(...) list(
                  set_vars = c(RunAllRcppTests = "yes"),
                  teardown = function() invisible(NULL)
                ))
  # Non-empty include dirs -> "Adding dev-tree include path(s)" message
  mockery::stub(create_tinytest_coverage, "prepare_tinytest_dev_includes",
                function(...) list(
                  include_dirs = "/pkg/inst/include",
                  teardown     = function() invisible(NULL)
                ))
  
  test_a <- file.path(tiny_dir, "test_a.R")
  mockery::stub(create_tinytest_coverage, "list.files",
                function(path, pattern, full.names) test_a)
  
  # The run "pollutes" the fake global env, triggering the cleanup branch.
  mockery::stub(create_tinytest_coverage, "tinytest::run_test_file",
                function(file, at_home, color) {
                  assign("polluted_fn", function() NULL, envir = fake_global)
                  list(results = "ok")
                })
  
  # Restore NOT_CRAN regardless of the function's own teardown
  old_notcran <- Sys.getenv("NOT_CRAN", unset = NA)
  withr::defer(
    if (is.na(old_notcran)) Sys.unsetenv("NOT_CRAN") else Sys.setenv(NOT_CRAN = old_notcran)
  )
  
  msgs <- testthat::capture_messages(
    res <- create_tinytest_coverage(pkg_source_path, pkg_name, tiny_dir = tiny_dir)
  )
  
  # all three informational messages were emitted
  expect_match(msgs, "Enabling gated tinytest variable\\(s\\) for mypkg: RunAllRcppTests=yes",
               all = FALSE)
  expect_match(msgs, "Adding dev-tree include path\\(s\\) for mypkg: /pkg/inst/include",
               all = FALSE)
  expect_match(msgs, "Removed 1 object\\(s\\) created in \\.GlobalEnv during mypkg tinytest coverage",
               all = FALSE)
  
  # the polluting object was removed from the (fake) global env
  expect_false(exists("polluted_fn", envir = fake_global, inherits = FALSE))
  
  expect_identical(res$coverage, fake_coverage)
})

