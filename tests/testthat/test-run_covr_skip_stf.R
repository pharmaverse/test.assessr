
test_that("run_covr_skip_stf filters test results and generates skip map", {
  
  # Create a temporary directory to satisfy the assertion
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_source_path, "tests", "testthat"), recursive = TRUE, showWarnings = FALSE)
  
  # Mock long summary
  mock_test_results_long <- data.frame(
    file    = c("test-a.R", "test-b.R", "test-c.R"),
    context = c("script1", "script2", "script3"),
    test    = c("test 1", "test 2", "test 3"),
    status  = c("FAILED", "PASS", "SKIPPED"),
    n       = c(1, 1, 1),
    time    = c(0.01, 0.02, 0.03),
    stringsAsFactors = FALSE
  )
  
  # Mock test map
  mock_test_map <- data.frame(
    test = c("test 1", "test 2", "test 3"),
    expectation = c("expect_equal", "expect_true", "expect_error"),
    line1 = c(2, 4, 6),
    line2 = c(2, 4, 6),
    file  = c("test-a.R", "test-b.R", "test-c.R"),
    stringsAsFactors = FALSE
  )
  
  mock_test_skip <- data.frame(
    file  = c("test-a.R", "test-c.R"),
    line1 = c(2, 6),
    line2 = c(2, 6),
    stringsAsFactors = FALSE
  )
  
  # Minimal pkg data
  mock_test_pkg_data <- list(
    has_testthat = TRUE,
    has_testit = FALSE,
    has_tests_base = FALSE
  )
  
  # ---- STUBS (Complete) ----
  
  mockery::stub(run_covr_skip_stf, "checkmate::assert_string", TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_directory_exists", TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_environment", TRUE)
  mockery::stub(run_covr_skip_stf, "get_pkg_name", function(...) "mockpkg")
  
  # test path resolver
  mockery::stub(run_covr_skip_stf, "get_stf_test_path", function(x, testdir) testdir)
  
  # avoid changing working directory
  mockery::stub(run_covr_skip_stf, "setwd", function(...) NULL)
  
  # dummy covr env setup
  mockery::stub(run_covr_skip_stf, "setup_covr_env", function(...) {
    list(datasets_loaded=TRUE, root_dir=".", initial_state=list(), helpers_loaded=TRUE)
  })
  
  # unload namespace stub
  mockery::stub(run_covr_skip_stf, "unloadNamespace", function(...) NULL)
  
  # fail reporter stub
  mockery::stub(run_covr_skip_stf, "create_fail_reporter", function(...) NULL)
  
  # create_test_reporter must return valid minimal objects
  mockery::stub(run_covr_skip_stf, "create_test_reporter", function(test_path, pkg) {
    list(
      list(file="test-a.R", context="script1"),
      list(file="test-b.R", context="script2"),
      list(file="test-c.R", context="script3")
    )
  })
  
  # VERY IMPORTANT: fix_test_context must be stubbed
  mockery::stub(run_covr_skip_stf, "fix_test_context", function(x) x)
  
  mockery::stub(run_covr_skip_stf, "map_tests_stf", function(...) mock_test_map)
  mockery::stub(run_covr_skip_stf, "get_tests_long_sum_stf", function(...) mock_test_results_long)
  mockery::stub(run_covr_skip_stf, "get_tests_skip_stf", function(...) mock_test_skip)
  
  # coverage skip stub
  mockery::stub(run_covr_skip_stf, "create_coverage_skip_stf",
                function(pkg_source_path, pkg, test_path,
                         test_skip, test_map, cov_env) {
                  structure(list(package="mockpkg", coverage=85.5),
                            class="coverage")
                })
  
  # summary stub
  mockery::stub(run_covr_skip_stf, "create_results_summary",
                function(cvr) {
                  list(
                    total_cov = 85.5,
                    res_cov = list(
                      name="mockpkg",
                      coverage=85.5,
                      errors=NULL,
                      notes="Coverage summary generated"
                    )
                  )
                })
  
  # no-skip path stub (not used in this test)
  mockery::stub(run_covr_skip_stf, "create_covr_list_no_skip",
                function(...) list())
  
  # ---- EXECUTION ----
  cov_env <- new.env(parent = emptyenv())
  
  result <- run_covr_skip_stf(
    pkg_source_path = pkg_source_path,
    test_pkg_data = mock_test_pkg_data,
    cov_env = cov_env
  )
  
  # ---- ASSERTIONS ----
  
  expect_type(result, "list")
  expect_equal(result$total_cov, 85.5)
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage, 85.5)
  expect_equal(result$res_cov$notes, "Coverage summary generated")
  
  expect_equal(result$test_skip, mock_test_skip)
  
  # new context column should match the original test column
  expect_equal(result$long_summary$context, mock_test_results_long$test)
  
  # new code_script column should match the original context column
  expect_equal(result$long_summary$code_script, mock_test_results_long$context)
  
})




test_that("run_covr_skip_stf processes no test dir and generates message", {
  
  # Simulate a package with no recognizable test directories
  mock_test_pkg_data <- list(
    has_testthat   = FALSE,
    has_testit     = FALSE,
    has_tests_base = FALSE
  )
  
  # Stubs required to reach the "no test dir" branch
  mockery::stub(run_covr_skip_stf, "checkmate::assert_string", TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_directory_exists", TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_environment", TRUE)
  
  mockery::stub(run_covr_skip_stf, "get_pkg_name", function(...) "mockpkg")
  mockery::stub(run_covr_skip_stf, "setwd", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "message", function(...) NULL)
  
  # Critical: this must return NULL or the "no test directory" branch never triggers
  mockery::stub(run_covr_skip_stf, "get_stf_test_path", function(...) NULL)
  
  cov_env <- new.env(parent = emptyenv())
  
  # Execute function
  result <- run_covr_skip_stf(
    pkg_source_path = tempdir(),
    test_pkg_data   = mock_test_pkg_data,
    cov_env         = cov_env
  )
  
  # --- Assertions ---
  expect_type(result, "list")
  expect_named(result, c("total_cov", "res_cov"))
  
  expect_equal(result$total_cov, 0)
  
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  
  expect_equal(result$res_cov$errors, "No test directory found")
  expect_equal(result$res_cov$notes,  NA)
})



test_that("run_covr_skip_stf handles missing test directory correctly", {
  # Create a temporary directory to simulate a package source path
  temp_pkg_path <- tempfile("pkg")
  dir.create(temp_pkg_path)
  
  # Mock test_pkg_data to simulate no test directories
  mock_test_pkg_data <- list(
    has_testthat = FALSE,
    has_testit = FALSE,
    has_tests_base = FALSE
  )
  
  # Stub required dependencies
  mockery::stub(run_covr_skip_stf, "checkmate::assert_string", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_directory_exists", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "get_pkg_name", function(...) "dummyPkg")
  mockery::stub(run_covr_skip_stf, "setwd", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "message", function(...) NULL)
  
  cov_env <- new.env(parent = emptyenv())
  
  # Execute function
  result <- run_covr_skip_stf(
    pkg_source_path = temp_pkg_path,
    test_pkg_data   = mock_test_pkg_data,
    cov_env         = cov_env
  )
  
  # Check that the result contains NAs as expected
  expect_type(result, "list")
  testthat::expect_named(result, c("total_cov", "res_cov"))
  testthat::expect_equal(result$res_cov$coverage$totalcoverage, 0)
  testthat::expect_equal(result$res_cov$errors, "No test directory found")
})


test_that("fallback problem_tests is created when no failed/skipped/error tests", {
  
  # Mocked test_map structure
  fake_test_map <- data.frame(
    test = rep(c("two working tests", "three working tests"), each = 2),
    expectation = rep("expect_equal", 4),
    line1 = c(2, 4, 8, 10),
    line2 = c(2, 4, 8, 10),
    file = rep("test-myscript1.R", 4),
    stringsAsFactors = FALSE
  )
  
  # Mocked test_results_long structure (all PASS)
  fake_test_results_long <- data.frame(
    file    = rep("test-myscript1.R#L", 2),
    context = rep("myscript1", 2),
    test    = c("two working tests", "three working tests"),
    status  = rep("PASS", 2),
    n       = c(2, 3),
    time    = c(0.01, 0.01),
    stringsAsFactors = FALSE
  )
  
  # Required stubs for input validation
  mockery::stub(run_covr_skip_stf, "checkmate::assert_string", TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_directory_exists", TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_environment", TRUE)
  mockery::stub(run_covr_skip_stf, "message", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "setwd", function(...) NULL)
  
  # Return a valid test path
  mockery::stub(run_covr_skip_stf, "get_stf_test_path", function(...) "tests")
  
  # Mock testthat reporters
  mockery::stub(run_covr_skip_stf, "create_fail_reporter", function(...) list())
  mockery::stub(run_covr_skip_stf, "create_test_reporter",
                function(test_path, pkg) {
                  list(
                    list(file="test-a.R", context="script1"),
                    list(file="test-b.R", context="script2")
                  )
                })
  
  # IMPORTANT: bypass fix_test_context errors
  mockery::stub(run_covr_skip_stf, "fix_test_context", function(x) x)
  
  # Provide map + long summary
  mockery::stub(run_covr_skip_stf, "map_tests_stf", function(...) fake_test_map)
  mockery::stub(run_covr_skip_stf, "get_tests_long_sum_stf", function(...) fake_test_results_long)
  
  # Corrected stub — now matches the actual call signature
  mockery::stub(run_covr_skip_stf, "create_covr_list_no_skip",
                function(test_map,
                         test_results_long,
                         pkg_source_path,
                         pkg,
                         cov_env) {
                  
                  list(
                    total_cov = 0.5,
                    res_cov = list(
                      name     = pkg,
                      coverage = list(filecoverage=fake_test_map, totalcoverage=50),
                      errors   = NA,
                      notes    = NA,
                      passed   = 5,
                      failed   = 0
                    ),
                    tests_skipped = list()
                  )
                })
  
  # Other stubs
  mockery::stub(run_covr_skip_stf, "get_pkg_name", function(...) "mockpkg")
  mockery::stub(run_covr_skip_stf, "unloadNamespace", function(...) NULL)
  
  cov_env <- new.env(parent = emptyenv())
  
  # Run the function
  result <- run_covr_skip_stf(
    pkg_source_path = tempdir(),
    test_pkg_data   = NULL,
    cov_env         = cov_env
  )
  
  # --- Assertions ---
  expect_true(is.list(result))
  expect_named(result, c("total_cov", "res_cov", "tests_skipped"))
  expect_equal(result$res_cov$passed, 5)
  expect_equal(result$res_cov$failed, 0)
})






test_that("create_coverage_skip_stf runs coverage with datasets", {
  # --- Arrange ---
  pkg             <- "mockpkg"
  pkg_source_path <- file.path(tempdir(), "mockpkg-root")
  test_path       <- file.path(pkg_source_path, "tests", "testthat")
  dir.create(test_path, recursive = TRUE, showWarnings = FALSE)
  
  # Create the *actual* source test file that the function will copy
  src_test_file <- file.path(test_path, "test-file.R")
  writeLines(c(
    "test_that('example', {",
    "  expect_true(TRUE)",
    "})"
  ), src_test_file)
  
  test_skip <- data.frame(file = "test-file.R", line1 = 1, line2 = 2, stringsAsFactors = FALSE)
  test_map  <- data.frame(file = "test-file.R", stringsAsFactors = FALSE)
  
  # The function expects a working directory in cov_env
  cov_env <- new.env(parent = emptyenv())
  cov_env$work_dir <- file.path(tempdir(), "cov-work")
  dir.create(cov_env$work_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- Stubs (minimal, let real IO happen where safe) ---
  # Let assertions pass (directories exist anyway)
  mockery::stub(create_coverage_skip_stf, "checkmate::assert_string", TRUE)
  mockery::stub(create_coverage_skip_stf, "checkmate::assert_directory_exists", TRUE)
  mockery::stub(create_coverage_skip_stf, "checkmate::assert_environment", TRUE)
  
  # Keep project root stable
  mockery::stub(create_coverage_skip_stf, "find_project_root", function(...) pkg_source_path)
  
  # Avoid changing the working directory for the test
  mockery::stub(create_coverage_skip_stf, "withr::with_dir", function(new_dir, code) force(code))
  
  # No-op cleanup if used internally
  mockery::stub(create_coverage_skip_stf, "unlink", function(x, recursive = FALSE, force = FALSE) 0)
  
  # Coverage stub that *survives* the Filter(length(c(srcref,value)) == 9)
  # Here srcref has 5 parts and value has 4 parts -> 9 total.
  mock_cov_list <- list(list(
    srcref = as.list(1:5),
    value  = as.list(1:4)
  ))
  mockery::stub(create_coverage_skip_stf, "covr::environment_coverage", function(env, test_files) {
    mock_cov_list
  })
  
  # --- Act ---
  result <- create_coverage_skip_stf(
    pkg_source_path = pkg_source_path,
    pkg             = pkg,
    test_path       = test_path,
    test_skip       = test_skip,
    test_map        = test_map,
    cov_env         = cov_env
  )
  
  # --- Assert ---
  # After the Filter step, the object is a plain list (class "coverage" is not preserved).
  expect_true(is.list(result))
  expect_length(result, 1L)
  expect_true(all(c("srcref", "value") %in% names(result[[1]])))
})





test_that("create_results_summary returns correct structure and values", {
  # Mock coverage object
  mock_cvr <- list(
    "file1.R:1" = list(value = c(1, 0, 1)),
    "file2.R:2" = list(value = c(0, 0, 1)),
    "file1.R:3" = list(value = c(1, 1))
  )
  
  # Add package attribute
  attr(mock_cvr, "package") <- list(package = "mockpkg")
  
  # Stub covr::percent_coverage to return a fixed value
  mockery::stub(create_results_summary, "covr::percent_coverage", function(x) 75.0)
  
  # Run the function
  result <- create_results_summary(mock_cvr)
  
  # Check top-level structure
  expect_type(result, "list")
  expect_named(result, c("total_cov", "res_cov"))
  
  # Check total coverage
  expect_equal(result$total_cov, 0.75)
  
  # Check res_cov structure
  expect_type(result$res_cov, "list")
  expect_equal(result$res_cov$name, "mockpkg")
  expect_type(result$res_cov$coverage, "list")
  
  # Check file coverage vector
  filecoverage <- result$res_cov$coverage$filecoverage
  expect_true(is.numeric(filecoverage))
  expect_true(!is.null(dimnames(filecoverage)))
  
  # Check that coverage values are correct
  expect_equal(names(filecoverage), c("file1.R", "file2.R"))
  
  expect_equal(filecoverage["file1.R"], round(100 * 4/5, 2), ignore_attr = TRUE)
  expect_equal(filecoverage["file2.R"], round(100 * 1/3, 2), ignore_attr = TRUE)
  
  
  # Check notes and errors
  expect_true(is.na(result$res_cov$errors))
  expect_true(is.na(result$res_cov$notes))
})


test_that("create_results_summary handles NULL input correctly", {
  # Run the function with NULL input
  result <- create_results_summary(NULL)
  
  # Check top-level structure
  expect_type(result, "list")
  expect_named(result, c("total_cov", "res_cov"))
  
  # Check that total_cov is NA
  expect_true(is.na(result$total_cov))
  
  # Check res_cov structure
  expect_type(result$res_cov, "list")
  expect_true(is.na(result$res_cov$name))
  expect_type(result$res_cov$coverage, "list")
  
  # Check filecoverage is an empty numeric vector with dimnames
  filecoverage <- result$res_cov$coverage$filecoverage
  expect_true(is.numeric(filecoverage))
  expect_equal(length(filecoverage), 1)
  expect_equal(dim(filecoverage), c(1))
  expect_true(is.list(dimnames(filecoverage)))
  expect_equal(length(dimnames(filecoverage)), 1)
  expect_equal(dimnames(filecoverage)[[1]], "no_coverage.R")
  
  
  # Check totalcoverage is NA
  expect_true(is.na(result$res_cov$coverage$totalcoverage))
  
  # Check notes and errors
  expect_true(is.na(result$res_cov$errors))
  expect_true(is.na(result$res_cov$notes))
})

test_that("create_covr_list_no_skip returns structured coverage list", {
  # Mock inputs
  test_map <- data.frame(file = c("test-one.R", "test-two.R"), stringsAsFactors = FALSE)
  test_results_long <- list(
    list(results = list(structure(TRUE, class = "expectation_success"))),
    list(results = list(structure(TRUE, class = "expectation_failure")))
  )
  pkg_source_path <- "mock/path"
  pkg <- "mockpkg"
  
  # ---- Stubs inside create_covr_list_no_skip ----
  # Unload is a no-op
  mockery::stub(create_covr_list_no_skip, "safe_unload_package", function(pkg) TRUE)
  
  # Simulate package load into env
  mockery::stub(
    create_covr_list_no_skip,
    "load_package_into_env",
    function(pkg_source_path, pkg, cov_env) TRUE
  )
  
  # Simulate available datasets via `data()`
  # Keep symbol name as used in the function body: `data`, not `utils::data`
  mockery::stub(
    create_covr_list_no_skip,
    "utils::data",
    function(package, ...) {
      list(results = data.frame(Item = c("iris", "mtcars"), stringsAsFactors = FALSE))
    }
  )
  
  
  # Pretend all referenced test files exist
  mockery::stub(create_covr_list_no_skip, "file.exists", function(path) TRUE)
  
  # Fake test file contents that reference datasets
  mockery::stub(
    create_covr_list_no_skip,
    "readLines",
    function(file, warn = FALSE) c("data(iris)", "data(mtcars)")
  )
  
  # Covr coverage over environment
  mock_coverage <- structure(list(), class = "coverage")
  mockery::stub(
    create_covr_list_no_skip,
    "covr::environment_coverage",
    function(...) mock_coverage
  )
  
  # Convert coverage to list
  mockery::stub(
    create_covr_list_no_skip,
    "covr::coverage_to_list",
    function(x) list(filecoverage = matrix(1, nrow = 1), totalcoverage = 85)
  )
  
  # Cleanup helper returns NULL when asked
  mockery::stub(create_covr_list_no_skip, "cleanup_and_return_null", function(...) NULL)
  
  cov_env <- new.env(parent = emptyenv())
  
  # ---- Run with the new argument explicitly passed ----
  result <- create_covr_list_no_skip(
    test_map = test_map,
    test_results_long = test_results_long,
    pkg_source_path = pkg_source_path,
    pkg = pkg,
    cov_env = cov_env
  )
  
  # ---- Assertions ----
  expect_type(result, "list")
  expect_equal(result$total_cov, 0.85)
  expect_equal(result$res_cov$name, pkg)
  expect_equal(result$res_cov$passed, 1)
  expect_equal(result$res_cov$failed, 1)
  expect_true(is.list(result$tests_skipped))
})


test_that("create_covr_list_no_skip falls back to package_coverage when no datasets used", {
  # Mock inputs
  test_map <- data.frame(file = "test-file.R", stringsAsFactors = FALSE)
  
  # Create mock expectations using official helpers
  mock_success <- testthat::expect_success(expect_true(TRUE))
  mock_failure <- testthat::expect_failure(expect_true(FALSE))
  
  test_results_long <- list(list(results = list(mock_success, mock_failure)))
  
  
  pkg_source_path <- "fake/path"
  pkg <- "fakepkg"
  
  # Stub safe_unload_package and load_package_into_env to do nothing
  mockery::stub(create_covr_list_no_skip, "safe_unload_package", function(pkg) NULL)
  mockery::stub(create_covr_list_no_skip, "load_package_into_env", function(path, pkg, env) NULL)
  
  # Stub data() to return no datasets
  mockery::stub(
    create_covr_list_no_skip,
    "utils::data",
    function(package, ...) {
      list(results = data.frame(Item = character(0), stringsAsFactors = FALSE))
    }
  )
  
  
  # Stub file.exists to always return TRUE
  mockery::stub(create_covr_list_no_skip, "file.exists", function(f) TRUE)
  
  # Stub readLines to return lines that do not match any dataset
  mockery::stub(create_covr_list_no_skip, "readLines", function(file, warn) c("some unrelated code"))
  
  # Stub covr::package_coverage to return a mock coverage object
  mock_coverage <- structure(list(), class = "coverage")
  mockery::stub(create_covr_list_no_skip, "covr::package_coverage", function(path, type) mock_coverage)
  
  # Stub covr::coverage_to_list to return mock coverage list
  mockery::stub(create_covr_list_no_skip, "covr::coverage_to_list", function(cov) {
    list(filecoverage = list(), totalcoverage = 85)
  })
  
  # Stub cleanup_and_return_null to do nothing
  mockery::stub(create_covr_list_no_skip, "cleanup_and_return_null", function(..., env) NULL)
  
  cov_env <- new.env(parent = emptyenv())
  # Run the function
  result <- create_covr_list_no_skip(test_map, 
                                     test_results_long, 
                                     pkg_source_path, 
                                     pkg, 
                                     cov_env = cov_env
                                     )
  
  # Assertions
  expect_type(result, "list")
  expect_equal(result$total_cov, 0.85)
  expect_equal(result$res_cov$name, pkg)
  expect_equal(result$res_cov$coverage$totalcoverage, 85)
})



test_that("create_covr_list_no_skip handles NULL coverage", {
  
  # Mock inputs
  test_map <- data.frame(file = "test-file.R", stringsAsFactors = FALSE)
  
  # Use valid expectation objects
  mock_success <- testthat::expect_success(expect_true(TRUE))
  mock_failure <- testthat::expect_failure(expect_true(FALSE))
  mock_skip    <- structure(list(), class = "expectation_skip")
  
  test_results_long <- list(
    list(results = list(mock_success, mock_failure, mock_skip))
  )
  
  pkg_source_path <- tempdir()
  pkg <- "mockpkg"
  
  # ----------- REQUIRED STUBS -------------
  
  # No-op package unloading
  mockery::stub(create_covr_list_no_skip, "safe_unload_package", function(...) NULL)
  
  # No-op environment loader
  mockery::stub(create_covr_list_no_skip, "load_package_into_env", function(...) NULL)
  
  # CRITICAL: stub utils::data(), not data()
  mockery::stub(
    create_covr_list_no_skip,
    "utils::data",
    function(package, ...) {
      list(results = data.frame(Item = character(0), stringsAsFactors = FALSE))
    }
  )
  
  # Pretend all referenced files exist
  mockery::stub(create_covr_list_no_skip, "file.exists", function(...) TRUE)
  
  # No datasets referenced in test files
  mockery::stub(create_covr_list_no_skip, "readLines", function(...) "some unrelated code")
  
  # Force NULL coverage from covr
  mockery::stub(create_covr_list_no_skip, "covr::package_coverage", function(...) NULL)
  
  # cleanup helper as no-op
  mockery::stub(create_covr_list_no_skip, "cleanup_and_return_null", function(...) NULL)
  
  # ----------------------------------------
  
  cov_env <- new.env(parent = emptyenv())
  
  # Run function using correct signature (named args)
  result <- create_covr_list_no_skip(
    test_map          = test_map,
    test_results_long = test_results_long,
    pkg_source_path   = pkg_source_path,
    pkg               = pkg,
    cov_env           = cov_env
  )
  
  # ---- Assertions for the fallback NULL-coverage branch ----
  expect_type(result, "list")
  expect_equal(result$total_cov, 0)
  
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  
  expect_equal(
    result$res_cov$coverage$filecoverage,
    matrix(0, nrow = 1, dimnames = list("No functions tested"))
  )
  
  expect_equal(result$res_cov$errors, "No testthat or testit configuration")
})




test_that("create_coverage_skip_stf reports no helpers when none exist", {
  pkg_name <- "mockpkg"
  pkg_dir  <- tempdir()
  
  # Create the test directory expected by the function
  test_path <- file.path(pkg_dir, "tests", "testthat")
  dir.create(test_path, recursive = TRUE, showWarnings = FALSE)
  
  # Create an empty test file that matches test_skip$file
  writeLines("test_that('x',{ expect_true(TRUE) })",
             file.path(test_path, "test.R"))
  
  # ---- cov_env must exist AND must contain work_dir ----
  cov_env <- new.env(parent = emptyenv())
  cov_env$work_dir <- file.path(tempdir(), "work")
  dir.create(cov_env$work_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ---- Stubs ----
  # NOTE: setup_covr_env is NOT used in this function (remove your stub)
  mockery::stub(create_coverage_skip_stf, "cleanup_and_return_null", function(...) NULL)
  mockery::stub(create_coverage_skip_stf, "remove_new_globals", function(...) NULL)
  
  mockery::stub(create_coverage_skip_stf, "covr::environment_coverage",
                function(...) structure(list(), class = "coverage"))
  
  # Prevent actual file mutation during test
  mockery::stub(create_coverage_skip_stf, "readLines", function(...) character())
  mockery::stub(create_coverage_skip_stf, "cat", function(...) NULL)
  
  mockery::stub(create_coverage_skip_stf, "safe_unload_package", function(...) NULL)
  
  # Do not actually change directories
  mockery::stub(create_coverage_skip_stf, "withr::with_dir", function(path, code) force(code))
  
  # ---- Capture messages ----
  msgs <- testthat::capture_messages({
    create_coverage_skip_stf(
      pkg_source_path = pkg_dir,
      pkg             = pkg_name,
      test_path       = test_path,
      test_skip       = data.frame(file = "test.R", line1 = 1, line2 = 1),
      test_map        = data.frame(file = "test.R"),
      cov_env         = cov_env
    )
  })
  
  # ---- Assertion ----
  expect_true(any(grepl("No helper files were loaded\\.", msgs)))
})




test_that("create_coverage_skip_stf reports loaded helpers when helpers exist", {
  pkg_name <- "mockpkg"
  pkg_dir  <- tempdir()
  
  # ------------------------------
  # Create real test directory + file
  # ------------------------------
  test_path <- file.path(pkg_dir, "tests", "testthat")
  dir.create(test_path, recursive = TRUE, showWarnings = FALSE)
  
  writeLines(
    "test_that('x',{ expect_true(TRUE) })",
    file.path(test_path, "test.R")
  )
  
  # ------------------------------
  # Create cov_env with required fields
  # ------------------------------
  cov_env <- new.env(parent = emptyenv())
  cov_env$helpers_loaded <- c("helper1.R", "helper2.R")
  cov_env$root_dir       <- pkg_dir
  cov_env$initial_state  <- character()
  cov_env$work_dir       <- file.path(tempdir(), "work")
  dir.create(cov_env$work_dir, recursive = TRUE, showWarnings = FALSE)
  
  # ------------------------------
  # Stubs for dependencies
  # ------------------------------
  mockery::stub(create_coverage_skip_stf, "remove_new_globals", function(...) NULL)
  mockery::stub(create_coverage_skip_stf, "cleanup_and_return_null", function(...) NULL)
  
  mockery::stub(
    create_coverage_skip_stf,
    "covr::environment_coverage",
    function(...) structure(list(), class = "coverage")
  )
  
  # Avoid file writing during comment-out
  mockery::stub(create_coverage_skip_stf, "readLines", function(...) character())
  mockery::stub(create_coverage_skip_stf, "cat", function(...) NULL)
  
  # Avoid directory switching
  mockery::stub(create_coverage_skip_stf, "withr::with_dir", function(path, code) force(code))
  
  # ------------------------------
  # Capture messages
  # ------------------------------
  msgs <- testthat::capture_messages({
    create_coverage_skip_stf(
      pkg_source_path = pkg_dir,
      pkg             = pkg_name,
      test_path       = test_path,
      test_skip       = data.frame(file = "test.R", line1 = 1, line2 = 1),
      test_map        = data.frame(file = "test.R"),
      cov_env         = cov_env
    )
  })
  
  # ------------------------------
  # Assertions
  # ------------------------------
  expect_true(any(grepl("Helper files loaded:", msgs)))
  expect_true(any(grepl("helper1.R", msgs)))
  expect_true(any(grepl("helper2.R", msgs)))
})



test_that("create_test_reporter returns mocked test results", {
  # Mocked test results
  fake_results <- list(
    list(file = "test-a.R", context = "script1"),
    list(file = "test-b.R", context = "script2")
  )
  
  # Stub testthat::test_dir to return fake results
  mockery::stub(create_test_reporter, "testthat::test_dir", function(...) fake_results)
  
  # Stub fix_test_context to just return the same results
  mockery::stub(create_test_reporter, "fix_test_context", function(x) x)
  
  # Call the function
  result <- create_test_reporter(test_path = "tests", pkg = "mockpkg")
  
  # Assertions
  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_equal(result[[1]]$context, "script1")
  expect_equal(result[[2]]$file, "test-b.R")
})

test_that("create_test_reporter handles errors gracefully", {
  # Stub testthat::test_dir to throw an error
  mockery::stub(create_test_reporter, "testthat::test_dir", function(...) stop("Simulated error"))
  
  # Call the function
  result <- create_test_reporter(test_path = "tests", pkg = "mockpkg")
  
  # Assertions
  expect_null(result) # Should return NULL on error
})


test_that("fix_test_context sets context when NULL", {
  # Mock input: context is NULL
  test_results <- list(list(file = "test-script.R", context = NULL))
  
  # Stub tools::file_path_sans_ext to return predictable value
  mockery::stub(fix_test_context, "tools::file_path_sans_ext", function(path) "test-script")
  
  # Call function
  result <- fix_test_context(test_results)
  
  # Assertions
  expect_equal(result[[1]]$context, "test-script")
})

test_that("fix_test_context sets context when empty string", {
  # Mock input: context is empty
  test_results <- list(list(file = "test-script.R", context = ""))
  
  # Stub tools::file_path_sans_ext
  mockery::stub(fix_test_context, "tools::file_path_sans_ext", function(path) "test-script")
  
  # Call function
  result <- fix_test_context(test_results)
  
  # Assertions
  expect_equal(result[[1]]$context, "test-script")
})

test_that("fix_test_context does not change context when already set", {
  # Mock input: context already set
  test_results <- list(list(file = "test-script.R", context = "existing-context"))
  
  # Stub tools::file_path_sans_ext (should not be called)
  mockery::stub(fix_test_context, "tools::file_path_sans_ext", function(path) "test-script")
  
  # Call function
  result <- fix_test_context(test_results)
  
  # Assertions
  expect_equal(result[[1]]$context, "existing-context")
})

test_that("get_stf_test_path returns correct paths based on test_pkg_data", {
  testdir <- tempfile()
  dir.create(testdir)
  
  # Create dummy subdirectories
  dir.create(file.path(testdir, "testthat"))
  dir.create(file.path(testdir, "testit"))
  dir.create(file.path(testdir, "test-ci"))
  dir.create(file.path(testdir, "test-cran"))
  
  # Case: has_testthat
  test_pkg_data <- list(has_testthat = TRUE, has_testit = FALSE, has_tests_base = FALSE)
  expect_equal(get_stf_test_path(test_pkg_data, testdir), file.path(testdir, "testthat"))
  
  # Case: has_testit
  test_pkg_data <- list(has_testthat = FALSE, has_testit = TRUE, has_tests_base = FALSE)
  expect_equal(get_stf_test_path(test_pkg_data, testdir), file.path(testdir, "testit"))
  
  # Case: fallback for nonstandard testit
  test_pkg_data <- list(has_testthat = FALSE, has_testit = FALSE, has_tests_base = FALSE)
  expect_equal(get_stf_test_path(test_pkg_data, testdir), testdir)
  
  # Case: has_tests_base
  unlink(file.path(testdir, "test-ci"), recursive = TRUE)
  unlink(file.path(testdir, "test-cran"), recursive = TRUE)
  test_pkg_data <- list(has_testthat = FALSE, has_testit = FALSE, has_tests_base = TRUE)
  expect_equal(get_stf_test_path(test_pkg_data, testdir), testdir)
  
  # Case: no known test framework
  test_pkg_data <- list(has_testthat = FALSE, has_testit = FALSE, has_tests_base = FALSE)
  expect_null(get_stf_test_path(test_pkg_data, testdir))
  
  # Cleanup
  unlink(testdir, recursive = TRUE)
})

test_that("create_fail_reporter returns results on success", {
  pkg <- "mockpkg"
  test_path <- tempdir()
  
  # 1. Create a mock for the FailReporter results
  mock_results <- list(
    list(file = "test-a.R", context = "script1", status = "FAILED"),
    list(file = "test-b.R", context = "script2", status = "PASS")
  )
  
  # 2. Mock the FailReporter R6 object
  # We stub the 'new' method to return an object with our fake results
  mock_rep <- list(results = mock_results)
  mockery::stub(create_fail_reporter, "testthat::FailReporter$new", mock_rep)
  
  # 3. Stub test_dir to do nothing (preventing the DESCRIPTION/path warning)
  mockery::stub(create_fail_reporter, "testthat::test_dir", function(...) NULL)
  
  # Execute
  result <- create_fail_reporter(test_path, pkg)
  
  # Assertions
  expect_equal(result, mock_results)
  expect_length(result, 2)
})

test_that("create_fail_reporter handles test_dir errors gracefully", {
  pkg <- "mockpkg"
  test_path <- "non/existent/path"
  
  # Stub FailReporter to return a valid object structure
  mockery::stub(create_fail_reporter, "testthat::FailReporter$new", list(results = list()))
  
  # Stub test_dir to throw an error
  mockery::stub(create_fail_reporter, "testthat::test_dir", function(...) {
    stop("Subprocess failure simulation")
  })
  
  # Execute and capture message
  expect_message(
    result <- create_fail_reporter(test_path, pkg),
    regexp = "Error running tests for mockpkg : Subprocess failure simulation"
  )
  
  # Assertions
  expect_null(result)
})