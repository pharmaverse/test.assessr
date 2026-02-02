
test_that("run_covr_skip_nstf returns correct structure on success", {
  # Mock data
  pkg_source_path <- "mock/path"
  test_pkg_data <- list(has_testit = FALSE)
  mapping <- data.frame(
    source_file = "file1.R",
    test_file = "test-file1.R",
    stringsAsFactors = FALSE
  )
  problems <- data.frame(
    test_file = "test-file1.R",
    issue_type = "skipped",
    stringsAsFactors = FALSE
  )
  no_tests_df <- data.frame(
    source_file = "file2.R",
    stringsAsFactors = FALSE
  )
  coverage_obj <- structure(list(), class = "coverage")
  coverage_list <- list(filecoverage = matrix(1), totalcoverage = 80)
  
  # Stubbing dependencies
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", "mockpkg")
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", "mock/tests/testthat")
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf", problems)
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", no_tests_df)
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", c("fun1", "fun2"))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", c("dataset1"))
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", coverage_obj)
  mockery::stub(run_covr_skip_nstf, "covr::coverage_to_list", coverage_list)
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  # Run function
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  
  # Check structure
  expect_type(result, "list")
  expect_true("total_cov" %in% names(result))
  expect_true("res_cov" %in% names(result))
  expect_true("functions_no_tests" %in% names(result))
  expect_true("tests_skipped" %in% names(result))
  expect_true("tests_passing" %in% names(result))
  expect_gt(result$total_cov, 0)
})


test_that("run_covr_skip_nstf returns testit nonzero coverage when at least one test passes", {
  pkg_source_path <- "mockpkg"
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  mapping <- data.frame(source_file = "foo.R", test_file = "test-foo.R", stringsAsFactors = FALSE)
  test_path <- "mockpkg/tests/testit"
  
  # Stubs
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  
  # Simulate all tests passing (no problems)
  fake_problems <- data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE)
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", fake_problems)
  
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", c("fun1"))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", character(0))
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  # Simulate coverage object and coverage list with nonzero coverage
  fake_coverage <- structure(list(), class = "coverage")
  fake_coverage_list <- list(
    filecoverage = matrix(1, nrow = 1, dimnames = list("foo.R")),
    totalcoverage = 100
  )
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", fake_coverage)
  mockery::stub(run_covr_skip_nstf, "covr::coverage_to_list", fake_coverage_list)
  
  # Run
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  
  # Check
  expect_type(result, "list")
  expect_true(result$total_cov > 0)
  expect_equal(result$res_cov$coverage$totalcoverage, 100)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "foo.R")
  expect_equal(result$tests_skipped, character(0)) # No skipped tests
  expect_equal(result$tests_passing, file.path(pkg_source_path, "tests/testit", "test-foo.R"))
})


test_that("run_covr_skip_nstf handles multiple problems from check_covr_skip_testit", {
  pkg_source_path <- "mockpkg"
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  mapping <- data.frame(source_file = "foo.R", test_file = "test1.R", stringsAsFactors = FALSE)
  test_path <- "mockpkg/tests/testit"
  
  # Mock dependencies
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  # This is the key stub for this test
  fake_problems <- data.frame(
    test_file = c("test1.R", "test2.R"),
    issue_type = c("error: Test error!", "warning: Test warning!"),
    stringsAsFactors = FALSE
  )
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", fake_problems)
  
  # Stub the rest to avoid side effects
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", character(0))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", character(0))
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", NULL)
  
  # Run
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  
 
  expect_type(result, "list")
  expect_equal(result$total_cov, 0)
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "No functions tested")
  expect_equal(as.numeric(result$res_cov$coverage$filecoverage[1, 1]), 0)
  expect_equal(result$res_cov$errors, "No testthat or testit configuration")
  expect_true(is.na(result$res_cov$notes))
  
})


test_that("run_covr_skip_nstf handles error in coverage_to_list", {
  pkg_source_path <- "mockpkg"
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  mapping <- data.frame(source_file = "foo.R", test_file = "test-foo.R", stringsAsFactors = FALSE)
  test_path <- "mockpkg/tests/testit"
  
  # Stubs for setup
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", data.frame(test_file = NA_character_, issue_type = "No tests skipped", stringsAsFactors = FALSE))
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame())
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", c("fun1"))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", character(0))
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  # Simulate a valid coverage object
  fake_coverage <- structure(list(), class = "coverage")
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", fake_coverage)
  
  # Simulate coverage_to_list throwing an error
  mockery::stub(run_covr_skip_nstf, "covr::coverage_to_list", function(x) stop("Simulated coverage_to_list error"))
  
  # Run
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  
  # Check error handling in res_cov
  expect_type(result, "list")
  expect_true("res_cov" %in% names(result))
  expect_equal(result$res_cov$name, pkg_name)
  expect_true(is.na(result$res_cov$coverage$filecoverage))
  expect_true(is.na(result$res_cov$coverage$totalcoverage))
  expect_s3_class(result$res_cov$errors, "simpleError")
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
  pkg_source_path <- "mockpkg"
  test_pkg_data <- list(has_testit = FALSE)
  pkg_name <- "mockpkg"
  test_path <- "mockpkg/tests"
  
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  # Simulate error in get_source_test_mapping_nstf
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", function(...) stop("Simulated mapping error"))
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  expect_null(result)
})


test_that("returns NULL if check_covr_skip_testit errors", {
  pkg_source_path <- "mockpkg"
  test_pkg_data <- list(has_testit = TRUE)
  pkg_name <- "mockpkg"
  mapping <- data.frame(source_file = "foo.R", test_file = "test-foo.R", stringsAsFactors = FALSE)
  test_path <- "mockpkg/tests/testit"
  
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  # Simulate error in check_covr_skip_testit
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_testit", function(...) stop("Simulated testit error"))
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  expect_null(result)
})


test_that("returns NULL if check_covr_skip_nstf errors", {
  pkg_source_path <- "mockpkg"
  test_pkg_data <- list(has_testit = FALSE)
  pkg_name <- "mockpkg"
  mapping <- data.frame(source_file = "foo.R", test_file = "test-foo.R", stringsAsFactors = FALSE)
  test_path <- "mockpkg/tests"
  
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", pkg_name)
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", test_path)
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", mapping)
  # Simulate error in check_covr_skip_nstf
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf", function(...) stop("Simulated nstf error"))
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  result <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  expect_null(result)
})

test_that("run_covr_skip_nstf returns NULL if problems fails", {
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", "mockpkg")
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", "mock/tests/testthat")
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", data.frame(source_file = "file1.R", test_file = "test-file1.R"))
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf", NULL)
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  result <- run_covr_skip_nstf("mock/path", list(has_testit = FALSE))
  expect_null(result)
})

test_that("run_covr_skip_nstf handles no coverage", {
  mockery::stub(run_covr_skip_nstf, "get_pkg_name", "mockpkg")
  mockery::stub(run_covr_skip_nstf, "get_nstf_test_path", "mock/tests/testthat")
  mockery::stub(run_covr_skip_nstf, "get_source_test_mapping_nstf", data.frame(source_file = "file1.R", test_file = "test-file1.R"))
  mockery::stub(run_covr_skip_nstf, "check_covr_skip_nstf", data.frame(test_file = "test-file1.R", issue_type = "skipped"))
  mockery::stub(run_covr_skip_nstf, "get_function_no_tests", data.frame(source_file = "file2.R"))
  mockery::stub(run_covr_skip_nstf, "pkgload::load_all", NULL)
  mockery::stub(run_covr_skip_nstf, "ls", c("fun1", "fun2"))
  mockery::stub(run_covr_skip_nstf, "get", function(obj, envir) obj)
  mockery::stub(run_covr_skip_nstf, "load_package_datasets", c("dataset1"))
  mockery::stub(run_covr_skip_nstf, "covr::environment_coverage", NULL)
  mockery::stub(run_covr_skip_nstf, "cleanup_and_return_null", NULL)
  mockery::stub(run_covr_skip_nstf, "normalizePath", function(path, ...) path)
  
  result <- run_covr_skip_nstf("mock/path", list(has_testit = FALSE))
  expect_type(result, "list")
  expect_equal(result$total_cov, 0)
})


test_that("returns problems for test files with errors and warnings", {
  dummy_mapping <- data.frame(
    source_file = c("modA.R", "modB.R"),
    test_file = c("test-modA.R", "test-modB.R"),
    stringsAsFactors = FALSE
  )
  
  # Stub file.path to return predictable paths
  mockery::stub(check_covr_skip_nstf, "file.path", function(...) paste(..., sep = "/"))
  
  # Stub source to simulate error and warning
  mockery::stub(check_covr_skip_nstf, "source", function(file, local) {
    if (grepl("test-modA", file)) stop("error in test-modA")
    if (grepl("test-modB", file)) warning("warning in test-modB")
  })
  
  result <- check_covr_skip_nstf("/mock/pkg", dummy_mapping)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$test_file, c("test-modA.R"))
  expect_equal(sort(result$issue_type), c("error"))
})

test_that("returns default message when no problems found", {
  dummy_mapping <- data.frame(
    source_file = c("modA.R"),
    test_file = c("test-modA.R"),
    stringsAsFactors = FALSE
  )
  
  # Stub file.path
  mockery::stub(check_covr_skip_nstf, "file.path", function(...) paste(..., sep = "/"))
  
  # Stub source to simulate successful execution
  mockery::stub(check_covr_skip_nstf, "source", function(file, local) NULL)
  
  result <- check_covr_skip_nstf("/mock/pkg", dummy_mapping)
  
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
  
  # Stub file.path
  mockery::stub(check_covr_skip_nstf, "file.path", function(...) paste(..., sep = "/"))
  
  # Stub source to simulate successful execution
  mockery::stub(check_covr_skip_nstf, "source", function(file, local) NULL)
  
  result <- check_covr_skip_nstf("/mock/pkg", dummy_mapping)
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file))
  expect_equal(result$issue_type, "No tests skipped")
})


test_that("returns 'No tests skipped' if all tests source fine", {
  mapping <- data.frame(test_file = c("test1.R", "test2.R"), stringsAsFactors = FALSE)
  pkg_source_path <- "mockpkg"
  
  # stub source to always succeed (returns NULL)
  mockery::stub(check_covr_skip_nstf, "source", NULL)
  
  result <- check_covr_skip_nstf(pkg_source_path, mapping)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file[1]))
  expect_equal(result$issue_type[1], "No tests skipped")
})

test_that("returns problems for tests that error or warn", {
  mapping <- data.frame(test_file = c("test1.R", "test2.R", "test3.R"), stringsAsFactors = FALSE)
  pkg_source_path <- "mockpkg"
  
  # stub source: first is fine, second errors, third warns
  mockery::stub(check_covr_skip_nstf, "source", function(path, local) {
    if (grepl("test2", path)) stop("fail")
    if (grepl("test3", path)) warning("warn")
    NULL
  })
  
  result <- check_covr_skip_nstf(pkg_source_path, mapping)
  expect_equal(nrow(result), 1)
  expect_setequal(result$test_file, c("test2.R"))
  expect_setequal(result$issue_type, c("error"))
})

test_that("drops NA rows in mapping", {
  mapping <- data.frame(test_file = c("test1.R", NA), stringsAsFactors = FALSE)
  pkg_source_path <- "mockpkg"
  
  mockery::stub(check_covr_skip_nstf, "source", NULL)
  mockery::stub(check_covr_skip_nstf, "tidyr::drop_na", data.frame(test_file = "test1.R", stringsAsFactors = FALSE))
  
  result <- check_covr_skip_nstf(pkg_source_path, mapping)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$test_file[1]))
  expect_equal(result$issue_type[1], "No tests skipped")
})

test_that("handles empty mapping", {
  mapping <- data.frame(test_file = character(0), stringsAsFactors = FALSE)
  pkg_source_path <- "mockpkg"
  
  mockery::stub(check_covr_skip_nstf, "source", NULL)
  
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


# A normalizePath mock: return two values in order (for testdir then pkg_root)
norm_mock <- function() mockery::mock("/pkg/tests", "/pkg")

# dir.exists mock factory: returns TRUE for any path in 'truthy' set
dir_exists_fun <- function(truthy) {
  function(p) {
    p %in% truthy
  }
}

# list.files mock factory: mapping is a named list where names are directory paths.
# It returns the vector for the requested path or character(0) if none.
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
    has_bioc_unit_tests = FALSE
) {
  list(
    has_testthat = has_testthat,
    has_testit = has_testit,
    has_tests_base = has_tests_base,
    has_bioc_unit_tests = has_bioc_unit_tests
  )
}


test_that("returns testthat path when tests/testthat exists", {
  test_pkg_data <- tpd()
  
  # stub normalizePath (two calls)
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  # testthat dir exists, tests root exists; others don't
  true_dirs <- c("/pkg/tests", "/pkg/tests/testthat")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  # no base .R files in tests root
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "testthat")
  expect_identical(res$testthat, "/pkg/tests/testthat")
})

test_that("returns testit path when tests/testit exists", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  true_dirs <- c("/pkg/tests", "/pkg/tests/testit")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "testit")
  expect_identical(res$testit, "/pkg/tests/testit")
})

test_that("includes nonstandard testit root when test-ci and test-cran both exist", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  true_dirs <- c("/pkg/tests", "/pkg/tests/test-ci", "/pkg/tests/test-cran")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "nonstandard_testit_root")
  expect_identical(res$nonstandard_testit_root, "/pkg/tests")
})

test_that("detects base R tests when .R files are directly under tests/", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  true_dirs <- c("/pkg/tests")  # tests root exists
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  # simulate R files directly in tests/
  files_map <- list("/pkg/tests" = c("/pkg/tests/a.R", "/pkg/tests/b.R"))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(files_map))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "base_tests_root")
  expect_identical(res$base_tests_root, "/pkg/tests")
})

test_that("detects Bioconductor unit tests under inst/unitTests", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  true_dirs <- c("/pkg/tests", "/pkg/inst/unitTests")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "bioc_unitTests")
  expect_identical(res$bioc_unitTests, "/pkg/inst/unitTests")
})

test_that("de-duplicates when both nonstandard_testit_root and base_tests_root resolve to tests/", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  true_dirs <- c("/pkg/tests", "/pkg/tests/test-ci", "/pkg/tests/test-cran")
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  # base files present
  files_map <- list("/pkg/tests" = c("/pkg/tests/only_base.R"))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(files_map))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  # Should keep only the first appearance (nonstandard_testit_root)
  expect_length(res, 1L)
  expect_named(res, "nonstandard_testit_root")
  expect_identical(res$nonstandard_testit_root, "/pkg/tests")
})

test_that("falls back to default_tests_root when nothing detected", {
  test_pkg_data <- tpd()
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  # No directories exist (not even /pkg/tests) -> has_base_R_in_tests_root will return FALSE
  true_dirs <- character(0)
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(true_dirs))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "default_tests_root")
  expect_identical(res$default_tests_root, "/pkg/tests")
})

test_that("flag-driven inclusion: has_testthat = TRUE includes tests/testthat even if dir doesn't exist", {
  test_pkg_data <- tpd(has_testthat = TRUE)
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  # Simulate that no directories exist; list.files irrelevant here
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(character(0)))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  # Since flag is TRUE, testthat should be included regardless of FS state.
  expect_named(res, "testthat")
  expect_identical(res$testthat, "/pkg/tests/testthat")
})

test_that("flag-driven inclusion: has_bioc_unit_tests = TRUE includes inst/unitTests even if dir doesn't exist", {
  test_pkg_data <- tpd(has_bioc_unit_tests = TRUE)
  
  mockery::stub(get_nstf_test_path, "normalizePath", norm_mock())
  mockery::stub(get_nstf_test_path, "dir.exists", dir_exists_fun(character(0)))
  mockery::stub(get_nstf_test_path, "list.files", list_files_fun(list()))
  
  res <- get_nstf_test_path(test_pkg_data, "IGNORED")
  
  expect_named(res, "bioc_unitTests")
  expect_identical(res$bioc_unitTests, "/pkg/inst/unitTests")
})
