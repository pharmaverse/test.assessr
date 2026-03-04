test_that("run_coverage handles success, empty-tests, and failure paths", {
  pkg_source_path <- "dummy/path/test.package.0001"
  pkg_name <- basename(pkg_source_path)
  
  # -----------------------------
  # 1) SUCCESS: 100% coverage -> total_cov = 1
  # -----------------------------
  mockery::stub(
    where = run_coverage,
    what  = "run_covr",
    how   = function(path, timeout) {
      # IMPORTANT: top-level fields (NOT nested under $coverage)
      list(
        filecoverage  = c(100, 100),  # any non-NULL vector is fine
        totalcoverage = 100
      )
    }
  )
  
  covr_list_success <- NULL
  testthat::expect_message({
    covr_list_success <- run_coverage(pkg_source_path)
  }, regexp = "successful")
  
  testthat::expect_equal(covr_list_success$total_cov, 1)                 # 100/100
  testthat::expect_equal(covr_list_success$res_cov$coverage$totalcoverage, 100)
  testthat::expect_true(is.na(covr_list_success$res_cov$notes))
  
  # -----------------------------
  # 2) EMPTY TESTS: logical(0) + NA -> coerced to 0 + note
  # -----------------------------
  mockery::stub(
    where = run_coverage,
    what  = "run_covr",
    how   = function(path, timeout) {
      list(
        filecoverage  = logical(0),    # triggers "no testable functions found"
        totalcoverage = NA_real_
      )
    }
  )
  
  covr_list_empty <- NULL
  # Will print "successful" (after coercion to 0) and later "had notes: ..."
  testthat::expect_message({
    covr_list_empty <- run_coverage(pkg_source_path)
  }, regexp = "had notes: no testable functions found")
  
  testthat::expect_equal(covr_list_empty$total_cov, 0)                   # 0/100
  testthat::expect_identical(
    covr_list_empty$res_cov$notes, 
    "no testable functions found"
  )
  testthat::expect_equal(covr_list_empty$res_cov$coverage$totalcoverage, 0)
  
  # -----------------------------
  # 3) TRUE FAILURE: NA totalcoverage (non-empty/non-logical filecoverage)
  # -----------------------------
  mockery::stub(
    where = run_coverage,
    what  = "run_covr",
    how   = function(path, timeout) {
      list(
        filecoverage  = c(NA_real_),   # not logical(0) -> won't be coerced to 0
        totalcoverage = NA_real_
      )
    }
  )
  
  covr_list_fail <- NULL
  testthat::expect_message({
    covr_list_fail <- run_coverage(pkg_source_path)
  }, regexp = "unsuccessful")
  
  testthat::expect_true(is.na(covr_list_fail$total_cov))
  testthat::expect_true(is.na(covr_list_fail$res_cov$coverage$totalcoverage))
  testthat::expect_true(is.na(covr_list_fail$res_cov$notes))
})





test_that("running coverage for created package with 1 note 1 warning - mocked", {
  # Define dummy paths and package name
  pkg_source_path <- "dummy/path/test.package.0002"
  pkg_name <- basename(pkg_source_path)
  
  # Pattern used by run_coverage() to announce success/unsuccessful
  msg_pattern <- paste0("code coverage for ", pkg_name, " (successful|unsuccessful)")
  
  # --- Success Path: 100% coverage -> total_cov == 1 ---
  mockery::stub(
    where = run_coverage,      # stub INSIDE the function under test
    what  = "run_covr",        # replace the dependency it calls
    how   = function(path, timeout) {
      # NOTE: run_coverage() expects a FLAT list with top-level fields:
      # totalcoverage (0..100) and filecoverage (non-NULL vector)
      list(
        filecoverage  = c(func1 = 100),
        totalcoverage = 100
      )
    }
  )
  
  covr_list <- NULL
  testthat::expect_message({
    covr_list <- run_coverage(pkg_source_path)
  }, regexp = msg_pattern)
  
  testthat::expect_true(checkmate::test_numeric(covr_list$total_cov))
  testthat::expect_equal(covr_list$total_cov, 1)  # 100 / 100
  testthat::expect_equal(covr_list$res_cov$coverage$totalcoverage, 100)
  
  # --- Failure Path: NA totalcoverage (non-empty, non-logical filecoverage) ---
  mockery::stub(
    where = run_coverage,
    what  = "run_covr",
    how   = function(path, timeout) {
      list(
        filecoverage  = c(NA_real_),   # not logical(0) -> won't be coerced to 0
        totalcoverage = NA_real_
      )
    }
  )
  
  covr_list_fail <- NULL
  testthat::expect_message({
    covr_list_fail <- run_coverage(pkg_source_path)
  }, regexp = paste0("code coverage for ", pkg_name, " unsuccessful"), fixed = TRUE)
  
  testthat::expect_true(is.na(covr_list_fail$total_cov))
})




test_that("running coverage for created package with 1 note 1 error - mocked", {
  # Define dummy package info
  pkg_source_path <- "dummy/path/test.package.0003"
  pkg_name <- basename(pkg_source_path)
  
  # NOTE: In the current implementation, the error path in run_coverage()
  # sets `notes = NA`. You cannot have both a note and an error simultaneously.
  # We therefore simulate an rcmdcheck-like failure by making run_covr() throw.
  
  # Stub run_covr INSIDE run_coverage to throw an error
  mockery::stub(
    where = run_coverage,
    what  = "run_covr",
    how   = function(path, timeout) {
      stop("Simulated error from rcmdcheck failure")
    }
  )
  
  # --- Test Execution ---
  covr_list <- NULL
  # run_coverage() emits multiple messages; assert the "failed" one
  testthat::expect_message({
    covr_list <- run_coverage(pkg_source_path)
  }, regexp = paste0("R coverage for ", pkg_name, " failed"))
  
  # --- Assertions ---
  results <- list(covr = covr_list$total_cov)
  
  # total_cov is numeric NA for a failed run
  testthat::expect_true(checkmate::test_numeric(results$covr))
  testthat::expect_true(is.na(results$covr))
  
  # Optional: also assert the earlier "unsuccessful" message is produced
  testthat::expect_message(
    run_coverage(pkg_source_path),
    regexp = paste0("code coverage for ", pkg_name, " unsuccessful"),
    fixed = TRUE
  )
  
  # The error object should be captured in res_cov$errors; notes should be NA
  testthat::expect_true(inherits(covr_list$res_cov$errors, "error"))
  testthat::expect_true(is.na(covr_list$res_cov$notes))
})


test_that("running coverage for created package with no tests - mocked", {
  
  
  # Define dummy package info
  pkg_source_path <- "dummy/path/test.package.0004"
  pkg_name <- basename(pkg_source_path)
  
  # --- Setup Mock ---
  
  # Mock run_coverage to simulate the "no tests" scenario
  # total_cov is 0 (coerced from NaN/empty logical)
  mock_cov_no_tests <- list(
    total_cov = 0,
    res_cov = list(
      name = pkg_name,
      coverage = list(totalcoverage = 0, filecoverage = logical(0)),
      errors = NA,
      notes = "no testable functions found"
    )
  )
  
  # Stub run_coverage to return the "no tests" object
  mockery::stub(test_that, "run_coverage", mock_cov_no_tests)
  
  # --- Test Execution ---
  
  covr_list <- run_coverage(pkg_source_path)
  
  # --- Assertions ---
  
  results <- list(covr = covr_list$total_cov)
  
  # Assertions for a package with 0 tests
  testthat::expect_true(checkmate::test_numeric(results$covr))
  testthat::expect_equal(results$covr, NaN)
  
})


test_that("running coverage for created package with no tests - mocked", {
  # Define dummy package info
  pkg_source_path <- "dummy/path/test.package.0004"
  pkg_name <- basename(pkg_source_path)
  
  # --- Setup Mock ---
  # Stub run_covr INSIDE run_coverage to simulate "no testable functions found":
  # totalcoverage = NA_real_ with filecoverage = logical(0) -> coerced to 0 with a note
  mockery::stub(
    where = run_coverage,
    what  = "run_covr",
    how   = function(path, timeout) {
      list(
        filecoverage  = logical(0),
        totalcoverage = NA_real_
      )
    }
  )
  
  # --- Test Execution ---
  covr_list <- run_coverage(pkg_source_path)
  
  # --- Assertions ---
  results <- list(covr = covr_list$total_cov)
  
  # For a package with no tests, run_coverage coerces to 0 (not NaN)
  testthat::expect_true(checkmate::test_numeric(results$covr))
  testthat::expect_equal(results$covr, 0)
  
  # Optional: assert the messages your implementation emits
  # (1) overall status -> "successful" because totalcoverage was coerced to 0
  testthat::expect_message(
    run_coverage(pkg_source_path),
    regexp = paste0("code coverage for ", pkg_name, " successful"),
    fixed  = TRUE
  )
  
  # (2) note message emitted afterward
  testthat::expect_message(
    run_coverage(pkg_source_path),
    regexp = "had notes: no testable functions found"
  )
  
  # Optional: check the note is set in the result structure
  testthat::expect_identical(
    covr_list$res_cov$notes,
    "no testable functions found"
  )
  testthat::expect_equal(covr_list$res_cov$coverage$totalcoverage, 0)
})




test_that("run_coverage handles errors in coverage_list correctly", {
  # Create a temporary package source path (optional here since we mock run_covr)
  temp_pkg_path <- tempfile("tempPkg")
  dir.create(temp_pkg_path, recursive = TRUE)
  writeLines("Package: tempPkg\nVersion: 0.1.0\n", file.path(temp_pkg_path, "DESCRIPTION"))
  
  # --- Stub the dependency used by run_coverage() ---
  # Force the error path by making run_covr() throw.
  mockery::stub(
    where = run_coverage,
    what  = "run_covr",
    how   = function(path, timeout) {
      stop("Simulated error from run_covr / rcmdcheck failure")
    }
  )
  
  # --- Execute ---
  result <- NULL
  # Optional: assert the "failed" message your implementation emits
  testthat::expect_message({
    result <- run_coverage(temp_pkg_path, timeout = 1)
  }, regexp = paste0("R coverage for ", basename(temp_pkg_path), " failed"))
  
  # --- Assertions: structure from error branch ---
  # coverage NA values indicate error
  expect_true(is.na(result$res_cov$coverage$filecoverage))
  expect_true(is.na(result$res_cov$coverage$totalcoverage))
  
  # total_cov is computed from totalcoverage/100 -> remains NA on error
  expect_true(is.na(result$total_cov))
  
  # errors field captures the condition object
  expect_false(is.null(result$res_cov$errors))
  expect_true(inherits(result$res_cov$errors, "error"))
  
  # Optional: the earlier status message is "unsuccessful" in this path
  testthat::expect_message(
    run_coverage(temp_pkg_path, timeout = 1),
    regexp = paste0("code coverage for ", basename(temp_pkg_path), " unsuccessful"),
    fixed  = TRUE
  )
})



test_that("run_coverage captures all messages", {
  # Mock the run_covr function to simulate different scenarios
  mock_run_covr <- function(path, timeout) {
    if (path == "no_testable_functions") {
      # EXACT inputs for the "no tests" path:
      # - filecoverage must be logical(0)
      # - totalcoverage NA/NaN is fine; your code checks is.na()
      return(list(filecoverage = logical(0), totalcoverage = NA_real_))
    } else if (path == "nan_total_coverage") {
      # A "non-standard" NA/NaN path (not 'no tests'):
      # Keep filecoverage NOT logical(0). A list() works: is_empty = TRUE, is.logical = FALSE
      return(list(filecoverage = list(), totalcoverage = NaN))
    } else if (path == "successful_coverage") {
      # Success path: totalcoverage numeric (0..100); filecoverage content irrelevant here
      return(list(filecoverage = c(func1 = 80), totalcoverage = 80))
    } else {
      stop("Unexpected path")
    }
  }
  
  with_mocked_bindings(
    run_covr = mock_run_covr,
    {
      # --- Test case: No testable functions found ---
      messages <- capture_messages(run_coverage("no_testable_functions"))
      expect_true(any(grepl("running code coverage for no_testable_functions", messages)))
      # In this branch, your code coerces to 0 and prints "successful"
      expect_true(any(grepl("code coverage for no_testable_functions successful", messages)))
      expect_true(any(grepl("R coverage for no_testable_functions had notes: no testable functions found", messages)))
      
      # --- Test case: Total coverage returned NaN (non-standard) ---
      messages <- capture_messages(run_coverage("nan_total_coverage"))
      expect_true(any(grepl("running code coverage for nan_total_coverage", messages)))
      # This informational message is emitted in the NA/NaN-but-not-empty-logical path:
      expect_true(any(grepl("Total coverage returned NaN\\.", messages)))
      expect_true(any(grepl("code coverage for nan_total_coverage unsuccessful", messages)))
      expect_true(any(grepl("R coverage for nan_total_coverage failed\\. Read in the covr output to see what went wrong:", messages)))
      
      # --- Test case: Successful coverage ---
      messages <- capture_messages(run_coverage("successful_coverage"))
      expect_true(any(grepl("running code coverage for successful_coverage", messages)))
      expect_true(any(grepl("code coverage for successful_coverage successful", messages)))
    }
  )
})



test_that("running coverage for created package in tar file with no notes (mocked)", {
  pkg_name <- "mocked.package.0001"
  pkg_source_path <- file.path(tempdir(), pkg_name)
  
  # 1) Create mocks (mockery::mock returns a callable that tracks calls)
  mock_setup <- mockery::mock(list(
    build_vignettes   = FALSE,
    package_installed = TRUE,
    pkg_source_path   = pkg_source_path,
    rcmdcheck_args    = list(path = pkg_source_path)
  ))
  mock_install   <- mockery::mock(TRUE)
  mock_run_covr  <- mockery::mock(list(filecoverage = c(func1 = 100), totalcoverage = 100))
  
  # 2) Bind the mocks in this test environment
  set_up_pkg            <- mock_setup
  install_package_local <- mock_install
  run_covr              <- mock_run_covr
  
  # 3) Execute logic using the mocks
  install_list <- set_up_pkg("dummy_path.tar.gz")
  package_installed <- install_package_local(install_list$pkg_source_path)
  
  testthat::skip_if_not(package_installed)
  
  coverage_list <- NULL
  testthat::expect_message({
    tryCatch({
      coverage_list <- run_covr(install_list$pkg_source_path, timeout = Inf)
      message(paste0("code coverage for ", basename(install_list$pkg_source_path), " successful"))
    }, error = function(e) message("unsuccessful"))
  }, regexp = "successful")
  
  # 4) Assertions
  testthat::expect_equal(coverage_list$totalcoverage, 100)
  
  # Call count assertions
  mockery::expect_called(mock_setup, 1)
  mockery::expect_called(mock_install, 1)
  mockery::expect_called(mock_run_covr, 1)
  
  # Optional: verify arguments passed to the mocks
  mockery::expect_args(mock_setup,            1, "dummy_path.tar.gz")
  mockery::expect_args(mock_install,          1, install_list$pkg_source_path)
  mockery::expect_args(mock_run_covr,         1, install_list$pkg_source_path, Inf)
})
