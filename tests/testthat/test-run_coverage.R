test_that("run_coverage returns correct fraction on success", {
  pkg_path <- "path/to/test.pkg"
  
  # Mock run_covr to return 75% coverage
  mock_cov_list <- list(totalcoverage = 75, filecoverage = c(a = 75))
  mockery::stub(run_coverage, "run_covr", mock_cov_list)
  
  testthat::expect_message(
    res <- run_coverage(pkg_path),
    "code coverage for test.pkg successful"
  )
  
  # Check fraction conversion (75/100 = 0.75)
  testthat::expect_equal(res$total_cov, 0.75)
  testthat::expect_equal(res$res_cov$name, "test.pkg")
})

test_that("run_coverage handles packages with no testable functions", {
  pkg_path <- "path/to/empty.pkg"
  
  # Mock the specific state: NaN total and logical(0) file coverage
  mock_cov_list <- list(totalcoverage = NaN, filecoverage = logical(0))
  mockery::stub(run_coverage, "run_covr", mock_cov_list)
  
  testthat::expect_message(
    res <- run_coverage(pkg_path),
    "R coverage for empty.pkg had notes: no testable functions found"
  )
  
  # Code coerces NaN to 0 in this specific case
  testthat::expect_equal(res$total_cov, 0)
  testthat::expect_equal(res$res_cov$notes, "no testable functions found")
})

test_that("run_coverage catches errors from run_covr", {
  pkg_path <- "path/to/failing.pkg"
  
  # Force run_covr to throw an error
  mockery::stub(run_coverage, "run_covr", function(...) stop("Process Timeout"))
  
  testthat::expect_message(
    res <- run_coverage(pkg_path),
    "code coverage for failing.pkg unsuccessful"
  )
  
  # Verify error capture
  testthat::expect_true(is.na(res$total_cov))
  testthat::expect_s3_class(res$res_cov$errors, "error")
  testthat::expect_match(res$res_cov$errors$message, "Process Timeout")
})

test_that("run_coverage identifies non-standard package failures", {
  pkg_path <- "path/to/weird.pkg"
  
  # NaN total but filecoverage is NOT an empty logical
  mock_cov_list <- list(totalcoverage = NaN, filecoverage = list(complex = 1))
  mockery::stub(run_coverage, "run_covr", mock_cov_list)
  
  testthat::expect_message(
    res <- run_coverage(pkg_path),
    "Total coverage returned NaN"
  )
  
  testthat::expect_true(is.na(res$total_cov))
  testthat::expect_true(is.na(res$res_cov$notes))
})

