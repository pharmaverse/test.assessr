test_that("run_covr returns successful coverage list", {
  path <- "my/pkg/path"
  timeout <- 60
  expected_output <- list(totalcoverage = 80, filecoverage = c(a = 80))
  
  # Mock callr::r_safe to return the expected coverage list directly
  mockery::stub(run_covr, "callr::r_safe", expected_output)
  
  result <- run_covr(path, timeout)
  
  testthat::expect_equal(result, expected_output)
})

test_that("run_covr passes correct arguments to r_safe", {
  path <- "test_path"
  timeout <- 100
  
  m <- mockery::mock(list(totalcoverage = 0))
  mockery::stub(run_covr, "callr::r_safe", m)
  
  run_covr(path, timeout)
  
  # Verify callr was called with correct args and timeout
  args <- mockery::mock_args(m)[[1]]
  testthat::expect_equal(args$args, list(path))
  testthat::expect_equal(args$timeout, timeout)
})

test_that("run_covr propagates errors from callr", {
  # Mock callr to throw an error (e.g., timeout or process crash)
  mockery::stub(run_covr, "callr::r_safe", function(...) stop("Timeout reached"))
  
  testthat::expect_error(run_covr("path", 1), "Timeout reached")
})

test_that("the anonymous function inside run_covr calls covr functions correctly", {
  test_path <- "mock/pkg/path"
  
  # 1. Setup mocks for the functions called INSIDE the anonymous closure
  # We use mockery::mock to track if they were called
  m_pkg_cov <- mockery::mock("raw_coverage_object")
  m_cov_list <- mockery::mock(list(totalcoverage = 100))
  
  # 2. Stub r_safe so we can capture the anonymous function
  m_r_safe <- mockery::mock(TRUE)
  mockery::stub(run_covr, "callr::r_safe", m_r_safe)
  
  # Call the wrapper to trigger the stub
  run_covr(test_path, timeout = 10)
  
  # 3. Extract the anonymous function (it's the first argument to r_safe)
  r_safe_args <- mockery::mock_args(m_r_safe)[[1]]
  inner_fun <- r_safe_args[[1]] # The anonymous function(p) { ... }
  
  # 4. Execute the extracted function in an environment where covr is mocked
  # We use with_mocked_bindings from testthat or mockery::stub logic
  withr::with_package("covr", {
    mockery::stub(inner_fun, "covr::package_coverage", m_pkg_cov, depth = 1)
    mockery::stub(inner_fun, "covr::coverage_to_list", m_cov_list, depth = 1)
    
    inner_fun(test_path)
  })
  
  # 5. Assertions: Did the inner function pass the path to covr?
  mockery::expect_called(m_pkg_cov, 1)
  testthat::expect_equal(mockery::mock_args(m_pkg_cov)[[1]][[1]], test_path)
  
  # Did it pass the results of package_coverage to coverage_to_list?
  mockery::expect_called(m_cov_list, 1)
  testthat::expect_equal(mockery::mock_args(m_cov_list)[[1]][[1]], "raw_coverage_object")
})
