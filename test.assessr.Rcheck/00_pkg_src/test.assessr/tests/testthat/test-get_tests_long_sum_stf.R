test_that("get_tests_long_sum_stf handles PASS and formats output correctly", {
  # Mocked expectation object
  mock_result <- list(
    message = "All good",
    srcref = c(10, 3, 10, 49, 3, 49, 10, 10),
    trace = NULL,
    start_frame = 36,
    end_frame = 38,
    test = "test A"
  )
  class(mock_result) <- c("expectation_success", "expectation", "condition")
  
  mock_input <- list(
    list(
      file = "test-file.R",
      context = "Context A",
      test = "Test A",
      real = 0.123,
      results = list(mock_result)
    )
  )
  
  result <- get_tests_long_sum_stf(mock_input)
  
  expect_type(result, "list")
  expect_equal(nrow(result), 1)
  expect_equal(result$status, "PASS")
  expect_equal(result$n, 1)
  expect_equal(result$time, 0.123)
  expect_true(grepl("test-file.R#L10_L10", result$file))
})




test_that("get_tests_long_sum_stf handles all statuses with varied srcref lines", {
  # PASS
  mock_result_pass <- list(
    message = "All good",
    srcref = c(5, 3, 5, 49, 3, 49, 6, 6),  # srcref[1] = 5, srcref[7] = 6
    trace = NULL,
    start_frame = 36,
    end_frame = 38,
    test = "test PASS"
  )
  class(mock_result_pass) <- c("expectation_success", "expectation", "condition")
  
  # FAILED
  mock_result_failed <- list(
    message = "Expected 2 but got 3",
    srcref = c(15, 3, 15, 49, 3, 49, 16, 17),  # srcref[1] = 15, srcref[7] = 16
    trace = NULL,
    start_frame = 36,
    end_frame = 38,
    test = "test FAILED"
  )
  class(mock_result_failed) <- c("expectation_failure", "expectation", "condition")
  
  # ERROR
  mock_result_error <- list(
    message = "Error: object not found",
    srcref = c(25, 3, 25, 49, 3, 49, 26, 28),  # srcref[1] = 25, srcref[7] = 26
    trace = NULL,
    start_frame = 36,
    end_frame = 38,
    test = "test ERROR"
  )
  class(mock_result_error) <- c("expectation_error", "expectation", "condition")
  
  # SKIPPED
  mock_result_skipped <- list(
    message = "Test skipped due to condition",
    srcref = c(35, 3, 35, 49, 3, 49, 36, 39),  # srcref[1] = 35, srcref[7] = 36
    trace = NULL,
    start_frame = 36,
    end_frame = 38,
    test = "test SKIPPED"
  )
  class(mock_result_skipped) <- c("expectation_skip", "expectation", "condition")
  
  # WARNING
  mock_result_warning <- list(
    message = "Warning: something might be wrong",
    srcref = c(45, 3, 45, 49, 3, 49, 46, 50),  # srcref[1] = 45, srcref[7] = 46
    trace = NULL,
    start_frame = 36,
    end_frame = 38,
    test = "test WARNING"
  )
  class(mock_result_warning) <- c("expectation_success", "expectation", "condition")
  
  # Combine all into mock input
  mock_input <- list(
    list(file = "test-file.R", context = "Context A", test = "Test A", real = 0.123, results = list(mock_result_pass)),
    list(file = "test-file.R", context = "Context B", test = "Test B", real = 0.234, results = list(mock_result_failed)),
    list(file = "test-file.R", context = "Context C", test = "Test C", real = 0.345, results = list(mock_result_error)),
    list(file = "test-file.R", context = "Context D", test = "Test D", real = 0.456, results = list(mock_result_skipped)),
    list(file = "test-file.R", context = "Context E", test = "Test E", real = 0.567, results = list(mock_result_warning))
  )
  
  result <- get_tests_long_sum_stf(mock_input)
  
  expect_equal(nrow(result), 5)
  expect_equal(result$status, c("PASS", "FAILED", "ERROR", "SKIPPED", "WARNING"))
  expect_equal(result$n, rep(1, 5))
  expect_equal(result$time, c(0.123, 0.234, 0.345, 0.456, 0.567))
  expect_true(grepl("test-file.R#L5_L6", result$file[1]))
  expect_true(grepl("test-file.R#L15_L16", result$file[2]))
  expect_true(grepl("test-file.R#L25_L26", result$file[3]))
  expect_true(grepl("test-file.R#L35_L36", result$file[4]))
  expect_true(grepl("test-file.R#L45_L46", result$file[5]))
})


test_that("get_tests_long_sum_stf returns NULL for empty input", {
  empty_input <- list()
  result <- get_tests_long_sum_stf(empty_input)
  expect_null(result)
})


test_that("else block is handled with correct srcref", {
  mock_result_else <- list(
    message = "Handled by else block",
    srcref = c(1, 3, 1, 49, 3, 49, 32, 32),  # srcref[1] = 1, srcref[7] = 32
    trace = NULL,
    start_frame = 36,
    end_frame = 38,
    test = "test ELSE"
  )
  class(mock_result_else) <- c("expectation_success", "expectation", "condition")
  
  mock_input <- list(
    list(file = "test-file.R", context = "Context F", test = "Test F", real = 0.678, results = list(mock_result_else))
  )
  
  result <- get_tests_long_sum_stf(mock_input)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$status, "PASS")
  expect_equal(result$n, 1)
  expect_equal(result$time, 0.678)
  expect_true(grepl("test-file.R#L1_L32", result$file[1]))
})

