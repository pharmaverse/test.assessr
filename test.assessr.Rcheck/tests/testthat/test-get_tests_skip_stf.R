
test_that("get_tests_skip_stf performs safe join with valid input", {
  problem_tests <- data.frame(
    file = c("test1.R", "test2.R"),
    context = c("ctx1", "ctx2"),
    status = c("FAILED", "FAILED"),
    n = c(1, 1),
    time = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )
  
  test_map <- data.frame(
    file = c("test1.R", "test1.R", "test2.R"),
    context = c("ctx1", "ctx1", "ctx2"),
    expectation = c("expect_equal", "expect_true", "expect_error"),
    line1 = c(10, 20, 30),
    line2 = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  result <- get_tests_skip_stf(problem_tests, test_map)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("file", "context", "expectation", "line1", "line2") %in% names(result)))
})

test_that("get_tests_skip_stf returns NULL for invalid problem_tests structure", {
  problem_tests <- data.frame(
    wrong_col = c("test1.R"),
    stringsAsFactors = FALSE
  )
  test_map <- data.frame(
    file = "test1.R",
    context = "ctx1",
    expectation = "expect_equal",
    line1 = 10,
    line2 = 10,
    stringsAsFactors = FALSE
  )
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})

test_that("get_tests_skip_stf returns NULL for invalid test_map structure", {
  problem_tests <- data.frame(
    file = "test1.R",
    context = "ctx1",
    status = "FAILED",
    n = 1,
    time = 0.1,
    stringsAsFactors = FALSE
  )
  test_map <- data.frame(
    wrong_col = "test1.R",
    stringsAsFactors = FALSE
  )
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})

test_that("get_tests_skip_stf returns NULL when join result is empty", {
  problem_tests <- data.frame(
    file = "nonexistent.R",
    context = "missing_ctx",
    status = "FAILED",
    n = 1,
    time = 0.1,
    stringsAsFactors = FALSE
  )
  
  test_map <- data.frame(
    file = "test1.R",
    context = "ctx1",
    expectation = "expect_equal",
    line1 = 10,
    line2 = 10,
    stringsAsFactors = FALSE
  )
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})


test_that("get_tests_skip_stf returns NULL when problem_tests is not a data frame", {
  problem_tests <- list(file = "test1.R", context = "ctx1")  # Not a data frame
  test_map <- data.frame(
    file = "test1.R",
    context = "ctx1",
    expectation = "expect_equal",
    line1 = 10,
    line2 = 10,
    stringsAsFactors = FALSE
  )
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})


test_that("get_tests_skip_stf returns NULL when problem_tests has no rows", {
  problem_tests <- data.frame(
    file = character(0),
    context = character(0),
    status = character(0),
    n = integer(0),
    time = numeric(0),
    stringsAsFactors = FALSE
  )
  
  test_map <- data.frame(
    file = "test1.R",
    context = "ctx1",
    expectation = "expect_equal",
    line1 = 10,
    line2 = 10,
    stringsAsFactors = FALSE
  )
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})


test_that("get_tests_skip_stf returns NULL when test_map is not a data frame", {
  problem_tests <- data.frame(
    file = "test1.R",
    context = "ctx1",
    status = "FAILED",
    n = 1,
    time = 0.1,
    stringsAsFactors = FALSE
  )
  
  test_map <- list(file = "test1.R", context = "ctx1")  # Not a data frame
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})

test_that("get_tests_skip_stf returns NULL when test_map has missing values", {
  problem_tests <- data.frame(
    file = "test1.R",
    context = "ctx1",
    status = "FAILED",
    n = 1,
    time = 0.1,
    stringsAsFactors = FALSE
  )
  
  test_map <- data.frame(
    file = c("test1.R", NA),
    context = c("ctx1", "ctx1"),
    expectation = c("expect_equal", "expect_true"),
    line1 = c(10, 20),
    line2 = c(10, 20),
    stringsAsFactors = FALSE
  )
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})

test_that("get_tests_skip_stf returns NULL when test_map has zero rows", {
  problem_tests <- data.frame(
    file = "test1.R",
    context = "ctx1",
    status = "FAILED",
    n = 1,
    time = 0.1,
    stringsAsFactors = FALSE
  )
  
  test_map <- data.frame(
    file = character(),
    context = character(),
    expectation = character(),
    line1 = integer(),
    line2 = integer(),
    stringsAsFactors = FALSE
  )
  
  result <- suppressWarnings(get_tests_skip_stf(problem_tests, test_map))
  expect_null(result)
})


test_that("get_tests_skip_stf handles error during test_map filtering", {
  # Prepare valid problem_tests and test_map data frames
  problem_tests <- data.frame(file = "test.R", context = "ctx", stringsAsFactors = FALSE)
  test_map <- data.frame(file = "test.R", context = "ctx", stringsAsFactors = FALSE)
  
  # Stub the subsetting operation to throw an error
  mockery::stub(get_tests_skip_stf, "[", function(...) stop("Simulated subsetting error"))
  
  # Capture warnings
  warnings <- capture_warnings({
    result <- get_tests_skip_stf(problem_tests, test_map)
  })
  
  # Assertions
  expect_null(result)
  expect_true(any(grepl("Error filtering test_map", warnings)))
})

test_that("successful join returns expected data frame", {
  df1 <- data.frame(id = 1:2, val = c("a", "b"))
  df2 <- data.frame(id = 1:2, extra = c("x", "y"))
  
  result <- safe_left_join(df1, df2, by = "id")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("extra" %in% names(result))
})


test_that("safe_left_join returns NULL and warns on error", {
  df1 <- data.frame(id = 1)
  df2 <- data.frame(id_wrong = 1)
  
  expect_warning(
    result <- safe_left_join(df1, df2, by = "id"),
    regexp = "Error joining data frames"
  )
  
  expect_null(result)
})


test_that("empty data frames still join correctly", {
  df1 <- data.frame(id = integer())
  df2 <- data.frame(id = integer())
  
  result <- safe_left_join(df1, df2, by = "id")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("join with missing columns triggers warning and returns NULL", {
  df1 <- data.frame(id = 1:2)
  df2 <- data.frame(other = 1:2)
  
  expect_warning(
    result <- safe_left_join(df1, df2, by = "id"),
    regexp = "Error joining data frames"
  )
  expect_null(result)
})


test_that("safe_left_join handles many-to-many relationships correctly", {
  # Create data frames with duplicate keys
  df1 <- data.frame(id = c(1, 1, 2), val1 = c("A", "B", "C"))
  df2 <- data.frame(id = c(1, 1, 2), val2 = c("X", "Y", "Z"))
  
  # Perform join
  result <- suppressWarnings(safe_left_join(df1, df2, by = "id"))
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "val1", "val2") %in% names(result)))
  
  # Expected number of rows: Cartesian product for duplicates
  # df1 has 2 rows with id=1, df2 has 2 rows with id=1 → 4 rows for id=1
  # id=2 matches 1 row each → 1 row
  expect_equal(nrow(result), 5)
  
  # Check that all combinations exist for id=1
  combos_id1 <- result[result$id == 1, c("val1", "val2")]
  expect_true(all(c("A", "B") %in% combos_id1$val1))
  expect_true(all(c("X", "Y") %in% combos_id1$val2))
})

test_that("safe_left_join warns and returns NULL when many-to-many join fails", {
  # Force an error by stubbing left_join
  mockery::stub(safe_left_join, "dplyr::left_join", function(...) {
    stop("Simulated join error")
  })
  
  expect_warning(
    result <- safe_left_join(data.frame(id = 1), data.frame(id = 1), by = "id"),
    regexp = "Error joining data frames"
  )
  
  expect_null(result)
})


test_that("returns NULL and warns when safe_left_join returns NULL", {
  # Mock safe_left_join to return NULL
  mockery::stub(get_tests_skip_stf, "safe_left_join", NULL)
  
  problem_tests <- data.frame(file = "file1", context = "ctx1")
  test_map <- data.frame(file = "file1", context = "ctx1")
  
  expect_warning(
    result <- get_tests_skip_stf(problem_tests, test_map),
    "Join result is empty or invalid."
  )
  expect_null(result)
})


test_that("returns NULL and warns when safe_left_join returns empty data frame", {
  # Mock safe_left_join to return empty data frame
  mockery::stub(get_tests_skip_stf, "safe_left_join", data.frame())
  
  problem_tests <- data.frame(file = "file1", context = "ctx1")
  test_map <- data.frame(file = "file1", context = "ctx1")
  
  expect_warning(
    result <- get_tests_skip_stf(problem_tests, test_map),
    "Join result is empty or invalid."
  )
  expect_null(result)
})

