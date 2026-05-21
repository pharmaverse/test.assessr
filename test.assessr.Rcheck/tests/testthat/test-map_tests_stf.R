test_that("nest_test correctly groups and names test blocks", {

  
  
  mock_parse_data <- data.frame(
    line1 = 1:6,
    token = c(
      "SYMBOL_FUNCTION_CALL", "STR_CONST", "expr", "expr", "SYMBOL_FUNCTION_CALL", "STR_CONST"
    ),
    text = c(
      "test_that", "\"first test\"", "expr1", "expr2", "describe", "\"second test\""
    ),
    parent = c(0, 1, 1, 1, 0, 5),
    terminal = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  rownames(mock_parse_data) <- as.character(1:nrow(mock_parse_data))
  
    
  # Stub eval(parse(...)) to return the string inside quotes
  mockery::stub(nest_test, "eval", function(expr, keep.source = TRUE) {
      gsub("\"", "", as.character(expr))
    })
  
  result <- nest_test(mock_parse_data)
  
  expect_type(result, "list")
  expect_equal(length(result), 1)
  expect_named(result, "test_that: first test")
  
  expect_s3_class(result[[1]], "data.frame")
  expect_equal(nrow(result[[1]]), 2)
  
})


test_that("get_expect extracts expectation calls and line ranges without NA", {
  
  mock_parse_data <- data.frame(
    line1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    line2 = c(1, 2, 3, 4, 5, 6, 7, 8),
    token = c(
      "expr", "expr", "SYMBOL_FUNCTION_CALL", "expr",
      "expr", "SYMBOL_FUNCTION_CALL", "expr", "expr"
    ),
    text = c(
      "x <- 1", "expect_equal", "expect_equal", "y <- 2",
      "expect_error", "expect_error", "z <- 3", "w <- 4"
    ),
    parent = c(0, 0, 2, 3, 0, 5, 5, 5),
    terminal = rep(TRUE, 8),
    stringsAsFactors = FALSE
  )
  
  # Stub grep inside the function to simulate matching expressions
  mockery::stub(get_expect, "grep", function(pattern, x) {
    # Simulate finding the expectation function in the expression
    which(grepl(pattern, x))
  })
    
  result <- get_expect(mock_parse_data)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$expectation, c("expect_equal", "expect_error"))
  expect_false(any(is.na(result$line1)))
  expect_false(any(is.na(result$line2)))

})

test_that("nest_expect combines expectations from multiple test blocks", {
  

  # Simulated output of get_expect()
  mock_expect_1 <- data.frame(
    expectation = c("expect_equal", "expect_true"),
    line1 = c(10, 12),
    line2 = c(10, 12),
    stringsAsFactors = FALSE
  )
  
  mock_expect_2 <- data.frame(
    expectation = c("expect_error"),
    line1 = c(20),
    line2 = c(20),
    stringsAsFactors = FALSE
  )
  
  # Simulated input to nest_expect (like output from nest_test)
  mock_test_blocks <- list(
    "test_that: test 1" = data.frame(dummy = 1),
    "test_that: test 2" = data.frame(dummy = 2)
  )

  
  # Stub get_expect to return mocked expectations
  mockery::stub(nest_expect, "get_expect", function(x) {
    if (identical(x$dummy, 1)) return(mock_expect_1)
    if (identical(x$dummy, 2)) return(mock_expect_2)
    NULL
  })
  
  # Stub unrowname to add a 'test' column
  mockery::stub(nest_expect, "unrowname", function(name, ret, label) {
    df <- ret[[name]]
    df[[label]] <- name
    df
  })
  
  result <- nest_expect(mock_test_blocks)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$expectation, c("expect_equal", "expect_true", "expect_error"))
  expect_equal(result$test, c("test_that: test 1", "test_that: test 1", "test_that: test 2"))
})

test_that("unrowname correctly processes the data frame", {
  # Mock data
  df1 <- data.frame(A = 1:3, B = 4:6, row.names = c("a", "b", "c"))
  ret <- list(file1 = df1)
  
  # Call the function
  result <- unrowname("file1", ret, "source")
  
  # Expectations
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("source", "A", "B"))
  expect_equal(result$source, rep("file1", 3))
})


test_that("map_tests_stf processes multiple test files correctly", {
  # Mock file paths
  mock_files <- c(
    "C:/Users/AppData/Local/Temp/test.package.0014/tests/testthat/test-myscript1.R",
    "C:/Users/AppData/Local/Temp/test.package.0014/tests/testthat/test-myscript2.R",
    "C:/Users/AppData/Local/Temp/test.package.0014/tests/testthat/test-myscript3.R"
  )
  
  # Mock return values from map_test
  mock_df1 <- data.frame(
    description = c("test_that: two working tests", "test_that: two working tests", "test_that: three working tests", "test_that: three working tests", "test_that: three working tests"),
    test = rep("expect_equal", 5),
    expectation = rep("expect_equal", 5),
    line1 = c(2, 4, 8, 10, 12),
    line2 = c(2, 4, 8, 10, 12),
    stringsAsFactors = FALSE
  )
  
  mock_df2 <- data.frame(
    description = c("test_that: test 1 works test 2 fails", "test_that: test 1 works test 2 fails",
                    "test_that: test 1 fails, tests 2 & 4  work, test 3 fails",
                    "test_that: test 1 fails, tests 2 & 4  work, test 3 fails",
                    "test_that: test 1 fails, tests 2 & 4  work, test 3 fails",
                    "test_that: test 1 fails, tests 2 & 4  work, test 3 fails"),
    test = rep("expect_equal", 6),
    expectation = rep("expect_equal", 6),
    line1 = c(2, 4, 8, 10, 12, 14),
    line2 = c(2, 4, 8, 10, 12, 14),
    stringsAsFactors = FALSE
  )
  
  mock_df3 <- data.frame(
    description = c("test_that: tests work", "test_that: tests work", "test_that: tests work",
                    "test_that: tests work_1", "test_that: tests work_1"),
    test = rep("expect_equal", 5),
    expectation = rep("expect_equal", 5),
    line1 = c(2, 4, 6, 10, 12),
    line2 = c(2, 4, 6, 10, 12),
    stringsAsFactors = FALSE
  )
  
  # Stub dependencies
  mockery::stub(map_tests_stf, "checkmate::assert_string", function(x) TRUE)
  mockery::stub(map_tests_stf, "checkmate::assert_directory_exists", function(x) TRUE)
  mockery::stub(map_tests_stf, "list.files", function(path, full.names, pattern) mock_files)
  mockery::stub(map_tests_stf, "map_test", function(path) {
    if (grepl("myscript1", path)) return(mock_df1)
    if (grepl("myscript2", path)) return(mock_df2)
    if (grepl("myscript3", path)) return(mock_df3)
    return(NULL)
  })
  
  # Run the function
  result <- map_tests_stf("tests/testthat")
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 5)
  expect_equal(unique(result$file), basename(mock_files))
  expect_true(all(c("test", "expectation", "line1", "line2", "file") %in% colnames(result)))
})

test_that("map_tests_stf processes test files correctly", {
  # Step 1: Mock list.files to simulate test files
  mockery::stub(map_tests_stf, "list.files", function(path, full.names, pattern) {
    return(c("tests/testthat/test-one.R", "tests/testthat/test-two.R"))
  })
  
  # Step 2: Mock checkmate assertions
  mockery::stub(map_tests_stf, "checkmate::assert_string", function(x) TRUE)
  mockery::stub(map_tests_stf, "checkmate::assert_directory_exists", function(x) TRUE)
  
  # Step 3: Mock map_test to return named data.frames
  mock_df1 <- data.frame(description = "desc1", test = " ", stringsAsFactors = FALSE)
  mock_df2 <- data.frame(description = "desc2", test = "it: does something", stringsAsFactors = FALSE)
  
  mockery::stub(map_tests_stf, "map_test", function(path) {
    if (grepl("test-one", path)) return(mock_df1)
    if (grepl("test-two", path)) return(mock_df2)
    return(NULL)
  })
  
  # Step 4: Run the function
  result <- map_tests_stf("tests/testthat")
  
  # Step 5: Assertions
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$file, c("test-one.R", "test-two.R"))
  expect_equal(result$test, c("desc1", "desc2: does something"))
})


#Sample mocked parse data for a test_that block
mock_parse_data_test_that <- data.frame(
  token = c("SYMBOL_FUNCTION_CALL", "expr", "expr"),
  text = c("test_that", "expect_true(TRUE)", "expect_equal(1, 1)"),
  stringsAsFactors = FALSE
)

# Sample mocked parse data for a describe/it block
mock_parse_data_describe <- data.frame(
  token = c("SYMBOL_FUNCTION_CALL", "expr", "SYMBOL_FUNCTION_CALL", "expr"),
  text = c("describe", "context", "it", "expect_true(TRUE)"),
  stringsAsFactors = FALSE
)

test_that("map_test returns expected structure for test_that block", {
  mockery::stub(map_test, "parse", function(...) NULL)  # parse is not used directly
  mockery::stub(map_test, "utils::getParseData", function(...) mock_parse_data_test_that)
  mockery::stub(map_test, "nest_test", function(x, token_text) setNames(list(x), "test_that"))
  mockery::stub(map_test, "nest_expect", function(x) setNames(list(x[[1]]), "test_that"))
  mockery::stub(map_test, "unrowname", function(name, ret, label) {
    df <- ret[[name]]
    df$description <- name
    as.data.frame(df, stringsAsFactors = FALSE)
  })
  
  
  result <- map_test("dummy_path.R")
  expect_true(is.data.frame(result))
  expect_equal(result$description[1], "test_that")
  expect_equal(result$description[2], "test_that")
  expect_equal(result$description[3], "test_that")
})


test_that("map_test handles describe blocks", {
  # Create mock parse data
  mock_parse_data <- data.frame(token = "SYMBOL_FUNCTION_CALL", text = "describe", terminal = TRUE, stringsAsFactors = FALSE)
  
  # Stub getParseData to return mock data
  mockery::stub(map_test, "utils::getParseData", mock_parse_data)
  
  # Stub nest_test to simulate describe block
  mockery::stub(map_test, "nest_test", function(x, token_text) {
    if (token_text == "^test_that$|^describe$|^tar_test$") {
      list(describe_block = mock_parse_data)
    } else if (token_text == "^it$") {
      list(it_block = mock_parse_data)
    } else {
      list()
    }
  })
  
  # Stub nest_expect to return a mock expectation
  mockery::stub(map_test, "nest_expect", function(x) {
    data.frame(expectation = "expect_equal", line1 = 1, line2 = 2, test = "it: does something", stringsAsFactors = FALSE)
  })
  
  result <- map_test("fake_path.R")
  
  expect_s3_class(result, "data.frame")
  expect_true("expectation" %in% names(result))
  expect_true(any(grepl("describe", result$description)))
})


test_that("map_test handles tar_test blocks", {
  mock_parse_data <- data.frame(token = "SYMBOL_FUNCTION_CALL", text = "tar_test", terminal = TRUE, stringsAsFactors = FALSE)
  
  mockery::stub(map_test, "utils::getParseData", mock_parse_data)
  
  mockery::stub(map_test, "nest_test", function(x, token_text) {
    list(tar_test_block = mock_parse_data)
  })
  
  mockery::stub(map_test, "nest_expect", function(x) {
    data.frame(expectation = "expect_true", line1 = 3, line2 = 4, test = " ", stringsAsFactors = FALSE)
  })
  
  result <- map_test("fake_path.R")
  
  expect_s3_class(result, "data.frame")
  expect_true("expectation" %in% names(result))
  expect_true(any(grepl("tar_test", result$description)))
})



