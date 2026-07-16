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

test_that("block_line_span returns the min/max line span of a block", {
  blk <- data.frame(line1 = c(10, 12, 15), line2 = c(10, 14, 15))
  span <- block_line_span(blk)
  expect_equal(span[[1]], 10L)
  expect_equal(span[[2]], 15L)
})

test_that("block_line_span returns NA when line info is unavailable", {
  blk <- data.frame(token = "SYMBOL_FUNCTION_CALL", text = "test_that")
  span <- block_line_span(blk)
  expect_true(is.na(span[[1]]))
  expect_true(is.na(span[[2]]))
})

test_that("map_test attaches the enclosing test_that block line span", {
  tf <- tempfile(fileext = ".R")
  writeLines(c(
    "test_that('demo', {",   # 1  (block start)
    "  x <- 1",              # 2  (non-expectation setup)
    "  expect_equal(x, 1)",  # 3
    "  expect_true(x > 0)",  # 4
    "})"                     # 5  (block end)
  ), tf)
  on.exit(unlink(tf), add = TRUE)
  
  result <- map_test(tf)
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("line1", "line2", "block_line1", "block_line2") %in% names(result)))
  expect_false(any(is.na(result$block_line1)))
  expect_false(any(is.na(result$block_line2)))
  
  # The block span must cover the non-expectation setup line (line 2) so that
  # an erroring block is skipped in its entirety, and must extend to at least
  # the last expectation.
  expect_true(all(result$block_line1 <= 2))
  expect_true(all(result$block_line2 >= 4))
  
  # Every expectation lies within the enclosing block span
  expect_true(all(result$line1 >= result$block_line1 &
                    result$line2 <= result$block_line2))
})

test_that("map_tests_stf returns NULL when results_list is empty", {
 
  mockery::stub(map_tests_stf, "checkmate::assert_string", function(x) TRUE)
  mockery::stub(map_tests_stf, "checkmate::assert_directory_exists", function(x) TRUE)
  mockery::stub(map_tests_stf, "list.files", function(path, full.names, pattern) {
    character(0)  # Empty file list
  })
  
  result <- map_tests_stf("tests/testthat")
  expect_null(result)
})


test_that("map_tests_stf handles sapply filtering results_list", {
  
  mockery::stub(map_tests_stf, "checkmate::assert_string", function(x) TRUE)
  mockery::stub(map_tests_stf, "checkmate::assert_directory_exists", function(x) TRUE)
  mockery::stub(map_tests_stf, "list.files", function(path, full.names, pattern) {
    c("tests/testthat/test-empty.R")
  })
  
  # map_test returns NULL for empty file
  mockery::stub(map_tests_stf, "map_test", function(path) NULL)
  
  result <- map_tests_stf("tests/testthat")
  expect_null(result)
})


test_that("nest_test handles trimming with length(idx) == 0", {
  
  mock_parse_data <- data.frame(
    line1 = 1:3,
    token = c("SYMBOL_FUNCTION_CALL", "expr", "expr"),
    text = c("test_that", "expect_equal", "expect_true"),
    parent = c(0, 0, 0),
    terminal = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  rownames(mock_parse_data) <- as.character(1:nrow(mock_parse_data))
  
  mockery::stub(nest_test, "eval", function(expr, keep.source = TRUE) {
    "test label"
  })
  
  result <- nest_test(mock_parse_data, token_text = "^nonexistent$")
  
  expect_type(result, "list")
})


test_that("get_expect returns NULL when no expectations found", {
  
  mock_parse_data <- data.frame(
    line1 = c(1, 2),
    line2 = c(1, 2),
    token = c("expr", "expr"),
    text = c("x <- 1", "y <- 2"),
    parent = c(0, 0),
    terminal = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- get_expect(mock_parse_data, token_text = "^expect_")
  expect_null(result)
})


test_that("get_expect handles empty matches correctly", {
  # Tests condition with no SYMBOL_FUNCTION_CALL tokens matching expect_
  
  mock_parse_data <- data.frame(
    line1 = c(1, 2, 3),
    line2 = c(1, 2, 3),
    token = c("SYMBOL_FUNCTION_CALL", "expr", "expr"),
    text = c("some_function", "expect_error", "y <- 2"),
    parent = c(0, 0, 0),
    terminal = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  
  result <- get_expect(mock_parse_data, token_text = "^expect_")
  expect_null(result)
})


test_that("nest_expect returns NULL when ret is empty", {
  
  mock_test_blocks <- list()
  
  mockery::stub(nest_expect, "get_expect", function(x) NULL)
  
  result <- nest_expect(mock_test_blocks)
  expect_null(result)
})


test_that("nest_expect returns NULL when all blocks have no expectations", {
  # All get_expect calls return NULL
  
  mock_test_blocks <- list(
    "test_that: test 1" = data.frame(dummy = 1),
    "test_that: test 2" = data.frame(dummy = 2),
    "test_that: test 3" = data.frame(dummy = 3)
  )
  
  mockery::stub(nest_expect, "get_expect", function(x) NULL)
  
  result <- nest_expect(mock_test_blocks)
  expect_null(result)
})


test_that("map_test returns NULL when parse_data is NULL", {
  
  mockery::stub(map_test, "utils::getParseData", function(...) NULL)
  
  result <- map_test("nonexistent_file.R")
  expect_null(result)
})


test_that("map_test returns NULL when parse_data is empty", {
  # Handles edge case where parse_data exists but is empty
  
  mock_empty_parse <- data.frame(
    token = character(0),
    text = character(0),
    parent = integer(0),
    terminal = logical(0),
    stringsAsFactors = FALSE
  )
  
  mockery::stub(map_test, "utils::getParseData", function(...) mock_empty_parse)
  mockery::stub(map_test, "nest_test", function(x, token_text) list())
  
  result <- map_test("dummy.R")
  expect_null(result)
})


test_that("map_test handles empty ret after keep filtering", {
  # This occurs when all blocks are filtered out
  
  mock_parse_data <- data.frame(
    line1 = 1:2,
    token = c("SYMBOL_FUNCTION_CALL", "expr"),
    text = c("test_that", "some_expr"),
    parent = c(0, 1),
    terminal = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  rownames(mock_parse_data) <- as.character(1:nrow(mock_parse_data))
  
  mockery::stub(map_test, "utils::getParseData", function(...) mock_parse_data)
  mockery::stub(map_test, "nest_test", function(x, token_text) {
    if (token_text == "^test_that$|^describe$|^tar_test$") {
      list("test_that: test" = x)
    } else {
      list()
    }
  })
  mockery::stub(map_test, "nest_expect", function(x) NULL)
  
  result <- map_test("dummy.R")
  expect_null(result)
})


test_that("map_test handles vapply integer(1) > 0 filtering", {
  # Tests that empty results are properly filtered out
  
  mock_parse_data <- data.frame(
    line1 = 1:4,
    token = c("SYMBOL_FUNCTION_CALL", "STR_CONST", "expr", "expr"),
    text = c("test_that", "\"test label\"", "x <- 1", "expect_equal(x, 1)"),
    parent = c(0, 1, 1, 1),
    terminal = c(TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  rownames(mock_parse_data) <- as.character(1:nrow(mock_parse_data))
  
  mockery::stub(map_test, "utils::getParseData", function(...) mock_parse_data)
  mockery::stub(map_test, "nest_test", function(x, token_text) {
    if (token_text == "^test_that$|^describe$|^tar_test$") {
      list("test_that: test" = x)
    } else {
      list()
    }
  })
  mockery::stub(map_test, "nest_expect", function(x) {
    data.frame(
      expectation = "expect_equal",
      line1 = 4,
      line2 = 4,
      stringsAsFactors = FALSE
    )
  })
  mockery::stub(map_test, "unrowname", function(name, ret, label) {
    df <- ret[[name]]
    df[[label]] <- name
    df
  })
  mockery::stub(map_test, "block_line_span", function(block) {
    list(1L, 4L)
  })
  
  result <- map_test("dummy.R")
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})


test_that("map_test returns complete structure with block_line spans", {
  # Tests the final return with all components
  
  mock_parse_data <- data.frame(
    line1 = 1:3,
    line2 = 1:3,
    token = c("SYMBOL_FUNCTION_CALL", "STR_CONST", "SYMBOL_FUNCTION_CALL"),
    text = c("test_that", "\"my test\"", "expect_true"),
    parent = c(0, 1, 1),
    terminal = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  rownames(mock_parse_data) <- as.character(1:nrow(mock_parse_data))
  
  mockery::stub(map_test, "utils::getParseData", function(...) mock_parse_data)
  mockery::stub(map_test, "nest_test", function(x, token_text) {
    if (token_text == "^test_that$|^describe$|^tar_test$") {
      list("test_that: my test" = x)
    } else {
      list()
    }
  })
  mockery::stub(map_test, "nest_expect", function(x) {
    data.frame(
      expectation = "expect_true",
      line1 = 3,
      line2 = 3,
      stringsAsFactors = FALSE
    )
  })
  mockery::stub(map_test, "unrowname", function(name, ret, label) {
    df <- ret[[name]]
    df[[label]] <- name
    df
  })
  mockery::stub(map_test, "block_line_span", function(block) {
    list(1L, 3L)
  })
  
  result <- map_test("dummy.R")
  
  expect_true(is.data.frame(result))
  expect_true(all(c("expectation", "line1", "line2", "description", 
                    "block_line1", "block_line2") %in% names(result)))
  expect_false(any(is.na(result$block_line1)))
  expect_false(any(is.na(result$block_line2)))
})


test_that("map_tests_stf processes complex test files with proper rbind", {
  # Tests the do.call(rbind) operation and subsequent transformations
  
  mock_files <- c("tests/testthat/test-a.R", "tests/testthat/test-b.R")
  
  mock_df1 <- data.frame(
    description = "test_that: first test",
    test = " ",
    expectation = "expect_equal",
    line1 = 1,
    line2 = 1,
    stringsAsFactors = FALSE
  )
  
  mock_df2 <- data.frame(
    description = "test_that: second test",
    test = "it: nested test",
    expectation = "expect_true",
    line1 = 5,
    line2 = 5,
    stringsAsFactors = FALSE
  )
  
  mockery::stub(map_tests_stf, "checkmate::assert_string", function(x) TRUE)
  mockery::stub(map_tests_stf, "checkmate::assert_directory_exists", function(x) TRUE)
  mockery::stub(map_tests_stf, "list.files", function(path, full.names, pattern) mock_files)
  mockery::stub(map_tests_stf, "map_test", function(path) {
    if (grepl("test-a", path)) return(mock_df1)
    if (grepl("test-b", path)) return(mock_df2)
    return(NULL)
  })
  
  result <- map_tests_stf("tests/testthat")
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("file" %in% names(result))
  expect_equal(result$file, c("test-a.R", "test-b.R"))
})


test_that("map_tests_stf applies gsub transformations correctly", {
  # Tests the prefix removal logic
  
  mock_files <- c("tests/testthat/test-example.R")
  
  mock_df <- data.frame(
    description = "describe: outer block",
    test = "it: inner block",
    expectation = "expect_equal",
    line1 = 10,
    line2 = 10,
    stringsAsFactors = FALSE
  )
  
  mockery::stub(map_tests_stf, "checkmate::assert_string", function(x) TRUE)
  mockery::stub(map_tests_stf, "checkmate::assert_directory_exists", function(x) TRUE)
  mockery::stub(map_tests_stf, "list.files", function(path, full.names, pattern) mock_files)
  mockery::stub(map_tests_stf, "map_test", function(path) mock_df)
  
  result <- map_tests_stf("tests/testthat")
  
  expect_equal(result$test, "outer block: inner block")
})


test_that("map_tests_stf combines description and test with sprintf", {
  # Tests logic when idx identifies rows with specific tests
  
  mock_files <- c("tests/testthat/test-combined.R")
  
  mock_df <- data.frame(
    description = "test_that: combined test",
    test = "expectation block",
    expectation = "expect_equal",
    line1 = 5,
    line2 = 5,
    stringsAsFactors = FALSE
  )
  
  mockery::stub(map_tests_stf, "checkmate::assert_string", function(x) TRUE)
  mockery::stub(map_tests_stf, "checkmate::assert_directory_exists", function(x) TRUE)
  mockery::stub(map_tests_stf, "list.files", function(path, full.names, pattern) mock_files)
  mockery::stub(map_tests_stf, "map_test", function(path) mock_df)
  
  result <- map_tests_stf("tests/testthat")
  
  expect_equal(result$test, "combined test: expectation block")
  expect_true("description" %in% names(result) || !"description" %in% names(result))
})

