#' @title Map the hierarchy structure of tests directory in standard testing framework
#' @description Query a test directory for the unit test structure.
#' @param path character, path to tests, Default: 'tests/testthat'
#' @details Return data.frame containing which expectations are in the testthat directory
#' by file/context/description/test/expectation/linerange
#' @return data.frame
#' @seealso
#' \code{\link[base]{names}}, \code{\link[base]{lapply}}, \code{\link[base]{rbind}}
#' @rdname map_testthat
#' @family utility
#' @export
map_tests_stf <- function(path = "tests/testthat") {
  
  # Input checking
  checkmate::assert_string(path)
  checkmate::assert_directory_exists(path)
  
  # Step 1: List all test files in the specified directory that match the pattern "test*.R"
  files <- list.files(path, full.names = TRUE, pattern = "^test(.*?)R$")
  
  # Step 2: Apply the map_test function to each file and store results in a named list
  results_list <- lapply(files, map_test)
  names(results_list) <- basename(files) # Assign file names as list names
  
  # Step 3: Remove entries with no content (length == 0)
  results_list <- results_list[sapply(results_list, length) > 0]
  
  # Step 4: If no valid test files were found, return NULL
  if (length(results_list) == 0) {
    return(NULL)
  }
  
  # Step 5: Add a 'file' column to each data.frame in the list using the file name
  results_list <- lapply(names(results_list), function(name) {
    df <- results_list[[name]]
    df$file <- name
    df
  })
  
  # Step 6: Combine all individual data.frames into one consolidated data.frame
  results <- do.call("rbind", results_list)
  
  # Step 7: Identify rows where the 'test' column is not just a space
  idx <- !(results$test == " ")
  
  # Step 8: Clean up the 'description' and 'test' columns by removing prefixes
  results$description <- gsub("describe: |test_that: |tar_test: ", "", results$description)
  results$test <- gsub("it: ", "", results$test)
  
  # Step 9: For rows without a specific test, use the description as the test name
  results$test[!idx] <- results$description[!idx]
  
  # Step 10: For rows with both description and test, combine them into a single string
  results$test[idx] <- sprintf("%s: %s", results$description[idx], results$test[idx])
  
  # Step 11: Remove the 'description' column as it's now merged into 'test'
  results$description <- NULL
  
  # Step 12: Return the final structured data.frame
  return(results)
}


#' @title Nest parsed testthat code into logical test blocks
#' @description Internal helper function to group parsed testthat code into blocks based on context, test_that, describe, or it calls.
#' @param x data.frame, parse data from utils::getParseData
#' @param token_text character, regex pattern to match test block function names, Default: '^context$'
#' @return named list of data.frames, each representing a nested test block
#' @keywords internal
#' @importFrom utils head tail
nest_test <- function(x, token_text = "^context$") {
 
  # Store row names of the parse data
  rx <- rownames(x)
  
  # Find indices of parent expressions that match the specified token_text (e.g., context)
  idx <- which(rx %in% x$parent[grepl("^SYMBOL_FUNCTION_CALL$", x$token) & grepl(token_text, x$text) & x$terminal])
  
  # Refine indices to get top-level expressions for those matches
  idx <- which(rx %in% x$parent[idx])
  
  # Initialize a grouping vector
  x1 <- rep(0, nrow(x))
  
  # Mark the start of each new block
  x1[idx] <- 1
  
  # Create a cumulative sum to assign group IDs
  x1 <- cumsum(x1)
  
  # Split the parse data into groups based on the group IDs
  x2 <- split(x, x1)
  
  # Assign names to each group based on the function call and its string argument
  names(x2) <- sapply(x2, function(x) {
    # Get the function name (e.g., test_that, describe, it)
    add_text <- x$text[grep("^SYMBOL_FUNCTION_CALL$", x$token)[1]]
    
    # Evaluate the string constant (e.g., test description)
    results <- eval(parse(text = x$text[grepl("^STR_CONST$", x$token)][1], keep.source = TRUE))
    
    # Prefix the name with the function name if it's a known test block
    if (add_text %in% c("test_that", "describe", "it", "tar_test")) {
      results <- sprintf("%s: %s", add_text, results)
    }
    
    results
  })
  
  # Trim each group to remove leading parse noise and keep only the relevant block
  x2 <- lapply(x2, function(x) {
    # Find the second top-level expression (if available)
    idx <- utils::tail(utils::head(which(x$parent == 0), 2), 1)
    if (length(idx) == 0) {
      idx <- 1
    }
    # Return the block from the identified index to the end
    x[idx:nrow(x), ]
  })
  
  x2
}


#' @title Extract expectation calls from parsed test code
#' @description Internal helper function to identify and extract expectation function calls (e.g., expect_equal) from parsed testthat code.
#' @param parsed_data data.frame, parse data from utils::getParseData
#' @param token_text character, regex pattern to match expectation function names, Default: '^expect_'
#' @return data.frame with expectation names and their line ranges
#' @keywords internal
get_expect <- function(parsed_data, token_text = "^expect_") {
  
  # Identify indices of function calls that match the expectation pattern
  idx <- which(grepl("^SYMBOL_FUNCTION_CALL$", parsed_data$token) & grepl(token_text, parsed_data$text) & parsed_data$terminal)
  
  # Extract the matched expectation function names
  expect_data <- parsed_data$text[idx]
  
  if (length(expect_data) == 0) {
    return(NULL)
  }
  
  # For each matched expectation, find the corresponding expression's line range
  line_ <- lapply(idx, function(y) {
    # Find the last two 'expr' tokens before the expectation
    this_idx <- tail(grep("expr", parsed_data$token[1:y]), 2)
    # Determine which of those expressions contains the expectation
    this_exp <- min(grep(sprintf("%s", parsed_data$text[y]), parsed_data$text[this_idx]))
    # Extract the line range for that expression
    parsed_data[this_idx[this_exp], c("line1", "line2")]
  })
  
  # Combine all line ranges into a single data.frame
  line_ <- do.call("rbind", line_)
  
  # Construct the final data.frame with expectation names and line numbers
  expect_data_df <- data.frame(
    expectation = expect_data,
    line1 = line_$line1,
    line2 = line_$line2,
    stringsAsFactors = FALSE
  )
  
  expect_data_df
}

#' @title Extract and structure expectations from test blocks
#' @description Internal helper function to extract expectations from a list of test blocks.
#' @param x list of parsed test blocks
#' @return data.frame containing structured expectations with test labels
#' @keywords internal
nest_expect <- function(x) {
  
  # Apply get_expect to each test block to extract expectations
  ret <- lapply(x, get_expect)
  
  # Remove any NULL results (i.e., blocks with no expectations)
  ret <- ret[!sapply(ret, is.null)]
  
  if (length(ret) == 0) {
    return(NULL)
  }
  
  # Add a 'test' column to each expectation block using the list names
  ret <- lapply(names(ret), unrowname, ret = ret, label = "test")
  
  # Combine all expectation blocks into a single data.frame
  ret <- do.call("rbind", ret)
  
  ret
}

#' @title Map the hierarchy structure of a testthat test file
#' @description Query a test file to see what expectations are defined in it.
#' @param path character, path to file
#' @details Return data.frame containing which expectations are in the test file
#'  by context/description/test/expectation/linerange
#' @return data.frame
#' @seealso
#'  \code{\link[utils]{getParseData}}
#' @rdname map_test
#' @family utility
#' @export
#' @importFrom utils getParseData
#' @importFrom stats setNames
map_test <- function(path) {
  
  # Parse the test file and extract parse data including source text
  parse_data <- utils::getParseData(parse(path, keep.source = TRUE), includeText = TRUE)
  if (is.null(parse_data)) return(NULL)
  
  # Treat test_that and describe as top-level test blocks
  ret <- lapply(nest_test(parse_data, token_text = "^test_that$|^describe$|^tar_test$"), function(xx) {
    # Identify the function name (e.g., test_that or describe)
    SYMB <- xx$text[grep("^SYMBOL_FUNCTION_CALL$", xx$token)[1]]
    
    # Depending on the function, process expectations differently
    switch(SYMB,
           # Nest 'it' blocks inside describe
           describe = {
             nest_expect(nest_test(xx, token_text = "^it$"))
           },
           test_that = {
             # Wrap test_that block in a list with a placeholder name
             nest_expect(setNames(list(xx), " "))
           },
           tar_test = {
             # Wrap test_that block in a list with a placeholder name
             nest_expect(setNames(list(xx), " "))
           }, {
             list()
           }
    )
  })
  
  # Remove empty elements
  ret <- ret[sapply(ret, length) > 0]
  if (length(ret) == 0) return(NULL)
  
  
  # Ensure unique names for test blocks
  names(ret) <- make.unique(names(ret), sep = "_")
  
  # Add 'description' column to each test block
  ret <- lapply(names(ret), unrowname, ret = ret, label = "description")
  
  # Combine all test blocks into a single data.frame
  ret <- do.call("rbind", ret)
  return(ret)
}

#' @title Add identifier column and remove row names from a data.frame
#' @description Internal helper function to restructure a data.frame by adding an identifier column and removing row names.
#' @param identifier character, the name or label to assign to the new column
#' @param ret list of data.frames, typically the result of applying a function over multiple files
#' @param label character, name of the new column to store the identifier
#' @return data.frame with identifier column prepended and row names removed
#' @keywords internal
unrowname <- function(identifier, ret, label) {
  # Extract the data.frame corresponding to the identifier
  results <- ret[[identifier]]
  
  # Get the number of columns in the original data.frame
  nc <- ncol(results)
  
  # Add a new column with the identifier value (e.g., file name)
  results[[label]] <- identifier
  
  
  # Remove row names to ensure clean output
  rownames(results) <- NULL
  
  # Reorder columns to place the identifier column first
  results <- results[, c(c(nc + 1), 1:nc)]
  
  return(results)
}


