#' Map the Structure of Testthat Tests (Standard Testing Framework)
#'
#' This function analyses the test directory used by the Standard Testing
#' Framework (STF), typically `tests/testthat/`, and extracts a structured
#' representation of the unit tests contained in the directory. It identifies
#' test files, expectations, contexts, test names, and associated line ranges.
#'
#' @param path Character. Path to the test directory. Defaults to
#'   `"tests/testthat"`.
#'
#' @details
#' The function scans all testthat test files matching the pattern
#' `test*.R`. For each file, it extracts structured metadata describing the
#' hierarchy of expectations, including:
#' \itemize{
#'   \item file name
#'   \item context or description
#'   \item test label
#'   \item expectation type
#'   \item line ranges associated with each expectation
#' }
#'
#' Files that contain no parseable test structure are omitted. If no valid
#' test files are found, the function returns `NULL`.
#'
#' @return 
#' A consolidated `data.frame` where each row corresponds to an expectation
#' discovered within the STF test directory. The data frame typically contains:
#' \describe{
#'   \item{file}{Name of the test file in which the expectation appears.}
#'   \item{test}{The combined description and test label assigned to the
#'         expectation.}
#'   \item{expectation}{The expectation type detected (e.g., `expect_equal`).}
#'   \item{linerange}{The associated line numbers for that expectation.}
#' }
#'
#' This structured mapping is used by downstream STF processing functions to
#' support skip mapping, coverage enhancement, and reporting.
#'
#' @seealso
#'   \code{\link[base]{list.files}},
#'   \code{\link[base]{lapply}},
#'   \code{\link[base]{rbind}}
#'
#' @family stf_utility
#' @rdname map_tests_stf
#' @keywords internal
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



#' Group Parsed Testthat Code into Logical Test Blocks (Internal)
#'
#' This internal helper groups parse data from a testthat test file into
#' logical test blocks based on calls to block‑defining functions such as
#' `context()`, `test_that()`, `describe()`, `it()`, or `tar_test()`. It
#' identifies top‑level expressions that match a given function‑name regex
#' and returns a named list of data frames—one per detected block—trimmed
#' to the relevant code region.
#'
#' @param x data.frame. Parse data produced by
#'   \code{utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)}.
#'   Must include columns such as \code{token}, \code{text}, \code{parent},
#'   and row names that correspond to node ids.
#' @param token_text Character. A regular expression used to select the
#'   block‑starting function names (e.g., \code{"^context$"}, 
#'   \code{"^test_that$|^describe$|^tar_test$"}, or \code{"^it$"}).
#'   Defaults to \code{"^context$"}.
#'
#' @details
#' The function locates top‑level expressions whose function call tokens
#' (\code{SYMBOL_FUNCTION_CALL}) match \code{token_text}. For each matching
#' block, it:
#' \itemize{
#'   \item assigns a human‑readable name using the function name and its
#'         string constant argument (when available),
#'   \item trims the block to exclude leading parse noise, and
#'   \item returns the subset of \code{x} corresponding to that block.
#' }
#' Block names are prefixed with the function (e.g., \code{"test_that: ..."},
#' \code{"describe: ..."}, \code{"it: ..."}, \code{"tar_test: ..."}) when the
#' function is recognized.
#'
#' @return
#' A named list of data frames, where each element represents the parse rows
#' for a single detected test block. Names encode the block function and label.
#' If no matching blocks are found, an empty list is returned. This structure
#' is consumed by higher‑level STF mapping helpers (e.g., \code{map_test()},
#' \code{map_tests_stf()}) to derive expectations, contexts, and line ranges.
#'
#' @seealso
#'   \code{\link[utils]{getParseData}},
#'   \code{\link{map_test}},
#'   \code{\link{map_tests_stf}}
#'
#' @family stf_utility
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



#' Extract Expectation Calls from Parsed Testthat Code
#'
#' This internal helper identifies expectation calls (e.g.,
#' `expect_equal()`, `expect_true()`, etc.) within parse data extracted from a
#' testthat test file. The function locates expectation function calls by
#' matching their token text and returns a structured data frame containing
#' the expectation names and their corresponding source line ranges.
#'
#' @param parsed_data data.frame. Parse data produced by
#'   \code{utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)}.
#'   Must include the columns \code{token}, \code{text}, \code{line1},
#'   and \code{line2}.
#' @param token_text Character. A regular expression used to detect
#'   expectation‑style function calls. Defaults to \code{"^expect_"}.
#'
#' @details
#' The function:
#' \itemize{
#'   \item locates all rows where the token is \code{"SYMBOL_FUNCTION_CALL"}
#'         and the text matches \code{token_text},
#'   \item identifies the expression node corresponding to each expectation,
#'   \item extracts line ranges (\code{line1}, \code{line2}) describing where the
#'         expectation appears in the source file, and
#'   \item returns a consolidated data frame of results.
#' }
#'
#' If no expectation calls are found, the function returns \code{NULL}.
#' This helper is used internally by STF mapping functions (such as
#' \code{map_test()} and \code{map_tests_stf()}) when constructing a full
#' hierarchy of test blocks and expectations.
#'
#' @return
#' A \code{data.frame} where each row corresponds to a detected expectation.
#' The data frame contains:
#' \describe{
#'   \item{expectation}{Character. Name of the expectation function (e.g.,
#'         \code{"expect_equal"}).}
#'   \item{line1}{Integer. Starting line number of the expression.}
#'   \item{line2}{Integer. Ending line number of the expression.}
#' }
#' Returns \code{NULL} if no matching expectations are found.
#'
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


#' Extract and Structure Expectations from Parsed Test Blocks
#'
#' This internal helper takes a list of parsed test blocks (as produced by
#' \code{nest_test()}) and extracts expectation calls from each block using
#' \code{get_expect()}. It then assembles all detected expectations into a
#' structured data frame that associates each expectation with its
#' corresponding test label.
#'
#' @param x A named list of parsed test blocks, typically produced by
#'   \code{nest_test()}. Each element should contain parse data suitable for
#'   expectation extraction via \code{get_expect()}.
#'
#' @details
#' For each block in \code{x}, the function:
#' \itemize{
#'   \item calls \code{get_expect()} to extract expectation function calls
#'         and their line ranges,
#'   \item removes blocks with no expectations,
#'   \item attaches a \code{test} label using the block's list name, and
#'   \item binds all expectation results into a single consolidated data frame.
#' }
#'
#' If no expectations are found in any block, the function returns \code{NULL}.
#'
#' @return
#' A \code{data.frame} in which each row corresponds to an expectation
#' extracted from the supplied test blocks. Typical columns include:
#' \describe{
#'   \item{expectation}{Name of the expectation function (e.g.,
#'         \code{"expect_equal"}).}
#'   \item{line1}{Starting line number of the expectation expression.}
#'   \item{line2}{Ending line number of the expectation expression.}
#'   \item{test}{Label identifying the test block the expectation belongs to.}
#' }
#' Returns \code{NULL} if no expectations are detected.
#'
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


#' Map the Structure of Expectations in a Testthat File
#'
#' This function parses a single testthat test file and extracts a structured
#' representation of the expectations it contains. It identifies top‑level test
#' blocks (e.g., `test_that()`, `describe()`, `tar_test()`) and any nested
#' expectation blocks (e.g., `it()`). The result maps expectation location,
#' context, and source line ranges for downstream analysis.
#'
#' @param path Character. Path to a single test file (typically named
#'   `test-*.R`).
#'
#' @details
#' The function uses `utils::getParseData()` to inspect parse information from
#' the test file and constructs a hierarchical mapping of:
#' \itemize{
#'   \item test block context (e.g., `describe()`, `test_that()`)
#'   \item test labels or sub‑contexts (e.g., `it()` blocks)
#'   \item expectation type
#'   \item source line ranges associated with each expectation
#' }
#'
#' If the file does not contain any parseable test blocks or expectations,
#' `NULL` is returned.
#'
#' @return 
#' A `data.frame` in which each row corresponds to an expectation identified in
#' the test file. Typical columns include:
#' \describe{
#'   \item{description}{Context or test block label derived from the test file.}
#'   \item{expectation}{The expectation type (e.g., `expect_equal`).}
#'   \item{linerange}{Numeric vector giving the range of lines associated with
#'         the expectation.}
#'   \item{test}{Test label (combined description and nested block label, if
#'         applicable).}
#' }
#'
#' This structured output is used internally by STF test‑mapping functions
#' (e.g., `map_tests_stf()`) for coverage analysis, skip mapping, and test
#' reporting.
#'
#' @seealso
#'   \code{\link[utils]{getParseData}}
#'
#' @family stf_utility
#' @rdname map_test
#' @keywords internal
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


#' Add Identifier Column and Remove Row Names from a Data Frame
#'
#' This internal helper restructures a data frame by adding an identifier
#' column (e.g., file name or test label) and removing row names. It is used
#' by STF parsing functions to attach contextual labels (such as test block
#' names or file names) to expectation or test‑block structures.
#'
#' @param identifier Character. The identifier to insert into the resulting
#'   data frame (typically the name of the list element being processed).
#' @param ret List of data frames, usually the output of applying a mapping
#'   or extraction function across multiple parsed test blocks.
#' @param label Character. Name of the column used to store the identifier
#'   (e.g., \code{"test"} or \code{"file"}).
#'
#' @details
#' The function:
#' \itemize{
#'   \item extracts the data frame corresponding to \code{identifier},
#'   \item appends a new column containing the identifier value,
#'   \item removes row names for clean downstream processing, and
#'   \item reorders columns so the identifier appears first.
#' }
#'
#' This structure is used by STF mapping helpers such as
#' \code{nest_expect()} and \code{map_tests_stf()} to carry forward file or
#' test labels when constructing unified test‑mapping tables.
#'
#' @return
#' A \code{data.frame} with an identifier column prepended and row names
#' removed. The remaining columns retain their original structure.
#'
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


