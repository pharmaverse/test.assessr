#' Safely generate skip test mapping from problem tests and test map
#'
#' This function filters and joins test metadata to identify which tests
#' should be skipped based on failed or problematic test results.
#'
#' @param problem_tests A data frame of problematic tests, including columns `file` and `context`.
#' @param test_map A data frame representing the test structure, including columns `file` and `context`.
#'
#' @return A data frame containing the merged skip test mapping, or `NULL` if validation or processing fails.
#'
#' @keywords internal
get_tests_skip_stf <- function(problem_tests, test_map) {
  
  # Validate 'problem_tests'
  if (!checkmate::test_data_frame(problem_tests, any.missing = TRUE, min.rows = 1)) {
    warning("'problem_tests' is not a valid data frame or has no rows.")
    return(NULL)
  }
  if (!checkmate::test_names(names(problem_tests), must.include = c("file", "context"))) {
    warning("'problem_tests' must include 'file' and 'context' columns.")
    return(NULL)
  }
  
  # Validate 'test_map'
  if (!checkmate::test_data_frame(test_map, any.missing = FALSE, min.rows = 1)) {
    warning("'test_map' is not a valid data frame or has no rows.")
    return(NULL)
  }
  if (!checkmate::test_names(names(test_map), must.include = c("file", "context"))) {
    warning("'test_map' must include 'file' and 'context' columns.")
    return(NULL)
  }
  
  # Filter test_map to only include rows matching both context and file in problem_tests
  test_map_filtered <- tryCatch({
    test_map[test_map$context %in% problem_tests$context &
               test_map$file %in% problem_tests$file, ]
  }, error = function(e) {
    warning("Error filtering test_map: ", e$message)
    return(NULL)
  })
  
  if (is.null(test_map_filtered) || nrow(test_map_filtered) == 0) {
    warning("Filtered test_map is empty or invalid.")
    return(NULL)
  }
  
  # Perform join
  test_skip <- safe_left_join(problem_tests, 
                              test_map_filtered, 
                              by = c("context", "file"))
  
  
  if (is.null(test_skip) || nrow(test_skip) == 0) {
    warning("Join result is empty or invalid.")
    return(NULL)
  }
  
  return(test_skip)
}


#' Safely perform a left join with error handling
#'
#' This helper function attempts to join two data frames using
#' `dplyr::left_join()`. If an error occurs during the join (for example,
#' due to missing columns or incompatible types), the function issues a
#' warning and returns `NULL` instead of stopping execution.
#'
#' @param x A data frame or tibble. The left-hand side of the join.
#' @param y A data frame or tibble. The right-hand side of the join.
#' @param by A character vector specifying the join keys. Passed to
#'   `dplyr::left_join()`.
#' @param relationship A string specifying the expected relationship between keys:
#'   `"one-to-one"`, `"one-to-many"`, `"many-to-one"`, or `"many-to-many"`.
#'   Defaults to `NULL` (no check).
#'
#' @return A data frame resulting from the left join if successful,
#'   otherwise `NULL` when an error occurs.
#'
#' @keywords internal
safe_left_join <- function(x, y, by, relationship = "many-to-many", ...) {
  tryCatch({
    dplyr::left_join(x, y, by = by, ...)
  }, error = function(e) {
    # Signal warning and return NULL
    warning("Error joining data frames: ", e$message, call. = FALSE)
    return(NULL)
  })
}

