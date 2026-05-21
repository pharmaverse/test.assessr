#' @title Summarize detailed testthat results
#' @description Internal helper function to generate a long-format summary of testthat results, including status and line references.
#' @param x list of test result objects
#' @return data.frame with detailed test results including status and source line links
#' @keywords internal

get_tests_long_sum_stf <- function(x) {
  if (length(x) == 0) return(NULL)
  
  test_rows <- list()
  
  for (i in seq_along(x)) {
    test_group <- x[[i]]
    results <- test_group$results
    for (j in seq_along(results)) {
      result <- results[[j]]
      
      # Determine flags based on result class or message
      failed_flag <- as.numeric(inherits(result, "expectation_failure"))
      error_flag <- as.numeric(inherits(result, "expectation_error"))
      skipped_flag <- as.numeric(inherits(result, "expectation_skip"))
      warning_flag <- as.numeric(grepl("warning", tolower(result$message)))
      
      # Extract start and end line numbers from srcref
      line_ref <- tryCatch({
        sr <- result$srcref
        if (!is.null(sr) && length(sr) >= 7) {
          start_line <- sr[1]
          end_line <- sr[7]
          paste0("L", start_line, "_L", end_line)
        } else {
          "L?"
        }
      }, error = function(e) "L?")
      
      test_rows[[length(test_rows) + 1]] <- list(
        file = paste0(test_group$file, "#", line_ref),
        context = test_group$context,
        test = test_group$test,
        failed = failed_flag,
        error = error_flag,
        skipped = skipped_flag,
        warning = warning_flag,
        nb = 1,
        real = test_group$real
      )
    }
  }
  
  test_summary_df <- do.call(rbind, lapply(test_rows, as.data.frame))
  
  # Compute pass/fail
  test_summary_df$pass <- as.numeric(
    (test_summary_df$failed + test_summary_df$error +
       test_summary_df$skipped + test_summary_df$warning) == 0
  )
  
  # Assign status
  test_summary_df$status <- "PASS"
  test_summary_df$status[test_summary_df$failed != 0] <- "FAILED"
  test_summary_df$status[test_summary_df$error != 0] <- "ERROR"
  test_summary_df$status[test_summary_df$skipped != 0] <- "SKIPPED"
  test_summary_df$status[test_summary_df$warning != 0] <- "WARNING"
  
  # Select and rename relevant columns
  result_df <- test_summary_df[, c("file", "context", "test", "status", "nb", "real")]
  names(result_df)[c(5, 6)] <- c("n", "time")
  
  return(result_df)
}


