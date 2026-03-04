#' Generate HTML Report for Package Test Assessment
#'
#' @description Generates an HTML report for the package test assessment results using rmarkdown.
#'
#' @param test_results List containing the results from get_package_coverage function.
#' @param output_dir (required) Character string path to an existing directory where the report will be saved. 
#'
#' @return Path to the generated HTML report.
#'
#' @examples
#' \dontrun{
#' test_results <- get_package_coverage()
#' # Always provide a directory; tempdir() is convenient in examples.
#' generate_test_report(test_results, output_dir = tempdir())
#' }
#' @importFrom rmarkdown render
#' @importFrom fs dir_exists
#' @importFrom fs path_abs
#' @export
generate_test_report <- function(test_results, output_dir = NULL) {
  
  # Check if rmarkdown is available
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    message("Package 'rmarkdown' is required but not installed.")
    return(NULL)
  }
  
  # check input
  checkmate::assert_list(test_results, names = "named", any.missing = TRUE)
  checkmate::assert_list(test_results$res_cov, names = "named", any.missing = TRUE)
  
  
  
  # output_dir is REQUIRED: must be a non-empty string, existing, and writable
  checkmate::assert_string(output_dir, min.chars = 1, na.ok = FALSE)
  output_dir <- fs::path_abs(output_dir)
  checkmate::assert_directory_exists(output_dir, access = "w")
  
  
  # Create a file name with pkg_name,version, and "risk_assessment.html"
  pkg_name <- test_results$pkg_name
  pkg_version <- test_results$pkg_ver
  date_time <- test_results$date_time
  
  # Create the risk summary data
  test_pkg_summary_output <- generate_test_pkg_summary(test_results)
  
  # Create the risk summary data
  test_details_output <- generate_test_details(test_results)
  
  # Capture the output of generate_coverage_section
  coverage_output <- generate_coverage_section(test_results, pkg_name)
  
  # set up data for checking if trace_matrix is empty
  total_coverage <- test_results$res_cov$coverage$totalcoverage
  file_coverage <- test_results$res_cov$coverage$filecoverage
  
  # check for non-standard testing framework results
  elements_to_check <- c("functions_no_tests", "tests_skipped", "tests_passing")
  
  nstf_elements <- check_elements_simple(test_results, elements_to_check) 
  
  # check for standard testing framework results
  elements_to_check <- c("long_summary", "test_skip")
  
  stf_elements <- check_elements_simple(test_results, elements_to_check) 
  
  # Check for non-standard testing framework
  nstf_none <- length(nstf_elements$present) == 0
  
  # Check for standard testing framework extra elements
  stf_none <- length(stf_elements$present) == 0
  
  # Initialize empty lists for data frames
  nstf_df_list <- list()
  
  stf_df_list <- list()
  
  # Check if both tests_skipped and tests_passing are present
  if ("functions_no_tests" %in% nstf_elements$present) {
    nstf_df_list$functions_no_tests <- test_results$functions_no_tests
  }
  
  # Check if both tests_skipped and tests_passing are present
  if ("tests_skipped" %in% nstf_elements$present 
      && length(test_results$tests_skipped) > 0
  ) {
    nstf_df_list$tests_skipped_df <- convert_vectors_to_df(test_results, "tests_skipped")[[1]]
  }
  
  # Check if both tests_skipped and tests_passing are present
  if ("tests_passing" %in% nstf_elements$present) {
    
    nstf_df_list$tests_passing_df <- convert_vectors_to_df(test_results, "tests_passing")[[1]]
    
    # Replace the 'file' column with the short path
    nstf_df_list$tests_passing_df$file <- sapply(nstf_df_list$tests_passing_df$file, extract_short_path)
    
  }
  
  # Check if long_summary  present
  if ("long_summary" %in% stf_elements$present) {
    long_summary <- create_long_summary_details(test_results$long_summary)
    stf_df_list$long_summary_df <- long_summary
  }
  
  # Check if tests_skip is present
  if ("test_skip" %in% stf_elements$present) {
    test_skip <- create_test_skip_details(test_results$test_skip)
    stf_df_list$tests_skip_df <- test_skip
  }
  
  # set up report environment
  report_env <- new.env()
  report_env$pkg_name <- pkg_name
  report_env$pkg_version <- pkg_version
  report_env$test_pkg_summary_output <- test_pkg_summary_output
  report_env$test_details_output <- test_details_output
  report_env$coverage_output  <- coverage_output
  report_env$functions_no_tests  <- nstf_df_list$functions_no_tests
  report_env$tests_skipped_df  <- nstf_df_list$tests_skipped_df
  report_env$tests_passing_df  <- nstf_df_list$tests_passing_df
  report_env$tests_skip_df  <- stf_df_list$tests_skip_df
  report_env$long_summary_df  <- stf_df_list$long_summary_df
  
  # check for type of testing framework 
  if (nstf_none) {
    # standard testing framework template
    template_path <- system.file(
      "report_templates",
      "test_report_template_stf.Rmd",
      package = "test.assessr") 
  } else {
    # non-standard testing framework template
    template_path <- system.file(
      "report_templates",
      "test_report_template_nstf.Rmd",
      package = "test.assessr")
  }
  
  
  # Compose output file path within the provided directory --------------------
  output_file <- fs::path(
    output_dir,
    paste0("test_report_", pkg_name, "_", pkg_version, ".html")
  )
  
  # Render if allowed
  if (identical(Sys.getenv("NOT_CRAN"), "true") || interactive()) {
    rmarkdown::render(
      input       = template_path,
      output_file = output_file,
      envir       = report_env,
      quiet       = TRUE
    )
  } else {
    message("Rendering skipped on CRAN or non-interactive environment.")
    return(NULL)
  }
  
  return(output_file)

}

#' Helper function to replace NULL with "N/A"
#'
#' @param x - input value
#'
#' @keywords internal
handle_null <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("N/A")
  } else if (is.logical(x)) {
    return(paste(as.character(x), collapse = ", "))
  } else if (is.numeric(x)) {
    return(paste(as.character(x), collapse = " "))
  } else {
    return(x)
  }
}


#' Convert number to abbreviation
#'
#' @param value 
#'
#' @keywords internal
convert_number_to_abbreviation <- function(value) {
  result <- NA
  
  if (!is.na(value) && is.numeric(value)) {
    abs_value <- abs(value)
    
    if (abs_value >= 1e6) {
      result <- paste0(round(value / 1e6, 1), "M")
    } else if (abs_value >= 1e3) {
      result <- paste0(round(value / 1e3, 1), "K")
    } else {
      result <- as.character(value)
    }
  }
  
  return(result)
}

#' Convert number to percent
#'
#' @param value 
#'
#' @keywords internal
convert_number_to_percent <- function(value) {
  
  result <- NA
  
  if (!is.na(value)) {
    # Try to coerce to numeric if it's a character
    if (is.character(value)) {
      value <- suppressWarnings(as.numeric(value))
    }
    
    # Proceed if it's now numeric and not NA
    if (!is.na(value) && is.numeric(value)) {
      
      percent_value <- round(
        ifelse(value <= 1, value * 100, value),
        1
      )
      
      result <- paste0(percent_value, "%")
    }
  }
  
  return(result)
}

#' Helper to conditionally apply handle_null or abbreviation
#'
#' @param x - value
#'
#' @keywords internal
safe_value <- function(x) {
  if (is.null(x)) {
    return(handle_null(x))
  } else if (is.numeric(x)) {
    return(convert_number_to_abbreviation(x))
  } else {
    return(handle_null(x))
  }
}



#' Generate Test Summary
#'
#' @description Generates the Risk Summary table for the HTML report.
#'
#' @param assessment_results - input data
#'
#' @keywords internal
generate_test_pkg_summary <- function(assessment_results) {
  
  test_pkg_summary_table <- data.frame(
    Metric = c('Package', 'Version'), 
    Value = c(
      assessment_results$pkg_name,
      assessment_results$pkg_ver
    )
  )
  
  return(test_pkg_summary_table)
}

#' Generate Test Details
#'
#' @description Generates the Test Details table for the HTML report.
#'
#' @param assessment_results - input data
#'
#' @keywords internal
generate_test_details <- function(assessment_results) {
  
  # Ensure covr is numeric before rounding
  if (!is.numeric(assessment_results$total_cov)) {
    assessment_results$total_cov <- 
      as.numeric(assessment_results$results$covr)
  }
  assessment_results$total_cov <- 
    round(assessment_results$total_cov, 2)
  
  covr <- handle_null(assessment_results$total_cov)
  
  covr <- convert_number_to_percent(covr)
  r_version <- as.character(getRversion())
  
  test_details_table <- data.frame(
    Metric = c(
      'Test Coverage Score', 'Date Time', 'Executor', 
      'OS Name', 'OS Release', 'OS Machine', 
      'R version', 'Testing Framework'
    ),
    Value = c(
      covr,
      handle_null(assessment_results$date_time),
      handle_null(assessment_results$executor),
      handle_null(assessment_results$sysname),
      handle_null(assessment_results$release),
      handle_null(assessment_results$machine),
      handle_null(assessment_results$r_version),
      handle_null(assessment_results$test_framework_type)
    )
  )
  return(test_details_table)
}

#' Generate file coverage df
#'
#' @description Generates file coverage df when errors list created.
#' 
#' @param file_names - file names
#' @param file_coverage - test coverage for files
#' @param errors - test coverage errors
#' @param notes - test coverage notes
#' 
#' @keywords internal
create_file_coverage_df <- function(file_names, file_coverage, errors, notes) {
  
  # Convert errors to a character string if it is a complex structure
  if (is.list(errors)) {
    errors <- sapply(errors, function(x) {
      if (is.null(x)) {
        return("N/A")
      } else if (is.data.frame(x)) {
        return(paste(utils::capture.output(print(x)), collapse = "; "))
      } else if (is.list(x)) {
        return(paste(unlist(x), collapse = "; "))
      } else {
        return(as.character(x))
      }
    })
  }
  
  # Ensure the number of rows match
  max_len <- max(length(file_names), length(file_coverage), length(notes))
  
  # Extend lists to match the maximum length
  file_names <- c(file_names, rep("", max_len - length(file_names)))
  
  # extract short file name from long path
  file_names <- vapply(file_names, extract_short_path, FUN.VALUE = character(1))
  
  file_coverage <- c(file_coverage, rep(NA, max_len - length(file_coverage)))
  notes <- c(notes, rep("", max_len - length(notes)))
  errors <- rep(paste(errors, collapse = "; "), max_len)
  
  # Create the data frame
  file_coverage_df <- data.frame(
    File = file_names,
    Coverage = file_coverage,
    Errors = errors,
    Notes = notes,
    stringsAsFactors = FALSE
  )
  
  return(file_coverage_df)
}

#' Generate Coverage Section
#'
#' @description Generates the Coverage section for the HTML report.
#' 
#' @param assessment_results - input data
#' @param pkg_name - name of the package
#' 
#' @keywords internal
generate_coverage_section <- function(assessment_results, pkg_name) {
  
  total_coverage <- assessment_results$res_cov$coverage$totalcoverage
  file_coverage <- assessment_results$res_cov$coverage$filecoverage
  
  # Extract file names from the attributes
  file_names <- attr(file_coverage, "dimnames")[[1]]
  
  # extract short file name from long path
  file_names <- vapply(file_names, extract_short_path, FUN.VALUE = character(1))
  
  # Handle errors and notes
  errors <- assessment_results$res_cov$errors
  if (all(is.na(errors))) {
    errors <- "No test coverage errors"
  }
  
  notes <- assessment_results$res_cov$notes
  if (all(is.na(notes))) {
    notes <- "No test coverage notes"
  }
  
  # Create a data frame for file coverage
  if (is.list(errors) && all(c("message", "srcref", "status", "stdout", "stderr", "parent_trace", "call", "procsrcref", "parent") %in% names(errors))) {
    # Create the file coverage data frame
    file_coverage_df <- create_file_coverage_df(file_names, file_coverage, errors, notes)
  } else {
    # Handle the case where errors does not have the expected structure
    
    if (is.null(file_names) || is.null(file_coverage)) {
      file_coverage_df <- data.frame(
        Function = NA_character_,
        Coverage = NA_real_,
        Errors = NA,
        Notes = NA,
        stringsAsFactors = FALSE
      )
    } else {
      file_coverage_df <- data.frame(
        Function = file_names,
        Coverage = file_coverage,
        Errors = errors,
        Notes = notes,
        stringsAsFactors = FALSE
      )
    }
  }
  
  return(file_coverage_df)
}

#' Check Which Elements Exist in a List
#'
#' This internal function checks whether specified elements exist in a given list
#' and returns two vectors: elements that exist and elements that do not.
#'
#' @param x A list object to check (e.g., `test_results`).
#' @param elems A character vector of element names to check.
#'
#' @return A list with:
#' \describe{
#'   \item{present}{Character vector of elements that exist in the list.}
#'   \item{missing}{Character vector of elements that do not exist in the list.}
#' }
#'
#' @keywords internal
check_elements_simple <- function(x, elems) {
  exists_flags <- elems %in% names(x)
  elements_checked <- list(
    present = elems[exists_flags],
    missing = elems[!exists_flags]
  )
  return(elements_checked)
}


' Convert Character Vectors to Data Frames
#'
#' This internal function takes named character vectors (e.g., `tests_skipped`, `tests_passing`)
#' from a list and converts them into data frames for easier reporting or processing.
#'
#' @param x A list containing character vectors (e.g., `test_results`).
#' @param elems A character vector of element names to convert (must exist in `x`).
#'
#' @return A named list of data frames, where each data frame has one column:
#' \describe{
#'   \item{file}{The file names or paths from the original character vector.}
#' }
#'
#' @keywords internal
convert_vectors_to_df <- function(x, elems) {
  # Filter only elements that exist
  elems <- elems[elems %in% names(x)]
  
  # Convert each to a data frame
  result <- lapply(elems, function(nm) {
    data.frame(file = x[[nm]], stringsAsFactors = FALSE)
  })
  
  names(result) <- elems
  return(result)
}

#' Generate Long Summary Details
#'
#' @description Generates the  Long Summary table for the HTML report.
#'
#' @param df - input data
#'
#' @keywords internal
create_long_summary_details <- function(df) {
  
  # validate input
  required <- c("file", "code_script", "status")
  if (!all(required %in% names(df))) {
    stop("Input df must contain columns: file, code_script, status")
  }
  
  file_orig <- as.character(df$file)
  func <- as.character(df$code_script)
  status_vec <- as.character(df$status)
  
  # extract portion before '#' (if any)
  before_hash <- sub("#.*$", "", file_orig)
  
  test <- before_hash  
  
  # extract the part after '#' (if any); then take up to first '_' (if any)
  after_hash <- ifelse(grepl("#", file_orig), sub(".*#", "", file_orig), NA_character_)
  start_line <- ifelse(is.na(after_hash) | after_hash == "",
                       NA_character_,
                       sub("_.*$", "", after_hash))
  
  # assemble output (use 'function' as requested; if you prefer a different name change here)
  out <- data.frame(
    `R function` = func,
    test = test,
    start_line = start_line,
    status = status_vec,
    stringsAsFactors = FALSE
  )
  
  # Filter: keep only rows with status == "PASS" (case/whitespace tolerant)
  keep <- toupper(trimws(out$status)) == "PASS"
  out <- out[keep, , drop = FALSE]
  
  rownames(out) <- NULL
  
  return(out)
}


#' Generate Test Skip Details
#'
#' @description Generates the Test Skip table for the HTML report.
#'
#' @param df - input data
#'
#' @keywords internal
create_test_skip_details <- function(df) {
  # validate input
  required <- c("file", "code_script", "status", "expectation", "line1")
  if (!all(required %in% names(df))) {
    stop("Input df must contain columns: file, code_script, status, expectation, line1")
  }
  
  file_orig    <- as.character(df$file)
  func         <- as.character(df$code_script)
  status_vec   <- as.character(df$status)
  expect_vec   <- as.character(df$expectation)
  line1_vec    <- as.character(df$line1)
  
  # test: portion of `file` before '#' (if present); keep digits before .R (e.g. test-myscript2.R)
  test <- sub("#.*$", "", file_orig)
  
  # start_line: rename of line1 (keeps as character)
  start_line <- line1_vec
  
  out <- data.frame(
    `Rfunction`    = func,
    test        = test,
    status      = status_vec,
    expectation = expect_vec,
    start_line  = start_line,
    stringsAsFactors = FALSE
  )
  
  rownames(out) <- NULL
  return(out)
}
