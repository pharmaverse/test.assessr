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
  is_multi <- isTRUE(test_results$multi_framework) &&
    !is.null(test_results$results) &&
    length(test_results$results) > 0L
  if (!is_multi) {
    checkmate::assert_list(test_results$res_cov, names = "named", any.missing = TRUE)
  }
  
  # Normalize (single unchanged; multi gets multi_framework + frameworks)
  test_results <- normalize_test_results(test_results)
  
  # output_dir is REQUIRED: must be a non-empty string, existing, and writable
  checkmate::assert_string(output_dir, min.chars = 1, na.ok = FALSE)
  output_dir <- fs::path_abs(output_dir)
  checkmate::assert_directory_exists(output_dir, access = "w")
  
  pkg_name <- test_results$pkg_name
  pkg_version <- test_results$pkg_ver
  date_time <- test_results$date_time
  multi_framework <- isTRUE(test_results$multi_framework)
  frameworks <- if (multi_framework) test_results$frameworks else character(0L)
  
  # Build report_env for unified template
  report_env <- new.env()
  report_env$pkg_name <- pkg_name
  report_env$pkg_version <- pkg_version
  report_env$multi_framework <- multi_framework
  report_env$frameworks <- frameworks
  
  test_pkg_summary_output <- generate_test_pkg_summary(test_results)
  report_env$test_pkg_summary_output <- test_pkg_summary_output
  
  if (!multi_framework) {
    # Single framework: current flow
    test_details_output <- generate_test_details(test_results)
    coverage_output <- generate_coverage_section(test_results, pkg_name)
    report_env$test_details_output <- test_details_output
    report_env$coverage_output <- coverage_output
    
    nstf_elements <- check_elements_simple(test_results, c("functions_no_tests", "tests_skipped", "tests_passing"))
    stf_elements <- check_elements_simple(test_results, c("long_summary", "test_skip"))
    nstf_none <- length(nstf_elements$present) == 0
    stf_none <- length(stf_elements$present) == 0
    
    nstf_df_list <- list()
    stf_df_list <- list()
    if ("functions_no_tests" %in% nstf_elements$present) {
      nstf_df_list$functions_no_tests <- test_results$functions_no_tests
      short_is_needed <- detect_path_shortening(nstf_df_list$functions_no_tests)
      if (short_is_needed) {
        nstf_df_list$functions_no_tests <- create_function_locations_df(nstf_df_list$functions_no_tests)
      }
    }
    if ("tests_skipped" %in% nstf_elements$present && length(test_results$tests_skipped) > 0) {
      nstf_df_list$tests_skipped_df <- convert_vectors_to_df(test_results, "tests_skipped")[[1]]
      short_is_needed <- detect_path_shortening(nstf_df_list$tests_skipped_df, "file")
      if (short_is_needed) {
        nstf_df_list$tests_skipped_df$file <- sapply(nstf_df_list$tests_skipped_df$file, extract_short_path)
      }
    }
    if ("tests_passing" %in% nstf_elements$present) {
      nstf_df_list$tests_passing_df <- convert_vectors_to_df(test_results, "tests_passing")[[1]]
      nstf_df_list$tests_passing_df$file <- sapply(nstf_df_list$tests_passing_df$file, extract_short_path)
    }
    if ("long_summary" %in% stf_elements$present) {
      stf_df_list$long_summary_df <- create_long_summary_details(test_results$long_summary)
    }
    if ("test_skip" %in% stf_elements$present) {
      stf_df_list$tests_skip_df <- create_test_skip_details(test_results$test_skip)
    }
    
    report_env$functions_no_tests <- nstf_df_list$functions_no_tests
    report_env$tests_skipped_df <- nstf_df_list$tests_skipped_df
    report_env$tests_passing_df <- nstf_df_list$tests_passing_df
    report_env$tests_skip_df <- stf_df_list$tests_skip_df
    report_env$long_summary_df <- stf_df_list$long_summary_df
    report_env$has_stf <- !stf_none
    report_env$has_nstf <- !nstf_none
  } else {
    # Multi-framework: build framework_results
    res_list <- test_results$results
    coverage_sections <- generate_coverage_sections(test_results, pkg_name)
    nstf_elements <- check_elements_simple(test_results, c("functions_no_tests", "tests_skipped", "tests_passing"), nested = TRUE)
    stf_elements <- check_elements_simple(test_results, c("long_summary", "test_skip"), nested = TRUE)
    has_stf <- length(stf_elements$present) > 0
    has_nstf <- length(nstf_elements$present) > 0
    report_env$has_stf <- has_stf
    report_env$has_nstf <- has_nstf
    
    framework_results <- setNames(lapply(frameworks, function(fw) {
      r <- res_list[[fw]]
      meta <- c(test_results, r)
      meta$date_time <- test_results$date_time
      meta$executor <- test_results$executor
      meta$sysname <- test_results$sysname
      meta$release <- test_results$release
      meta$machine <- test_results$machine
      meta$r_version <- test_results$r_version
      meta$pkg_name <- test_results$pkg_name
      meta$pkg_ver <- test_results$pkg_ver
      meta$total_cov <- r$total_cov
      
      out <- list(
        coverage_output = coverage_sections[[fw]],
        test_details_output = generate_test_details(meta, framework_id = fw)
      )
      if (has_nstf) {
        if (!is.null(r$functions_no_tests) && is.data.frame(r$functions_no_tests) && nrow(r$functions_no_tests) > 0) {
          fn <- r$functions_no_tests
          short_is_needed <- tryCatch(detect_path_shortening(fn), error = function(e) FALSE)
          if (short_is_needed) {
            fn <- tryCatch(create_function_locations_df(fn), error = function(e) r$functions_no_tests)
          }
          out$functions_no_tests <- fn
        }
        if (!is.null(r$tests_skipped) && length(r$tests_skipped) > 0) {
          df <- convert_vectors_to_df(list(tests_skipped = r$tests_skipped), "tests_skipped")[[1]]
          short_is_needed <- detect_path_shortening(df, "file")
          if (short_is_needed) df$file <- sapply(df$file, extract_short_path)
          out$tests_skipped_df <- df
        }
        if (!is.null(r$tests_passing) && length(r$tests_passing) > 0) {
          df <- convert_vectors_to_df(list(tests_passing = r$tests_passing), "tests_passing")[[1]]
          df$file <- sapply(df$file, extract_short_path)
          out$tests_passing_df <- df
        }
      }
      if (has_stf) {
        if (!is.null(r$long_summary) && is.data.frame(r$long_summary) && nrow(r$long_summary) > 0) {
          out$long_summary_df <- create_long_summary_details(r$long_summary)
        }
        if (!is.null(r$test_skip) && is.data.frame(r$test_skip) && nrow(r$test_skip) > 0) {
          out$tests_skip_df <- create_test_skip_details(r$test_skip)
        }
      }
      out
    }), frameworks)
    report_env$framework_results <- framework_results
  }
  
  template_path <- system.file(
    "report_templates",
    "test_report_template.Rmd",
    package = "test.assessr")
  
  
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

#' Normalize test results for single or multi-framework structure
#'
#' @description For single-framework results, passes through unchanged. For
#'   multi-framework, sets \code{multi_framework} and \code{frameworks} while
#'   keeping \code{results} as a named list of per-framework blocks (no flattening).
#'
#' @param test_results List from \code{get_package_coverage} or \code{run_covr_modes}.
#'
#' @return A list with \code{multi_framework} (logical), \code{frameworks}
#'   (character, multi only), and \code{results} preserved for multi-framework.
#'
#' @keywords internal
normalize_test_results <- function(test_results) {
  is_multi <- isTRUE(test_results$multi_framework) &&
    !is.null(test_results$results) &&
    length(test_results$results) > 0L
  
  if (!is_multi) {
    return(test_results)
  }
  
  test_results$multi_framework <- TRUE
  test_results$frameworks <- names(test_results$results)
  test_results
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
#' @param value - number to be converted
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
#' @param value - number to be converted
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
#' @param framework_id Optional character; when provided (multi-framework), the
#'   "Testing Framework" row shows this framework name.
#'
#' @keywords internal
generate_test_details <- function(assessment_results, framework_id = NULL) {
  
  total_cov <- assessment_results$total_cov
  if (!is.numeric(total_cov)) {
    fallback <- tryCatch(
      as.numeric(assessment_results$results[["covr"]]),
      error = function(e) NA_real_
    )
    total_cov <- if (!is.na(fallback)) fallback else NA_real_
  }
  total_cov <- round(total_cov, 2)
  
  covr <- handle_null(total_cov)
  covr <- convert_number_to_percent(covr)
  
  fw_label <- if (!is.null(framework_id)) {
    handle_null(framework_id)
  } else {
    handle_null(assessment_results$test_framework_type)
  }
  
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
      fw_label
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

#' Generate Coverage Section (single framework)
#'
#' @description Generates the Coverage section for the HTML report from a single
#'   framework's \code{res_cov}.
#' 
#' @param assessment_results - input data with \code{res_cov}
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

#' Generate Coverage Sections (multi-framework)
#'
#' @description Returns a named list of coverage data frames, one per framework.
#' 
#' @param test_results Multi-framework list with \code{results} (named list of
#'   per-framework blocks, each with \code{res_cov}).
#' @param pkg_name - name of the package
#' 
#' @return Named list of data frames (Function, Coverage, Errors, Notes).
#' @keywords internal
generate_coverage_sections <- function(test_results, pkg_name) {
  frameworks <- names(test_results$results)
  res_list <- test_results$results
  out <- setNames(
    lapply(frameworks, function(fw) {
      r <- res_list[[fw]]
      if (is.null(r$res_cov) || is.null(r$res_cov$coverage)) {
        return(data.frame(
          Function = NA_character_,
          Coverage = NA_real_,
          Errors = "No coverage data",
          Notes = "No coverage notes",
          stringsAsFactors = FALSE
        ))
      }
      fake_assessment <- list(res_cov = r$res_cov)
      generate_coverage_section(fake_assessment, pkg_name)
    }),
    frameworks
  )
  out
}

#' Check Which Elements Exist in a List
#'
#' This internal function checks whether specified elements exist in a given list
#' and returns two vectors: elements that exist and elements that do not. When
#' \code{nested = TRUE} and \code{x} has \code{results} (multi-framework), checks
#' inside each \code{x$results$<framework>}.
#'
#' @param x A list object to check (e.g., `test_results`).
#' @param elems A character vector of element names to check.
#' @param nested If TRUE and \code{x} has \code{results}, check inside each
#'   framework block. Returns \code{present} (flattened unique) and
#'   \code{by_framework} (named list: framework -> present elements).
#'
#' @return A list with:
#' \describe{
#'   \item{present}{Character vector of elements that exist.}
#'   \item{missing}{Character vector of elements that do not exist (top-level only).}
#'   \item{by_framework}{Named list (framework -> present elements), when nested.}
#' }
#'
#' @keywords internal
check_elements_simple <- function(x, elems, nested = FALSE) {
  if (!nested || is.null(x$results)) {
    exists_flags <- elems %in% names(x)
    return(list(
      present = elems[exists_flags],
      missing = elems[!exists_flags]
    ))
  }
  frameworks <- names(x$results)
  by_fw <- setNames(
    lapply(frameworks, function(fw) {
      r <- x$results[[fw]]
      elems[elems %in% names(r)]
    }),
    frameworks
  )
  present <- unique(unlist(by_fw, use.names = FALSE))
  missing <- setdiff(elems, present)
  list(present = present, missing = missing, by_framework = by_fw)
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


#' Normalize a function-locations data frame
#'
#' Accept a data frame containing function locations and return a normalized
#' data frame with columns `file`, `function_name`, `start_line`, and `end_line`.
#' If the `file` column is missing or entirely `NA`, file names may be derived
#' from `attr(file_coverage, "dimnames")[[1]]`. File paths are shortened with
#' `basename()` by default; a custom shortener can be provided.
#'
#' @param df A data frame with at least columns `function_name`, `start_line`,
#'   `end_line`. Column `file` is recommended, but may be omitted if
#'   `file_coverage` is supplied.
#' @param file_coverage Optional coverage-like object whose first `dimnames`
#'   entry contains file paths; used only if `df$file` is missing or entirely `NA`.
#' @param extract_short_path A function to convert long paths to short names.
#'   Defaults to `basename`. Set to `NULL` to skip shortening.
#'
#' @return A `data.frame` with columns:
#'   `file` (character), `function_name` (character),
#'   `start_line` (integer), `end_line` (integer).
#'
#' @keywords internal
#' @noRd
create_function_locations_df <- function(
    df,
    file_coverage = NULL,
    extract_short_path = base::basename
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame (or tibble).", call. = FALSE)
  }
  
  # --- Resolve required columns (allow a few common variants) ---
  cn <- names(df)
  tolower_cn <- tolower(cn)
  
  find_col <- function(candidates_regex) {
    idx <- grep(paste0("^(", candidates_regex, ")$"), tolower_cn, perl = TRUE)
    if (length(idx)) cn[idx[1]] else NA_character_
  }
  
  col_function_name <- find_col("function_name|function|fn|func")
  col_file          <- find_col("file|filepath|path")
  col_start_line    <- find_col("start_line|start|startline")
  col_end_line      <- find_col("end_line|end|endline")
  
  missing_core <- setdiff(
    c("function_name", "start_line", "end_line"),
    c(if (!is.na(col_function_name)) "function_name",
      if (!is.na(col_start_line))   "start_line",
      if (!is.na(col_end_line))     "end_line")
  )
  if (length(missing_core)) {
    stop(
      "Missing required column(s): ",
      paste(missing_core, collapse = ", "),
      ". Expected columns include: function_name, start_line, end_line.",
      call. = FALSE
    )
  }
  
  function_name <- df[[col_function_name]]
  start_line    <- df[[col_start_line]]
  end_line      <- df[[col_end_line]]
  file          <- if (!is.na(col_file)) df[[col_file]] else NULL
  
  # --- If file is missing or entirely NA, try to derive from coverage dimnames ---
  needs_file <- is.null(file) || all(is.na(file))
  if (needs_file) {
    dn <- tryCatch(attr(file_coverage, "dimnames"), error = function(e) NULL)
    if (!is.null(dn) && length(dn) >= 1L && !is.null(dn[[1]])) {
      fc_files <- as.character(dn[[1]])
      n <- nrow(df)
      if (length(fc_files) == n) {
        file <- fc_files
      } else if (length(fc_files) == 1L) {
        file <- rep(fc_files, n)
      } else {
        # pad/truncate to n rows
        file <- rep(NA_character_, n)
        file[seq_len(min(n, length(fc_files)))] <- fc_files[seq_len(min(n, length(fc_files)))]
      }
    } else {
      file <- rep(NA_character_, nrow(df))
    }
  }
  
  
  # --- Coerce types ---
  function_name <- as.character(function_name)
  file          <- as.character(file)
  start_line    <- as.integer(start_line)
  end_line      <- as.integer(end_line)
  
  # --- Shorten paths, if requested ---
  if (!is.null(extract_short_path)) {
    file <- vapply(
      file,
      function(p) {
        if (is.na(p) || !nzchar(p)) return(p)
        extract_short_path(p)
      },
      FUN.VALUE = character(1)
    )
  }
  
  # --- HARDEN AGAINST STRAY NAMES/ROW.NAMES ON VECTORS ---
  # 1) Drop any names on the vectors (these can be used as row.names by data.frame)
  file          <- unname(file)
  function_name <- unname(function_name)
  start_line    <- unname(start_line)
  end_line      <- unname(end_line)
  
  # 2) Also remove any stray 'row.names' attribute on the vectors (ultra defensive)
  attr(file, "row.names")          <- NULL
  attr(function_name, "row.names") <- NULL
  attr(start_line, "row.names")    <- NULL
  attr(end_line, "row.names")      <- NULL
  
  # --- Build normalized output, then force safe row names ---
  n <- length(function_name)
  
  out <- data.frame(
    file = file,
    function_name = function_name,
    start_line = start_line,
    end_line = end_line,
    stringsAsFactors = FALSE
  )
  
  # Force clean numeric row names (prevents "row names contain missing values")
  row.names(out) <- seq_len(n)
  
  return(out)
}  


#' Detect if path shortening is advisable (boolean, df input)
#'
#' Given a data frame and a column that contains file paths, decide whether
#' shortening those paths is advisable. Returns a single TRUE/FALSE.
#'
#' @param df A data frame that contains a path column.
#' @param file_col Character scalar **or vector** of candidate column names that hold
#'   paths. Defaults to `c("file", "source_file")`. The first existing column
#'   (case-insensitive) will be used.
#' @param max_len_threshold Consider paths "long" if any has length > this (default: 60).
#' @param gap_threshold Consider shortening if `max(nchar(full)) - max(nchar(basename))`
#'   is at least this many characters (default: 10).
#' @param min_common_prefix Consider a "long" common directory prefix if >= this many
#'   characters (default: 20).
#' @param min_dir_share Minimum fraction of entries that must contain a directory
#'   separator to treat them as true paths (default: 0.2).
#'
#' @return TRUE if shortening is advisable; otherwise FALSE.
#'
#' @examples
#' \dontrun{
#' # Explicit column name "file"
#' short_is_needed <- detect_path_shortening(nstf_df_list$functions_no_tests, "file")
#'
#' # Explicit column name "source_file"
#' short_is_needed <- detect_path_shortening(nstf_df_list$functions_no_tests, "source_file")
#'
#' # Let the function choose the first that exists among c("file","source_file")
#' short_is_needed <- detect_path_shortening(nstf_df_list$functions_no_tests)
#' }
#' @keywords internal
#' @noRd
detect_path_shortening <- function(
    df,
    file_col = c("file", "source_file"),
    max_len_threshold = 60L,
    gap_threshold = 10L,
    min_common_prefix = 20L,
    min_dir_share = 0.2
) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame.", call. = FALSE)
  }
  
  # Resolve the path column: accept a single name or a vector of candidates
  resolve_col <- function(df, candidates) {
    if (length(candidates) == 1L) {
      if (!(candidates %in% names(df))) {
        # try case-insensitive
        idx <- which(tolower(names(df)) == tolower(candidates))
        if (length(idx)) return(names(df)[idx[1]])
        return(NA_character_)
      }
      return(candidates)
    } else {
      # pick the first candidate that exists (case-insensitive)
      df_lower <- tolower(names(df))
      cand_lower <- tolower(candidates)
      for (cl in cand_lower) {
        hit <- which(df_lower == cl)
        if (length(hit)) return(names(df)[hit[1]])
      }
      return(NA_character_)
    }
  }
  
  col <- resolve_col(df, file_col)
  if (is.na(col)) {
    stop(
      "Could not find any of the specified path columns in `df`. Looked for: ",
      paste(file_col, collapse = ", "),
      call. = FALSE
    )
  }
  
  paths <- df[[col]]
  xp <- as.character(paths)
  # normalize to '/' for detection; keep logic simple
  xp_norm <- gsub("\\\\", "/", xp)
  xpn <- xp_norm[!is.na(xp_norm) & nzchar(xp_norm)]
  
  # If no usable paths, no shortening needed
  if (!length(xpn)) return(FALSE)
  
  # Share with directory separators
  dir_share <- mean(grepl("/", xpn, fixed = TRUE))
  
  # If hardly any directory separators, shortening not helpful
  if (!(dir_share >= min_dir_share)) return(FALSE)
  
  # Length heuristics
  base_names <- basename(xpn)
  max_full <- max(nchar(xpn), na.rm = TRUE)
  max_base <- max(nchar(base_names), na.rm = TRUE)
  big_gap  <- (max_full - max_base) >= gap_threshold
  too_long <- max_full > max_len_threshold
  
  # Longest common directory prefix (approximate, trimmed to dir boundary)
  common_prefix_raw <- {
    if (length(xpn) == 1L) xpn else {
      split_chars <- strsplit(xpn, "", fixed = TRUE)
      min_len <- min(lengths(split_chars))
      lcp_len <- 0L
      for (i in seq_len(min_len)) {
        chars_i <- vapply(split_chars, function(v) v[i], "", USE.NAMES = FALSE)
        if (length(unique(chars_i)) == 1L) lcp_len <- i else break
      }
      substr(xpn[1], 1L, lcp_len)
    }
  }
  cut_at <- regexpr(".*/", common_prefix_raw)
  common_prefix <- if (cut_at[1] > 0) substr(common_prefix_raw, 1, attr(cut_at, "match.length")) else ""
  long_common_prefix <- nchar(common_prefix) >= min_common_prefix
  
  # Final decision
  is_needed <- (too_long || big_gap || long_common_prefix)
  
  return(is_needed)
}
