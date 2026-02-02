#' Run covr with failing tests
#' #' 
#' This function runs test coverage and skip analysis for a package using testthat or other supported frameworks.
#'
#' @param pkg_source_path Character. Path to the root of the package source.
#' @param test_pkg_data List. Output from `check_pkg_tests_and_snaps()` indicating test framework presence.
#' @param preserve_vars Character. preserve `old` for CRAN examples 
#'
#' @return skip_results - test coverage with PASS tests passing or FAIL for tests failing
#' 
#' @examples
#' \dontrun{
#' r = getOption("repos")
#' r["CRAN"] = "http://cran.us.r-project.org"
#' old <- options(repos = r)
#'
#' dp <- system.file("test-data", 
#'   "test.package.0014_0.1.0.tar.gz", 
#'   package = "test.assessr")
#' 
#' # set up package
#' install_list <- set_up_pkg(dp)
#'
#' package_installed <- install_list$package_installed
#' pkg_source_path <- install_list$pkg_source_path
#'
#' # install package locally to ensure test works
#' package_installed <- install_package_local(pkg_source_path)
#' package_installed <- TRUE
#'
#' test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
#' test_covr_list <- run_covr_skip_stf(pkg_source_path, test_pkg_data)
#' 
#' options(old)
#' }  
#' @export
run_covr_skip_stf <- function(pkg_source_path, 
                              test_pkg_data, 
                              preserve_vars = "old"
                              ) {
  
  # Input checking
  checkmate::assert_string(pkg_source_path)
  checkmate::assert_directory_exists(pkg_source_path)
  
  path <- as.character(pkg_source_path)
  testdir <- file.path(path, "tests")
  pkg <- get_pkg_name(pkg_source_path)
  
  test_path <- get_stf_test_path(test_pkg_data, testdir)
  
  if (is.null(test_path)) {
    message(paste0("No test directory found for ", pkg))
    return(create_empty_covr_list(pkg, "No test directory found"))
  }
  
  setwd(path)
  message(paste0("Creating Default test reporter for ", pkg))
  testthat::set_max_fails(Inf)
  
  # unload target package
  try(unloadNamespace(pkg), silent = TRUE)
  
  # create fail reporter
  create_fail_reporter(test_path, pkg)
  
  # create default reporter
  test_results <- create_test_reporter(test_path, pkg)
  
  if (is.null(test_results)) {
    return(create_empty_covr_list(pkg, "Default reporter execution failed"))
  } else {
    # update context column 
    test_results <- fix_test_context(test_results)
  }
  
  message(paste0("Mapping package test structure for ", pkg))
  map_path <- test_path
  test_map <- tryCatch({
    map_tests_stf(map_path)
  }, error = function(e) {
    message(paste0("Error mapping tests for ", pkg, " : ", e$message))
    return(NULL)
  })
 
  if (is.null(test_map)) {
    message(paste0("Test map could not be created for ", pkg))
    return(create_empty_covr_list(pkg, "Test map creation failed"))
  }
  
  test_results_long <- get_tests_long_sum_stf(test_results)
  
  if (is.null(test_results_long)) {
    message(paste0("Unable to create skipped tests for :", pkg))
    return(create_empty_covr_list(pkg, "Skipped tests map creation failed"))
  }
  
  lookup <- c(code_script = "context", context = "test")
  test_results_long <- dplyr::rename(test_results_long, dplyr::all_of(lookup))
  
  test_results_filtered <- test_results_long[test_results_long$status != 'PASS', ]
  test_results_filtered$file <- gsub('#(.*?)$', '', basename(test_results_filtered$file))
  test_map <- dplyr::rename(test_map, context = "test")
  
  status_filter <- c("FAILED", "ERROR", "SKIPPED")
  problem_tests <- subset(test_results_filtered, status %in% status_filter)
  
  if (nrow(problem_tests) == 0) {
    problem_tests <- data.frame(
      file = "", code_script = "", context = "", status = "",
      n = NA_integer_, time = NA_real_, stringsAsFactors = FALSE
    )
  }
  
  failed <- any(test_results_long$status %in% c('ERROR', 'FAILED'))
  
  if (failed) {
    test_skip <- get_tests_skip_stf(problem_tests, test_map)
    message(paste0("Creating skip test map for ", pkg))
    cvr <- create_coverage_skip_stf(pkg_source_path, 
                                    pkg, 
                                    map_path, 
                                    test_skip, 
                                    test_map, 
                                    preserve_vars = "old")
    
    message(paste0("Creating test coverage for ", pkg))
    res_sum <- create_results_summary(cvr)
    
    covr_list <- list(
      total_cov = res_sum$total_cov,
      res_cov = list(
        name = res_sum$res_cov$name,
        coverage = res_sum$res_cov$coverage,
        errors = res_sum$res_cov$errors,
        notes = res_sum$res_cov$notes
      ),
      long_summary = test_results_long,
      test_skip = test_skip
    )
  } else {
    message(paste0("Creating test results for no skipped tests for ", pkg))
    covr_list <- create_covr_list_no_skip(test_map, 
                                          test_results, 
                                          pkg_source_path, 
                                          pkg,
                                          preserve_vars = "old")
  }
  
  return(covr_list)
}


#' @title Create Test Reporter
#' @description Executes all tests in a given directory using `testthat::test_dir()`
#' and applies context fixes to ensure each test has a valid context.
#'
#' @param test_path Character string specifying the path to the test directory.
#' @param pkg Character string specifying the package name.
#'
#' @return A list of test results with corrected contexts, or `NULL` if an error occurs.
#' @keywords internal
create_test_reporter <- function(test_path, pkg) {
  
  message(paste0("Creating test reporter for: ", pkg))
  test_results <- tryCatch({
    sink(tempfile())
    res <- testthat::test_dir(
      path = test_path,
      reporter = testthat::default_reporter(),
      stop_on_failure = FALSE,
      stop_on_warning = FALSE,
      package = pkg,
      load_package = "source"
    )
    sink()
    res
  }, error = function(e) {
    message(paste0("Error running tests for ", pkg, " : ", e$message))
    return(NULL)
  })
  test_results
}

#' @title Create Fail Reporter
#' @description Executes all tests in a given directory using `testthat::test_dir()`
#' and applies context fixes to ensure each test has a valid context.
#'
#' @param test_path Character string specifying the path to the test directory.
#' @param pkg Character string specifying the package name.
#'
#' @return A list of test results with corrected contexts, or `NULL` if an error occurs.
#' @keywords internal
create_fail_reporter <- function(test_path, pkg) {
  message("Creating  reporter for: ", pkg)
  
  rep <- testthat::FailReporter$new()
  
  test_results <- tryCatch({
    testthat::test_dir(
      path = test_path,
      reporter = rep,
      stop_on_failure = FALSE,
      stop_on_warning  = FALSE,
      package = pkg,
      load_package = "source"
    )
    # rep$results contains every expectation recorded
    rep$results
  }, error = function(e) {
    message("Error running tests for ", pkg, " : ", e$message)
    NULL
  })
  
  test_results
}


#' @title Fix Missing Test Contexts
#' @description Ensures that each test result has a valid `context` field.
#' If missing or empty, the context is set to the file name (without extension).
#'
#' @param test_results A list of test result objects from `testthat`.
#'
#' @return A list of test results with updated contexts.
#' @keywords internal
fix_test_context <- function(test_results) {
  lapply(test_results, function(x) {
    if (is.null(x$context) || x$context == "") {
      file_name <- tools::file_path_sans_ext(basename(x$file))
      x$context <- file_name
    }
    x
  })
}


#' @title Run covr with failing tests
#' @description Run \code{\link[covr]{package_coverage}} ignoring failing tests from \code{\link[testthat]{test_dir}}.
#' 
#' @param pkg_source_path Character. path to the package
#' @param pkg Character. Name of the package being tested.
#' @param test_path character, path to test directory, Default: 'tests/testthat'
#' @param test_skip data.frame, mapping of failing tests
#' @param test_map data.frame, mapping of all the tests in the package
#' @param preserve_vars Character. preserve `old` for CRAN examples 
#' @return NULL
#' @details If test_skip is NULL then a new test_dir will be run internally checking
#' for failing tests.
#' @seealso
#'  \code{\link[testthat]{test_dir}}
#'  \code{\link[covr]{package_coverage}}
#' @rdname coverage_skip
#' @family utility
#' @export
#' @importFrom testthat test_dir
#' @importFrom covr package_coverage environment_coverage
#' @importFrom stats setNames
create_coverage_skip_stf <- function(
    pkg_source_path,
    pkg,
    test_path = "tests/testthat",
    test_skip,
    test_map,
    preserve_vars = "old"
) {
  
  message(paste0("Setting up coverage environment for : ", pkg))
  
  # Setup coverage environment
  covr_setup <- setup_covr_env(pkg_source_path, pkg, test_path)
  cov_env <- covr_setup$cov_env
  datasets_loaded <- covr_setup$datasets_loaded
  root_dir <- covr_setup$root_dir
  initial_cov_env_state <- covr_setup$initial_state
  helpers_loaded <- covr_setup$helpers_loaded
  
  
  on.exit({
    message(paste0("Cleaning up environment for ", pkg))
    cleanup_and_return_null(env = cov_env, preserve_vars = preserve_vars)
  }, add = TRUE)
  
  
  # Report loaded helpers
  if (length(helpers_loaded) > 0) {
    message("Helper files loaded:")
    for (helper in helpers_loaded) {
      message("  - ", helper)
    }
  } else {
    message("No helper files were loaded.")
  }
  
  # Identify lines to skip
  message(paste0("Identifying test lines to skip for : ", pkg))
  test_skip_lines <- lapply(
    split(test_skip, test_skip$file),
    function(x) {
      unlist(mapply(seq, from = x$line1, to = x$line2, SIMPLIFY = FALSE))
    }
  )
  
  # Prepare test files
  message(paste0("Preparing test files to check for : ", pkg))
  if (length(test_skip_lines) > 0) {
    test_files <- file.path(test_path, names(test_skip_lines))
    
    on.exit({
      for (next_line in names(test_skip_lines)) {
        file_path <- file.path(test_path, next_line)
        test_lines <- readLines(file_path, warn = FALSE)
        lines_to_uncomment <- test_skip_lines[[next_line]]
        test_lines[lines_to_uncomment] <- gsub("^#", "", test_lines[lines_to_uncomment])
        cat(test_lines, file = file_path, sep = "\n")
      }
    }, add = TRUE)
    
    for (next_line in names(test_skip_lines)) {
      file_path <- file.path(test_path, next_line)
      test_lines <- readLines(file_path, warn = FALSE)
      lines_to_comment <- test_skip_lines[[next_line]]
      test_lines[lines_to_comment] <- sprintf("#%s", test_lines[lines_to_comment])
      cat(test_lines, file = file_path, sep = "\n")
    }
  } else {
    test_files <- list.files(test_path, full.names = TRUE)
  }
  
  # Run coverage
  message(paste0("Running environment coverage for ", pkg))
  cvr <- tryCatch({
    withr::with_dir(root_dir, {
      cov <- covr::environment_coverage(env = cov_env, test_files = test_files)
      # clean problematic entries (avoid devtools vapply bug)
      cov <- Filter(function(x) length(c(x$srcref, x$value)) == 9, cov)
      cov
    })
  }, error = function(e) {
    NULL
  })
  
  # Clean up newly introduced globals
  message(paste0("Removing new globals from environment for ", pkg))
  remove_new_globals(env = cov_env, initial_state = initial_cov_env_state)
  
  # Final cleanup
  
  # cleanup_and_return_null(env = cov_env)
  
  return(cvr)
}



#' Create a results summary for easier access to scores
#' 
#' @description - retrieve overall percentage and file coverage percentages
#' 
#' @param cvr - covr object with full results of skip coverage
#'
#' @return - dataframe with total scores
#' @keywords internal
#' @importFrom dplyr %>%
create_results_summary <- function(cvr) {
  
  # check for empty cvr object
  if (is.null(cvr)) {
    # create filecoverage object
    filecoverage <- structure(
      c(NA_real_),
      dim = c(1),
      dimnames = list("no_coverage.R")
    )
    # create covr_list object
    covr_list <- list(
      total_cov = NA,
      res_cov = list(
        name = NA,
        coverage = list(
          filecoverage = filecoverage,
          totalcoverage = NA
        ),
        errors = NA,
        notes = NA
      )
    )
    
  } else {
    # Extract package name
    pkg <- attr(cvr, "package")$package
    # Extract overall coverage percentage
    total_cov <- covr::percent_coverage(cvr)
    
    # convert percentage into a score between 0 and 1
    total_cov <- total_cov / 100
    
    
    # Aggregate coverage by base filename
    file_coverage <- lapply(names(cvr), function(srcref_name) {
      entries <- cvr[[srcref_name]]
      values <- entries$value
      base_file <- sub(":.*", "", srcref_name)  # Extract base filename before first colon
      data.frame(filename = base_file, covered = values > 0)
    })
    
    file_coverage_df <- do.call(rbind, file_coverage)
    
    # Summarize coverage per base file
    file_summary <- file_coverage_df %>%
      dplyr::group_by(filename) %>%
      dplyr::summarise(
        percent = round(100 * sum(covered) / length(covered), 2),
        .groups = "drop"
      )
    
    # Create 1D numeric vector with dimnames (correct structure)
    filecoverage <- setNames(file_summary$percent, file_summary$filename)
    filecoverage <- structure(
      filecoverage,
      dim = c(length(filecoverage)),
      dimnames = list(names(filecoverage))
    )
    
    # Build result structure
    covr_list <- list(
      total_cov = total_cov,
      res_cov = list(
        name = pkg,
        coverage = list(
          filecoverage = filecoverage,
          totalcoverage = total_cov
        ),
        errors = NA,
        notes = NA
      )
    )
  }
  return(covr_list)
}


#' create covr_list when no tests are skipped
#'
#' @param test_map - R object with mapped tests
#' @param test_results_long - cvr object with no tests skipped
#' @param pkg_source_path - path to the package
#' @param pkg - name of the package
#' @param preserve_vars Character. preserve `old` for CRAN examples 
#'
#' @returns - summary cvr object
#' @keywords internal
#' @importFrom utils data
create_covr_list_no_skip <- function(test_map, 
                                     test_results_long, 
                                     pkg_source_path, 
                                     pkg,
                                     preserve_vars = "old"
                                     ) {
  
  # Attempt to unload package safely
  safe_unload_package(pkg)
  
  # Create environment for coverage
  cov_env <- new.env(parent = globalenv())
  
  # Load package into cov_env
  load_package_into_env(pkg_source_path, pkg, cov_env)
  
  # Detect datasets used in test files
  available_datasets <- data(package = pkg)$results[, "Item"]
  
  test_files <- unique(file.path(pkg_source_path, "tests", "testthat", test_map$file))
  
  # Validate file paths
  valid_test_files <- Filter(function(f) {
    is.character(f) && length(f) == 1 && file.exists(f)
  }, test_files)
  
  
  used_datasets <- unique(unlist(lapply(valid_test_files, function(file) {
    lines <- readLines(file, warn = FALSE)
    matches <- grep(paste(available_datasets, collapse = "|"), lines, value = TRUE)
    regmatches(matches, gregexpr(paste(available_datasets, collapse = "|"), matches))
  })))
  
  
  # Flatten and clean dataset names
  used_datasets <- unique(unlist(used_datasets))
  used_datasets <- used_datasets[used_datasets %in% available_datasets]
  
  # Load datasets if needed
  if (length(used_datasets) > 0) {
    message(paste0("Loading datasets used in testing: ", paste(used_datasets, collapse = ", ")))
    for (ds in used_datasets) {
      tryCatch({
        data(list = ds, package = pkg, envir = cov_env)
      }, error = function(e) {
        message(paste0("Failed to load dataset ", ds, " : ", e$message))
      })
    }
    
    message(paste0("Running coverage manually for ", pkg))
    
    coverage <- tryCatch({
      result <- covr::environment_coverage(
        env = cov_env,
        test_files = test_files
      )
      rm(list = ls(envir = cov_env), envir = cov_env)
      # clean problematic entries (avoid devtools vapply bug)
      result <- Filter(function(x) length(c(x$srcref, x$value)) == 9, result)
      result
    }, error = function(e) {
      cleanup_and_return_null(paste0("Error running coverage for ", pkg, " : ", e$message), env = cov_env)
    })
    
  } else {
    message(paste0("Creating package coverage for ", pkg))
    # unload package before running package_coverage
    try(unloadNamespace(pkg), silent = TRUE)
    coverage <- covr::package_coverage(path = pkg_source_path, type = "tests")
    rm(list = ls(envir = cov_env), envir = cov_env)
  }
  
  # Enrich each test entry with passed, failed, and skipped counts
  test_results_long <- lapply(test_results_long, function(entry) {
    results <- entry$results
    entry$passed  <- sum(vapply(results, inherits, logical(1), "expectation_success"))
    entry$failed  <- sum(vapply(results, inherits, logical(1), "expectation_failure"))
    entry$skipped <- sum(vapply(results, inherits, logical(1), "expectation_skip"))
    return(entry)
  })
  
  if (!is.null(coverage)) {
    # Extract coverage list
    coverage_list <- covr::coverage_to_list(coverage)
    filecoverage <- coverage_list$filecoverage
    totalcoverage <- coverage_list$totalcoverage
    
    total_passed <- sum(sapply(test_results_long, function(entry) entry$passed))
    total_failed <- sum(sapply(test_results_long, function(entry) entry$failed))
    tests_skipped <- Filter(function(entry) entry$skipped > 0, test_results_long)
    
    res_cov <- list(
      name = pkg,
      coverage = list(
        filecoverage = filecoverage,
        totalcoverage = totalcoverage
      ),
      errors = NA,
      notes = NA,
      passed = total_passed,
      failed = total_failed
    )
    
    total_cov <- if (!is.na(totalcoverage)) totalcoverage / 100 else NA_real_
    
    
    covr_list <- list(
      total_cov = total_cov,
      res_cov = res_cov,
      tests_skipped = tests_skipped
    )
   } else {
      message("No test coverage for this configuration")
      covr_list <- list(
        total_cov = 0,
        res_cov = list(
          name = pkg,
          coverage = list(
            filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
            totalcoverage = 0
          ),
          errors = "No testthat or testit configuration",
          notes = NA
        )
      )
    } 
    
  # Remove yarn dataset if created globally
  if (exists("yarn", envir = .GlobalEnv)) {
    rm(yarn, envir = .GlobalEnv)
  }
  
  cleanup_and_return_null(env = globalenv(), preserve_vars = preserve_vars)
  
  return(covr_list)
}

#' Determine the appropriate standard test path for a package
#'
#' This internal utility function inspects the `test_pkg_data` structure to identify
#' which testing framework is used in the package and returns the corresponding test path.
#'
#' @param test_pkg_data A named list containing logical flags such as `has_testthat`, `has_testit`, `has_tests_base`, etc.
#' @param testdir A character string specifying the root directory where test folders are located.
#'
#' @return A character string representing the path to the test directory, or `NULL` if no known test framework is found.
#'
#' @keywords internal
get_stf_test_path <- function(test_pkg_data, testdir) {
  if (isTRUE(test_pkg_data$has_testthat)) {
    return(file.path(testdir, "testthat"))
  } else if (isTRUE(test_pkg_data$has_testit)) {
    return(file.path(testdir, "testit"))
  } else if (
    dir.exists(file.path(testdir, "test-ci")) &&
    dir.exists(file.path(testdir, "test-cran"))
  ) {
    return(testdir)  # fallback for nonstandard testit
  } else if (isTRUE(test_pkg_data$has_tests_base)) {
    return(testdir)  # base R scripts
  }
  
  return(NULL)
}