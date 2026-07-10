#' get package test coverage 
#'  
#' @description simplified input to assess package for test coverage
#' @param path (optional) path to locally stored package source code. If
#'   \code{package_installed = TRUE}, \code{path} must point to the unpacked
#'   local package directory (the package root containing files such as
#'   \code{DESCRIPTION}), not to a source tarball such as \code{.tar.gz}.
#' @param package_installed (optional) logical flag that controls local
#'   package installation. If \code{TRUE}, \code{install_package_local} is
#'   skipped and \code{path} is used directly. If \code{NULL} or
#'   \code{FALSE}, a local package installation is attempted.
#' 
#' @return 
#' An object of class \code{"coverage"} as produced by 
#' \code{test.assessr::run_covr_modes()}.  
#' This object is a structured list containing detailed test coverage
#' information, including:
#' \itemize{
#'   \item \strong{File-level coverage:} percentage of lines covered in each file.
#'   \item \strong{Line-by-line coverage:} number of executions for each line.
#'   \item \strong{Overall coverage:} aggregated coverage across the package.
#'   \item \strong{Metadata:} source file paths, expressions, and summary stats.
#' }
#'
#' The resulting object can be printed, summarized, or passed to 
#' \code{test.assessr::generate_test_report()} to produce a human-readable test
#' coverage report.
#'
#' Returns \code{NULL} if the package cannot be installed, if
#' \code{install_list$pkg_source_path} is missing/empty, or if the specified
#' \code{path} does not exist. 
#' 
#' @examples
#' \donttest{
#' # Example STF package included in test.assessr
#' pkg_source_path <- system.file(
#'   "test-data",
#'   "test.package.0001_0.1.0.tar.gz",
#'   package = "test.assessr"
#' )
#'
#' # Run get_package_coverage
#' pkg_test_coverage <- get_package_coverage(pkg_source_path)
#' }
#' @export
get_package_coverage <-function(path = NULL, package_installed = NULL) {
  
  # record covr tests (temporarily set and restore on exit)
  old_covr_record_tests <- getOption("covr.record_tests")
  options(covr.record_tests = TRUE)
  on.exit(
    {
      # If the option was previously unset (NULL), remove it again;
      # otherwise, restore the original value.
      if (is.null(old_covr_record_tests)) {
        options(covr.record_tests = NULL)
      } else {
        options(covr.record_tests = old_covr_record_tests)
      }
    },
    add = TRUE
  )
  
  # save and set user's current working directory
  oldwd <- getwd()  
  on.exit(setwd(oldwd))
  
  # --- Temporarily set CRAN repo (restore immediately on exit) ---
  old_repos <- getOption("repos")
  tmp_repos <- old_repos
  tmp_repos["CRAN"] <- "http://cran.us.r-project.org"
  options(repos = tmp_repos)
  # Ensure options are restored even if the function errors
  on.exit(options(repos = old_repos), add = TRUE)
 
  # Baseline of the user's workspace, captured before any tests run. Package
  # tests can load objects into .GlobalEnv (e.g. data(trees, package =
  # "datasets") in RcppArmadillo's test_fastLm.R, whose default envir is
  # .GlobalEnv). These are removed after the run via remove_new_globals(), which
  # only drops newly-added names, so pre-existing user objects are preserved.
  global_baseline <- ls(envir = globalenv(), all.names = TRUE)
  
  pkg_source_path <- if (is.null(path)) file.choose() else path
  
  if (!file.exists(pkg_source_path)) {
    warning(paste("The specified path", pkg_source_path, "does not exist. Returning NULL."))
    return(NULL)
  }
  
  if (!isTRUE(package_installed)) {
    setup_result <- set_up_pkg(pkg_source_path)
    pkg_source_path <- setup_result$pkg_source_path
    
    if (is.null(pkg_source_path) || !nzchar(pkg_source_path)) {
      warning(paste("`pkg_source_path` is missing after setup. Returning NULL."))
      return(NULL)
    }
    package_installed <- install_package_local(pkg_source_path)
  } 
  
  # Check if the package was installed successfully
  if (package_installed == TRUE) {
    # Assess the package
    package_coverage <- run_covr_modes(pkg_source_path)
    # Output the assessment result
  } else {
    message("Package installation failed.")
    package_coverage <- NULL
  }
  
  #clean up cov env
  cleanup_and_return_null(env = cov_env)
  
  # Drop datasets/objects that package tests loaded into .GlobalEnv during the
  # run, while preserving the returned coverage object and the user's
  # pre-existing workspace.
  suppressMessages(
    remove_new_globals(env = globalenv(), initial_state = global_baseline)
  )
  
  return(package_coverage)
} 
