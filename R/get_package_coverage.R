#' get package test coverage 
#'  
#' @description simplified input to assess package for test coverage
#' @param path (optional) path of locally stored package source code
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
#' Returns \code{NULL} if the package cannot be installed or if the specified
#' path does not exist.
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
#' get_package_coverage <- get_package_coverage(pkg_source_path)
#' }
#' @export
get_package_coverage <-function(path = NULL) {
  
  # save and set user's current working directory
  oldwd <- getwd()  
  on.exit(setwd(oldwd), add = TRUE)
  
  # get user chosen file
  pkg_source_path <- if (is.null(path)) file.choose() else path
  
  if (!file.exists(pkg_source_path)) {
    warning(paste("The specified path", pkg_source_path, "does not exist. Returning NULL."))
    return(NULL)
  }
  
  # --- Temporarily set CRAN repo (restore immediately on exit) ---
  old_repos <- getOption("repos")
  tmp_repos <- old_repos
  tmp_repos["CRAN"] <- "http://cran.us.r-project.org"
  options(repos = tmp_repos)
  # Ensure options are restored even if the function errors
  on.exit(options(repos = old_repos), add = TRUE)
  
  
  # Set up the package using the temporary file
  install_list <- set_up_pkg(pkg_source_path)
  
  # Extract information from the installation list
  package_installed <- install_list$package_installed
  pkg_source_path <- install_list$pkg_source_path
  
  # check if the package needs to be installed locally
  package_installed <- install_package_local(pkg_source_path)
  
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
  
  return(package_coverage)
} 