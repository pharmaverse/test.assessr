#' get test package coverage 
#'  
#' @description simplified input to assess package for test coverage
#' @param path (optional) path of locally stored package source code
#' @return list containing results - list containing package coverage
#' 
#' @examples
#' \dontrun{
#' package_coverage <- get_package_coverage()
#' 
#' package_coverage <- get_package_coverage(path/to/package.tar.gz)
#' }
#' @export
get_package_coverage <-function(path = NULL) {
  
  # save and set user's current working directory
  oldwd <- getwd()  
  on.exit(setwd(oldwd))
  
  # get user chosen file
  pkg_source_path <- if (is.null(path)) file.choose() else path
  
  if (!file.exists(pkg_source_path)) {
    warning(paste("The specified path", pkg_source_path, "does not exist. Returning NULL."))
    return(NULL)
  }
  
  # set CRAN repo to enable running of reverse dependencies
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  options(repos = r)
  
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
  
  #clean up global env
  cleanup_and_return_null(env = .GlobalEnv)
  
  return(package_coverage)
} 