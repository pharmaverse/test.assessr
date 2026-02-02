#' Run covr and potentially save results to disk
#'
#' @param pkg_source_path package installation directory
#' @param timeout Timeout to pass to [callr::r_safe()] when running covr.
#'
#'
#' @return list with total coverage and function coverage
#' @examples
#' \donttest{
#' r <- getOption("repos")
#' # save current repo options
#' old <- options(repos = r)
#' r["CRAN"] = "http://cran.us.r-project.org"
#' options(repos = r)
#'
#' dp <- system.file("test-data", "test.package.0001_0.1.0.tar.gz",
#'                   package = "test.assessr")
#'
#' install_list <- set_up_pkg(dp)
#'
#' package_installed <- install_list$package_installed
#' pkg_source_path <- install_list$pkg_source_path
#'
#' package_installed <- install_package_local(pkg_source_path)
#' package_installed <- TRUE
#'
#' if (package_installed == TRUE) {
#'    test_package <- run_coverage(pkg_source_path)
#' } else {
#'   message("Package installation failed.")
#' }
#' # restore user's repo options
#' options(old)
#' }
#'
#' @export
run_coverage <- function(pkg_source_path, timeout = Inf) {
  
  pkg_name <- basename(pkg_source_path)
  
  message(paste0("running code coverage for ", pkg_name))
  
  # run covr
  res_cov <- tryCatch({
    coverage_list <- run_covr(pkg_source_path, timeout)
    
    # If no testable functions are found in the package, `filecoverage` and `totalcoverage`
    # will yield logical(0) and NaN respectively. Coerce to usable format
    if(is.na(coverage_list$totalcoverage)){
      if(rlang::is_empty(coverage_list$filecoverage) && is.logical(coverage_list$filecoverage)){
        coverage_list$totalcoverage <- 0
        notes <- "no testable functions found"
      }else{
        message("Total coverage returned NaN. This likely means the package had non-standard characteristics.")
        notes <- NA
      }
    }else{
      notes <- NA
    }
    
    list(name = pkg_name, coverage = coverage_list, errors = NA, notes = notes)
  },
  error = function(cond){
    coverage_list <- list(filecoverage = NA, totalcoverage = NA_integer_)
    list(
      name = pkg_name, coverage = coverage_list,
      errors = cond,
      notes = NA
    )
  })
  
  if(is.na(res_cov$coverage$totalcoverage)) {
    message(paste0("code coverage for ", pkg_name, " unsuccessful"))
  } else {
    message(paste0("code coverage for ", pkg_name, " successful"))
  }
  
  # return total coverage as fraction
  total_cov <- as.numeric(res_cov$coverage$totalcoverage/100)
  
  if(is.na(total_cov)){
    message(paste0("R coverage for ", pkg_name, " failed. Read in the covr output to see what went wrong: "))
  }
  
  if(!is.na(res_cov$notes)){
    message(paste0("R coverage for ", pkg_name, " had notes: ", res_cov$notes))
  }
  
  covr_list <- list(
    total_cov = total_cov,
    res_cov = res_cov
  )
  return(covr_list)
}

#' Run covr in subprocess with timeout
#'
#' @param path - path to source file
#' @param timeout - length of timeout - set to Inf
#' @keywords internal
run_covr <- function(path, timeout) {
  callr::r_safe(
    function(p) {
      covr::coverage_to_list(covr::package_coverage(p, type = "tests"))
    },
    args = list(path),
    libpath = .libPaths(),
    repos = NULL,
    package = FALSE,
    user_profile = FALSE,
    error = "error",
    timeout = timeout
  )
}

