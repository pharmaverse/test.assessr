#' Run Coverage and Return Structured Coverage Results
#'
#' This function executes code coverage analysis for a package using the
#' `covr` framework. It is typically used after a package has been installed
#' locally and test files are available. The function runs coverage in an
#' isolated process (using `callr`) and returns a structured summary of
#' overall and file‑level coverage.
#'
#' @param pkg_source_path Character. Path to the installed package directory
#'   from which coverage should be computed.
#' @param timeout Numeric. Timeout (in seconds) passed to
#'   \code{callr::r_safe()} when running coverage. This limits the maximum
#'   time allowed for executing the tests underlying the coverage analysis.
#'
#' @details
#' The function invokes covr’s coverage evaluation in a clean R session and
#' extracts both:
#' \itemize{
#'   \item total coverage: percentage of lines covered across the package, and
#'   \item function/file‑level coverage: coverage data for individual files.
#' }
#'
#' It is used internally by higher‑level functions such as
#' \code{run_covr_modes()} and skip‑aware coverage wrappers in the Standard
#' Testing Framework (STF).
#'
#' @return
#' A named list containing:
#' \describe{
#'   \item{total_cov}{Numeric. Aggregated coverage percentage for the package.}
#'   \item{res_cov}{A list containing file‑level or function‑level coverage
#'         results as returned by `covr`. This includes per‑file coverage,
#'         errors, and diagnostic notes if present.}
#' }
#'
#' Returns \code{NULL} if coverage could not be computed.
#'
#' @examples
#' \donttest{
#' # Save and adjust CRAN mirror for reproducibility
#' r <- getOption("repos")
#' old <- options(repos = r)
#' r["CRAN"] <- "http://cran.us.r-project.org"
#' options(repos = r)
#'
#' # Example package from test.assessr
#' dp <- system.file(
#'   "test-data",
#'   "test.package.0001_0.1.0.tar.gz",
#'   package = "test.assessr"
#' )
#'
#' # Set up package source directory
#' install_list <- set_up_pkg(dp)
#' pkg_source_path <- install_list$pkg_source_path
#'
#' # Install locally to enable testing and coverage
#' package_installed <- install_package_local(pkg_source_path)
#'
#' if (isTRUE(package_installed)) {
#'   coverage_results <- run_coverage(pkg_source_path)
#' }
#'
#' # Restore user's original repository settings
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


#' Run covr in a Subprocess with a Timeout
#'
#' This internal helper runs coverage evaluation in a separate R subprocess
#' using `callr`. It allows coverage to be computed with a specified timeout,
#' ensuring that long‑running or hanging test processes do not block further
#' analysis.
#'
#' @param path Character. Path to the package source directory for which
#'   coverage should be computed.
#' @param timeout Numeric. Maximum time (in seconds) allowed for the coverage
#'   computation. Defaults to \code{Inf}, meaning no timeout is applied unless
#'   explicitly set.
#'
#' @details
#' The function launches a safe R session via \code{callr::r_safe()} and
#' executes the relevant covr routines within that isolated environment.
#' This is used internally by higher‑level STF/NSTF coverage functions to
#' ensure reproducible and controlled coverage execution.
#'
#' @return
#' A coverage object returned by the underlying covr call, or \code{NULL} if
#' the subprocess execution fails or times out.
#'
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
