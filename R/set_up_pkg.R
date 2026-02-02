#' Creates information on package installation
#'
#' @param dp data path and name for the package.
#'
#' @return list with local package install
#' @examples
#' \dontrun{
#' set_up_pkg(path/to/package, "mypackage")
#' }
#' @export
set_up_pkg <- function(dp) {
  
    suppressWarnings(pkg_source_path <- unpack_tarball(dp))
  
  # check if pkg_source_path exists
  if (length(pkg_source_path) == 0) {
    package_installed <- FALSE
    results <- ""
    pkg_source_path <- ""
    out_dir <- ""
  } else {
    if (fs::file_exists(pkg_source_path)) {
      package_installed <- TRUE
    }
  }
  
  install_list <- list(
    package_installed = package_installed,
    pkg_source_path = pkg_source_path
  )
  return(install_list)
}
