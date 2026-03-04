
#' Create Information on Local Package Installation
#'
#' This function unpacks a package tarball using \code{unpack_tarball()} and
#' returns information about whether the unpacked directory exists. It is a
#' lightweight preparatory step used before attempting a local package
#' installation.
#'
#' @param dp Character string. Path to a package tarball or package directory.
#'
#' @return
#' A named list with the following elements:
#' \itemize{
#'   \item \code{package_installed} — Logical. \code{TRUE} if the unpacked
#'         package directory exists, otherwise \code{FALSE}.
#'   \item \code{pkg_source_path} — Character string giving the unpacked
#'         package source directory, or \code{""} if unpacking failed.
#' }
#'
#' The returned object is always a base R \code{list}. It contains no side
#' effects besides calling \code{unpack_tarball()} and checking filesystem paths.
#'
#' @section Output Meaning:
#' A value of \code{package_installed = TRUE} indicates that the unpacked
#' directory exists on disk and can be used for local installation.  
#'
#' \code{package_installed = FALSE} indicates either:
#' \itemize{
#'   \item \code{unpack_tarball()} returned an empty result, or
#'   \item the unpacked directory does not exist on disk.
#' }
#'
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
