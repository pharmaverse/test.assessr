#' Install a Package from a Local Source Directory
#'
#' Attempts to install an R package from a local source directory using
#' \code{remotes::install_local()}. The function reports on whether installation
#' succeeded, whether the package was already installed, or whether the provided
#' source path does not exist.
#'
#' The display name of the package is derived from the input path using
#' \code{get_pkg_name()}.
#'
#' @param pkg_source_path Character string. Path to the local package source
#'   directory (e.g., an unpacked package or extracted tarball path).
#'
#' @return
#' A logical value indicating whether the package is installed after running
#' the function.  
#'
#' The returned object is always of class \code{logical}:
#' \itemize{
#'   \item \code{TRUE} — The package is already installed or was successfully installed.
#'   \item \code{FALSE} — Installation failed or the path does not exist.
#' }
#'
#' @section Output Meaning:
#' \code{TRUE} does not necessarily imply that the installation occurred during
#' this function call—it may also mean the package was already installed.  
#'
#' \code{FALSE} indicates a failure to install or an invalid path. All diagnostic
#' messages are printed via \code{message()} for user visibility.
#'
#' @examples
#' \dontrun{
#' results <- install_package_local("pkg_source_path")
#' print(results)
#' }
#' @export
install_package_local <- function(pkg_source_path) {
  
  pkg_disp <- get_pkg_name(pkg_source_path)
  message(paste0("installing ", pkg_disp, " locally"))
  
  # Check if the package source path exists
  if (!dir.exists(pkg_source_path)) {
    message(paste0("No such file or directory: ", pkg_source_path))
    package_installed <- FALSE
  } else if (requireNamespace(pkg_disp, quietly = TRUE)) {
    message(paste0(pkg_disp, " is already installed"))
    package_installed <- TRUE
  } else {
    package_installed <- tryCatch(
      {
        remotes::install_local(
          pkg_source_path,
          dependencies = TRUE,
          upgrade = "never",
          force = TRUE,
          quiet = TRUE,
          INSTALL_opts = "--with-keep.source"
        )
        message(paste0(pkg_disp, " installed locally"))
        TRUE
      },
      error = function(cond) {
        message(paste0("Local installation issue is: ", cond))
        message(paste0(pkg_disp, " not installed locally"))
        FALSE
      }
    )
  }
  
  return(package_installed)
}
