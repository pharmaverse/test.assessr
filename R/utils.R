#' Read Description file and parse the package name and version
#'
#' @param pkg_source_path path to package source code (untarred)
#' @param fields - select specified elements from description
#'
#' @return list with package description
#' @keywords internal
get_pkg_desc <- function(pkg_source_path, fields = NULL){
  
  pkg_desc_path <- file.path(pkg_source_path, "DESCRIPTION")
  
  desc_file <- read.dcf(pkg_desc_path, fields = fields)[1L,]
  pkg_desc <- as.list(desc_file)
  
  return(pkg_desc)
}

#' Get test metadata
#'
#' @param executor - user who executes the riskmetrics process 
#' 
#' adapted from mrgvalprep::get_sys_info() and mpn.scorecard
#' @importFrom rlang %||%
#' 
#' @return list with metadata
#' @keywords internal
get_test_metadata <- function(executor = NULL) {
  checkmate::assert_string(executor, null.ok = TRUE)
  
  metadata <- list(
    datetime = as.character(Sys.time()),
    executor = executor %||% Sys.getenv("USER"),
    info = list()
  )
  
  metadata[["info"]][["sys"]] <- as.list(Sys.info())[c("sysname", "version", "release", "machine")]
  
  return(metadata)
}

#' Get Package Name for Display
#'
#' Extracts a display-friendly package name from either a file path or a
#' filename. The function removes directory components (if present) and then
#' returns the substring up to the first underscore or hyphen. This is useful
#' for converting paths or tarball names into a clean package identifier.
#'
#' @param input_string Character string. A package filename or a path containing
#'   the filename (e.g., \code{"mypkg_1.0.0.tar.gz"} or
#'   \code{"/path/to/mypkg_1.0.0.tar.gz"}).
#'
#' @return
#' A character scalar containing the cleaned package name.  
#'
#' The returned object is always of class \code{character} and corresponds to
#' the portion of the filename before the first underscore or hyphen.
#'
#' @section Output Meaning:
#' The value represents a human‑readable package name extracted from a file
#' path or filename. It does not validate whether the extracted name corresponds
#' to an installed or existing package—only that it conforms to the expected
#' tarball naming convention.
#'
#' @examples
#' \donttest{
#' pkg_source_path <- "/home/user/R/test.package.0001_0.1.0.tar.gz"
#' pkg_disp_1 <- get_pkg_name(pkg_source_path)
#' print(pkg_disp_1)
#' 
#' pkg <- "TxDb.Dmelanogaster.UCSC.dm3.ensGene_3.2.2.tar.gz"
#' pkg_disp_2 <- get_pkg_name(pkg)
#' print(pkg_disp_2)
#' }
#' 
#' @export
get_pkg_name <- function(input_string) {
  
  # check if input string is a file path or filename   
  test_string <-  stringr::str_match(input_string, "/")  
  
  if (any(is.na(test_string)) == FALSE) {
    # extract package name from the last part of the file path
    input_string <- stringr::str_split_i(input_string, "/", -1)  
    
  }
  
  # extract package name
  pkg_disp <- stringr::str_extract(input_string, "[^-|_]+")
  
  return(pkg_disp)
}

#' Extract the Last Two Path Components from a File Path
#'
#' This helper function takes any full file path and extracts only the last
#' two components, such as `"R/add.R"`. It supports both forward slashes (`/`)
#' and backslashes (`\\`) to accommodate Windows, macOS, and Linux paths.
#'
#' Trailing separators are preserved where meaningful (e.g., `"R/"` when the
#' input ends with a slash). Empty path components are removed except when
#' required to detect a trailing separator.
#'
#' @param long_file_name Character string. A full file path using `/` or `\\`
#'   as separators.
#'
#' @return
#' A character scalar containing the last two components of the path joined
#' with a forward slash—for example `"R/add.R"`—or a single component if only
#' one exists. The returned value is always of class \code{character}.
#'
#' @section Output Meaning:
#' This function is intended for display and logging purposes, where only the
#' tail portion of a full file path is meaningful. It does not check for file
#' existence; it simply processes the string supplied by the user.
#'
#' @examples
#' extract_short_path("pkg/R/add.R")
#' extract_short_path("C:\\\\projects\\\\mypkg\\\\R\\\\helper.R")
#'
#' @export
extract_short_path <- function(long_file_name) {
  # Split on both forward and backslashes
  parts <- unlist(strsplit(long_file_name, "[/\\\\]"))
  
  # Drop empty components except when trailing separator matters
  trailing_sep <- grepl("[/\\\\]$", long_file_name)
  parts <- parts[parts != ""]
  
  # Compute final value
  if (length(parts) == 0) {
    parts <- ""
  } else if (length(parts) == 1) {
    parts <- if (trailing_sep) paste0(parts[1], "/") else parts[1]
  } else {
    # If trailing separator, last component is empty → keep slash
    if (trailing_sep) {
      parts <- paste0(parts[length(parts)], "/") # last non-empty component
    } else {
      parts <- paste0(parts[length(parts) - 1], "/", parts[length(parts)])
    }
  }
  
  return(parts)
}
