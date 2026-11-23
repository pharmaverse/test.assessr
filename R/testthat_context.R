#' List namespaces flagged as loaded by pkgload
#'
#' Returns loaded namespaces that pkgload marks as development packages.
#'
#' @return Character vector of namespace names, possibly empty.
#' @noRd
get_dev_namespaces <- function() {
  ns <- loadedNamespaces()
  is_dev <- vapply(
    ns,
    function(x) !is.null(pkgload::dev_meta(x)),
    logical(1),
    USE.NAMES = FALSE
  )
  out <- sort(ns[is_dev])
  return(out)
}

#' Read the testthat edition of a package source
#'
#' Reads Config/testthat/edition from DESCRIPTION; defaults to 2.
#'
#' @param pkg_source_path Path to the package source directory.
#'
#' @return Integer testthat edition, 2L when absent or invalid.
#' @noRd
get_testthat_edition <- function(pkg_source_path) {
  checkmate::assert_string(pkg_source_path, .var.name = "pkg_source_path")
  desc_path <- file.path(pkg_source_path, "DESCRIPTION")
  raw <- if (file.exists(desc_path)) {
    tryCatch(
      read.dcf(desc_path, fields = "Config/testthat/edition")[1, 1],
      error = function(e) NA_character_
    )
  } else {
    NA_character_
  }
  edition <- suppressWarnings(as.integer(raw))
  if (is.na(edition)) {
    edition <- 2L
  }
  return(edition)
}

#' Copy a tests directory into a workspace
#'
#' Copies the whole tests/testthat tree and optionally keeps only chosen test files.
#'
#' @param src_dir Source tests directory.
#' @param dest_dir Destination directory, created if missing.
#' @param test_files Optional test file names to keep; NULL keeps all.
#'
#' @return Destination path, or NULL if copying failed.
#' @noRd
prepare_tests_dir <- function(src_dir, dest_dir, test_files = NULL) {
  checkmate::assert_directory_exists(src_dir, .var.name = "src_dir")
  checkmate::assert_string(dest_dir, .var.name = "dest_dir")
  checkmate::assert_character(test_files, null.ok = TRUE, .var.name = "test_files")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  entries <- list.files(
    src_dir,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  copied <- file.copy(entries, dest_dir, recursive = TRUE, overwrite = TRUE)
  out <- dest_dir
  if (length(copied) > 0 && !all(copied)) {
    message("Failed to copy some test files to ", dest_dir)
    out <- NULL
  }
  if (!is.null(out) && !is.null(test_files)) {
    all_tests <- list.files(dest_dir, pattern = "^test.*\\.[rR]$")
    unlink(file.path(dest_dir, setdiff(all_tests, basename(test_files))))
  }
  return(out)
}

#' Run covr environment coverage through testthat
#'
#' Runs test_dir inside environment_coverage so testthat handles helpers, setup, teardown and failing tests.
#'
#' @param pkg Package name, non-empty string.
#' @param env Environment holding the package code to trace.
#' @param tests_dir Prepared tests directory to run.
#' @param pkg_source_path Package source path, used to read the testthat edition.
#'
#' @return Coverage object with attributes n_failed and n_error; NULL if no coverage.
#' @noRd
run_environment_coverage <- function(pkg, env, tests_dir, pkg_source_path) {
  checkmate::assert_string(pkg, min.chars = 1, .var.name = "pkg")
  checkmate::assert_environment(env, .var.name = "env")
  checkmate::assert_directory_exists(tests_dir, .var.name = "tests_dir")
  checkmate::assert_string(pkg_source_path, .var.name = "pkg_source_path")
  cfg_name <- ".covr_cfg"
  res_name <- ".covr_results"
  assign(cfg_name, list(dir = tests_dir, pkg = pkg, env = env), envir = env)
  wrapper <- tempfile(pattern = "covr-wrapper-", fileext = ".R")
  on.exit({
    unlink(wrapper)
    rm(
      list = intersect(c(cfg_name, res_name), ls(envir = env, all.names = TRUE)),
      envir = env
    )
  }, add = TRUE)
  writeLines(c(
    sprintf("cfg <- get('%s', envir = parent.env(environment()))", cfg_name),
    "res <- testthat::test_dir(",
    "  cfg$dir, package = cfg$pkg, load_package = 'none', env = cfg$env,",
    "  reporter = 'silent', stop_on_failure = FALSE, stop_on_warning = FALSE",
    ")",
    sprintf("assign('%s', res, envir = cfg$env)", res_name)
  ), wrapper)
  cov <- withr::with_envvar(
    c(
      TESTTHAT_PARALLEL = "false",
      TESTTHAT_EDITION = as.character(get_testthat_edition(pkg_source_path))
    ),
    covr::environment_coverage(env = env, test_files = wrapper)
  )
  res <- get0(res_name, envir = env, inherits = FALSE)
  df <- if (is.null(res)) NULL else as.data.frame(res)
  if (!is.null(cov)) {
    attr(cov, "n_failed") <- if (is.null(df)) NA_integer_ else as.integer(sum(df$failed))
    attr(cov, "n_error") <- if (is.null(df)) NA_integer_ else as.integer(sum(df$error))
  }
  return(cov)
}
