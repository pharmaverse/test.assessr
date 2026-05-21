#' Run Coverage Analysis on data.table Package Source and Tests
#'
#' This function maps source files to test files, checks for skipped/problematic tests,
#' runs the remaining test files manually, calculates coverage, and optionally generates a report.
#'
#' @param pkg_source_path Character string. Path to the root of the package source directory.
#' @param test_pkg_data List. Output from `check_pkg_tests_and_snaps()` indicating test framework presence.
#' @param cov_env Environment that holds the temporary coverage workspace and
#'   shared state for the run.
#'
#'
#' @return returns a list with coverage object and source files with no tests.
#'
#' @keywords internal
run_covr_skip_dt_nstf <- function(pkg_source_path, 
                                  test_pkg_data,
                                  cov_env) {
  
  # --- Input checking (CRAN-safe and explicit) ---
  checkmate::assert_string(pkg_source_path, .var.name = "pkg_source_path")
  checkmate::assert_directory_exists(pkg_source_path, .var.name = "pkg_source_path")
  checkmate::assert_true(!missing(cov_env), .var.name = "cov_env")
  checkmate::assert_environment(cov_env, .var.name = "cov_env")
  
  pkg_name <- get_pkg_name(pkg_source_path)
  
  # 1) Resolve data.table test files (prefers tests/main.R)
  #    tests/main.R -> test.data.table() -> inst/tests/tests.Rraw (big suite)
  #    https://github.com/Rdatatable/data.table/blob/master/tests/main.R
  #    https://rdatatable.gitlab.io/data.table/reference/test.data.table.html
  test_files_clean <- compute_test_files_clean_dt(
    pkg_name        = pkg_name,
    pkg_source_path = pkg_source_path,
    clean_mapping   = NULL,            # mapping not used for dt
    test_pkg_data   = test_pkg_data,
    skip_tests      = NULL
  )
  test_files_clean <- normalizePath(test_files_clean, winslash = "/", mustWork = FALSE)
  
  if (!length(test_files_clean) || !all(file.exists(test_files_clean))) {
    return(cleanup_and_return_null(
      sprintf("No resolvable data.table tests for %s", pkg_name)
    ))
  }
  
  # 2) Load package + datasets into coverage env
  message(sprintf("loading package and datasets for %s", pkg_name))
  tryCatch({
    pkgload::load_all(pkg_source_path)
    
    pkg_all <- ls(envir = asNamespace(pkg_name), all.names = TRUE)
    for (obj in pkg_all) {
      tryCatch({
        assign(obj, get(obj, envir = asNamespace(pkg_name)), envir = cov_env)
      }, error = function(e) {
        message(sprintf("Failed to assign object %s : %s", obj, e$message))
      })
    }
    datasets <- load_package_datasets(pkg_name, env = cov_env)
    if (length(datasets)) message("Datasets loaded: ", paste(datasets, collapse = ", "))
  }, error = function(e) {
    return(cleanup_and_return_null(
      sprintf("Error loading package or datasets for %s : %s", pkg_name, e$message)
    ))
  })
  
  # 3) Ensure runtime layout for test.data.table(): MIRROR inst/tests/* -> tests/ if needed
  #    (CRAN-friendly; no .GlobalEnv mods)
  prep <- prepare_tests_for_package_dt(
    pkg_name        = pkg_name,
    pkg_source_path = pkg_source_path,
    test_files      = test_files_clean,
    mode            = "mirror"
  )
  on.exit(prep$teardown(), add = TRUE)
  
  # 4) Run coverage on the resolved tests
  message(sprintf("running coverage manually for %s", pkg_name))
  coverage <- tryCatch({
    covr::environment_coverage(
      env        = cov_env,
      test_files = test_files_clean
    )
  }, error = function(e) {
    return(cleanup_and_return_null(
      sprintf("Error running coverage for %s : %s", pkg_name, e$message),
      env = cov_env
    ))
  })
  
  if (is.null(coverage)) {
    message("No test coverage for this configuration")
    return(list(
      total_cov = 0,
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage   = matrix(0, nrow = 1, dimnames = list("No functions tested")),
          totalcoverage  = 0
        ),
        errors = "No coverage generated",
        notes  = NA
      ),
      functions_no_tests = data.frame(function_name = character(), file = character()),
      tests_skipped      = character(),
      tests_passing      = character()
    ))
  }
  
  # 5) Convert coverage -> list, gather file coverage summary
  res_cov <- tryCatch({
    coverage_list <- covr::coverage_to_list(coverage)
    list(name = pkg_name, coverage = coverage_list, errors = NA, notes = NA)
  }, error = function(cond) {
    coverage_list <- list(filecoverage = NA, totalcoverage = NA_integer_)
    list(name = pkg_name, coverage = coverage_list, errors = cond, notes = NA)
  })
  
  # 6) Summaries specifically for data.table
  # 6a) tested_files = how many test files executed (tests/main.R => 1)
  tested_files <- length(unique(test_files_clean))
  
  # 6b) skipped_tests = known wrapper scripts present but not selected
  dt_wrapper_candidates <- dt_find_wrapper_tests(pkg_source_path)
  skipped_tests <- setdiff(dt_wrapper_candidates, test_files_clean)
  
  # 6c) untested_functions = functions whose srcref span has zero executed lines
  dt_no_tests_df <- dt_compute_untested_functions(pkg_name, coverage)
  
  # 7) Aggregate totals with your existing coverage combiner
  percent_cov <- as.numeric(res_cov$coverage$totalcoverage)
  calc_covr_list <- compute_total_coverage(
    percent_cov, 
    dt_no_tests_df,         # functions_no_tests_df
    test_files_clean,       # tests_passing
    skipped_tests           # tests_skipped
  )
  
  total_cov           <- calc_covr_list$total_cov
  percent_cov_round   <- round(total_cov, 2)
  n_testfiles         <- calc_covr_list$n_testfiles
  n_no_function_tests <- calc_covr_list$n_no_function_tests
  n_skipped_files     <- calc_covr_list$n_skipped_files
  
  message(sprintf("Total Coverage for %s is: %.2f%%", pkg_name, percent_cov_round))
  message(sprintf(
    "Approx total coverage: (%.2f%%), counts: tested_files=%d, untested_functions=%d, skipped_tests=%d",
    calc_covr_list$percent_cov, n_testfiles, n_no_function_tests, n_skipped_files
  ))
  
  cleanup_and_return_null(env = cov_env)
  
  list(
    total_cov = total_cov,
    res_cov = list(
      name     = res_cov$name,
      coverage = res_cov$coverage,  # contains $filecoverage and $totalcoverage
      errors   = res_cov$errors,
      notes    = res_cov$notes
    ),
    functions_no_tests = dt_no_tests_df,
    tests_skipped      = skipped_tests,
    tests_passing      = test_files_clean
  )
}




#' Locate data.table wrapper test scripts (internal)
#'
#' Returns absolute, normalized paths to known wrapper test scripts under
#' \code{tests/} for a data.table source checkout. This is used to determine
#' which wrapper files exist locally so that callers can distinguish between
#' the canonical entrypoint \code{tests/main.R} and other wrapper scripts
#' (e.g., \code{froll.R}, \code{programming.R}, etc.).
#'
#' @param pkg_source_path character(1). Path to the package source root that
#'   contains the \code{tests/} directory.
#'
#' @return character(). A vector of existing files among the known candidates,
#'   with absolute paths normalized via \code{normalizePath(..., winslash = "/")}.
#'   The vector is empty if none of the candidates exist.
#'
#' @section Notes:
#' The candidate list is intentionally small and stable. If the upstream
#' repository adds additional wrappers, extend this function’s list.
#'
#'
#' @keywords internal
dt_find_wrapper_tests <- function(pkg_source_path) {
  tests_dir  <- file.path(pkg_source_path, "tests")
  candidates <- file.path(tests_dir, c(
    "main.R", "froll.R", "programming.R", "types.R",
    "nafill.R", "other.R", "S4.R", "autoprint.R"
  ))
  normalizePath(candidates[file.exists(candidates)], winslash = "/", mustWork = FALSE)
}


#' Compute untested functions from coverage using srcref spans (internal)
#'
#' Identifies package functions that were not executed by the provided coverage
#' object. A function is considered \emph{untested} if \strong{no} line within
#' its \code{srcref} span (from \code{start_line} to \code{end_line}) has a
#' positive execution count in \code{covr::tally_coverage()}.
#'
#' @param pkg_name character(1). The package name whose namespace is inspected
#'   for functions (\code{asNamespace(pkg_name)}).
#' @param coverage A coverage object as returned by \code{covr} (e.g.,
#'   \code{covr::environment_coverage()} or \code{covr::package_coverage()}),
#'   consumable by \code{covr::tally_coverage()}.
#'
#' @return \link[base:data.frame]{data.frame} with one row per untested function
#'   and columns:
#'   \describe{
#'     \item{function_name}{Function symbol in the package namespace.}
#'     \item{file}{Absolute, normalized path to the source file containing the function.}
#'     \item{start_line}{Start line (inclusive) from the function's \code{srcref}.}
#'     \item{end_line}{End line (inclusive) from the function's \code{srcref}.}
#'   }
#'   Returns an empty data.frame with these columns if (a) the coverage table is
#'   empty, (b) no functions have an \code{srcref}, or (c) all functions had at
#'   least one executed line within their span.
#'
#' @section Details:
#' \itemize{
#'   \item Functions without an \code{srcref} are skipped.
#'   \item Files are matched by \emph{basename} between the coverage table and the
#'         function’s \code{srcref} source path to make the join resilient to
#'         differing absolute paths in temporary build trees.
#'   \item A line is considered executed when its coverage \code{value} is a finite
#'         number greater than zero.
#' }
#'
#' @seealso \code{\link[covr]{tally_coverage}}
#' @keywords internal
dt_compute_untested_functions <- function(pkg_name, coverage) {
  tab <- covr::tally_coverage(coverage)
  if (is.null(tab) || !nrow(tab)) {
    return(data.frame(function_name = character(), file = character(),
                      start_line = integer(), end_line = integer(),
                      stringsAsFactors = FALSE))
  }
  tab$file_base <- basename(tab$filename)
  
  ns   <- asNamespace(pkg_name)
  objs <- ls(ns, all.names = TRUE)
  
  out <- lapply(objs, function(nm) {
    obj <- tryCatch(get(nm, envir = ns), error = function(e) NULL)
    if (!is.function(obj)) return(NULL)
    sref <- attr(obj, "srcref")
    if (is.null(sref)) return(NULL)
    
    srcfile <- attr(sref, "srcfile")
    fpath   <- if (!is.null(srcfile) && !is.null(srcfile$filename)) srcfile$filename else NA_character_
    if (is.na(fpath)) return(NULL)
    
    data.frame(
      function_name = nm,
      file          = normalizePath(fpath, winslash = "/", mustWork = FALSE),
      file_base     = basename(fpath),
      start_line    = sref[[1]],  # start line
      end_line      = sref[[3]],  # end line
      stringsAsFactors = FALSE
    )
  })
  fun_df <- do.call(rbind, out)
  if (is.null(fun_df) || !nrow(fun_df)) {
    return(data.frame(function_name = character(), file = character(),
                      start_line = integer(), end_line = integer(),
                      stringsAsFactors = FALSE))
  }
  
  fun_df$tested <- FALSE
  for (i in seq_len(nrow(fun_df))) {
    sub <- tab[tab$file_base == fun_df$file_base[i] &
                 tab$line >= fun_df$start_line[i] &
                 tab$line <= fun_df$end_line[i], , drop = FALSE]
    # 'value' = hit count; treat missing/NA as zero
    fun_df$tested[i] <- any(is.finite(sub$value) & sub$value > 0)
  }
  
  fun_df[!fun_df$tested, c("function_name", "file", "start_line", "end_line")]
}



#' Prepare package-specific test environment for data.table (internal)
#'
#' For data.table, Only "mirror" mode is supported here: 
#' copy inst/tests/* -> tests/ and clean up.
#'
#' Returns a list with:
#'   - test_files: character() of wrapper files to execute
#'   - teardown:   function() to revert any changes (delete mirrored files, unset env)
#'
#' @keywords internal
prepare_tests_for_package_dt <- function(pkg_name,
                                      pkg_source_path,
                                      test_files,
                                      mode = c("mirror")) {
  mode <- match.arg(mode)
  teardown <- function() {}
  tests_to_run <- test_files
  
  if (identical(pkg_name, "data.table")) {
    inst_tests_dir <- file.path(pkg_source_path, "inst", "tests")
    tests_dir      <- file.path(pkg_source_path, "tests")
    main           <- file.path(tests_dir, "main.R")
    raw_in_inst    <- file.path(inst_tests_dir, "tests.Rraw")
    raw_in_tests   <- file.path(tests_dir, "tests.Rraw")
    
    created <- character(0)
    
    # If tests/tests.Rraw is missing but inst/tests/tests.Rraw exists,
    # mirror top-level files from inst/tests -> tests (non-recursive).
    if (!file.exists(raw_in_tests) && file.exists(raw_in_inst)) {
      if (mode == "mirror") {
        message("data.table: MIRROR mode -> copying inst/tests/* -> tests/ to satisfy test.data.table()")
        dir.create(tests_dir, showWarnings = FALSE, recursive = TRUE)
        inst_files <- list.files(inst_tests_dir, all.files = FALSE, full.names = TRUE, no.. = TRUE)
        
        finfo <- file.info(inst_files)
        inst_files <- inst_files[!is.na(finfo$isdir) & !finfo$isdir]  # files only, top-level
        
        for (src in inst_files) {
          dest <- file.path(tests_dir, basename(src))
          if (!file.exists(dest)) {
            ok <- tryCatch(file.copy(src, dest, overwrite = FALSE), error = function(e) FALSE)
            if (isTRUE(ok)) {
              created <- c(created, normalizePath(dest, winslash = "/", mustWork = FALSE))
            }
          }
        }
      } else {
        message("data.table: MIRROR disabled; tests/tests.Rraw missing. Full suite may fail.")
      }
    }
    
    # Prefer canonical entrypoint (runs 5,000+ tests)
    # https://github.com/Rdatatable/data.table/blob/master/tests/main.R
    # https://rdatatable.gitlab.io/data.table/reference/test.data.table.html
    if (file.exists(main)) {
      tests_to_run <- normalizePath(main, winslash = "/", mustWork = FALSE)
    } else {
      # If main.R is missing, leave tests_to_run as provided; compute_test_files_clean()
      # will pick wrappers (e.g., froll.R, programming.R, etc.) as a fallback.
      message("data.table: tests/main.R not found; will rely on wrapper selection.")
    }
    
    # Teardown: remove only what we created via MIRROR
    teardown <- function() {
      if (length(created)) {
        try(unlink(created, force = TRUE), silent = TRUE)
      }
    }
  }
  
  list(test_files = tests_to_run, teardown = teardown)
}




#' Compute wrapper data.table test files to run for coverage (internal)
#'
#' Selects and sanitizes the list of **wrapper** test files to execute during
#' coverage, supporting both the standard `tests/` layout and the `testit`
#' layout (`tests/testit/`). When `pkg_name` is `"data.table"`, the function
#' adds package-aware behavior: it falls back to `tests/main.R` if the mapping
#' yields no existing files and optionally prioritizes `froll.R` so rolling
#' function tests run early.
#'
#' This helper **only resolves file paths**; it does **not** modify the
#' evaluation environment, copy/mirror test assets, or toggle data.table's
#' test harness mode. Any package-specific preparation (e.g., mirroring
#' `inst/tests/` to `tests/` or exposing `test.data.table()` in dev mode)
#' must be handled elsewhere (see `prepare_tests_for_package()`).
#'
#' @param pkg_name        `character(1)`. Package name.
#' @param pkg_source_path `character(1)`. Absolute or relative path to the
#'   package source root.
#' @param clean_mapping   A data frame or list containing at least a
#'   `test_file` column/element with **relative** wrapper paths to run.
#'   Paths are resolved against `tests/` or `tests/testit/` depending on
#'   `test_pkg_data$has_testit`.
#' @param test_pkg_data   `list`. Must include logical flag `has_testit`
#'   indicating whether the package uses a `tests/testit/` layout.
#' @param skip_tests      `NULL` or `character()`. Relative wrapper paths to
#'   exclude (matched within the chosen base directory).
#'
#' @return `character()` - **normalized**, de-duplicated vector of wrapper
#'   file paths that **exist** on disk. May be zero length if no candidates
#'   exist and no fallback is available.
#'
#' @keywords internal
compute_test_files_clean_dt <- function(pkg_name,
                                     pkg_source_path,
                                     clean_mapping,
                                     test_pkg_data,
                                     skip_tests = NULL) {
  if (identical(pkg_name, "data.table")) {
    tests_dir <- file.path(pkg_source_path, "tests")
    main      <- file.path(tests_dir, "main.R")
    
    # If main.R exists, use it (this calls test.data.table() -> full suite)
    if (file.exists(main)) {
      return(normalizePath(main, winslash = "/", mustWork = FALSE))
    }
    
    # Otherwise, fall back to wrapper scripts present under tests/.
    # Keep froll.R as first priority (legacy behavior),
    # then add broader wrappers commonly present in data.table/tests:
    # programming.R, types.R, nafill.R, other.R, S4.R
    # (See repo listing for these files.)
    # https://github.com/Rdatatable/data.table/blob/master/tests/
    priority_wrappers <- c("froll.R", "programming.R", "types.R", "nafill.R", "other.R", "S4.R")
    existing <- file.path(tests_dir, priority_wrappers)
    existing <- existing[file.exists(existing)]
    
    # If a mapping was provided, use it but sort by our priority; else use the priority set.
    test_files_col <- clean_mapping$test_file
    mapped <- character()
    if (!is.null(test_files_col)) {
      mapped <- file.path(tests_dir, test_files_col)
      mapped <- mapped[file.exists(mapped)]
    }
    
    # Apply skip list if provided
    if (!is.null(skip_tests) && length(skip_tests) > 0) {
      mapped   <- setdiff(mapped, file.path(tests_dir, skip_tests))
      existing <- setdiff(existing, file.path(tests_dir, skip_tests))
    }
    
    # Merge (mapped first, then priority list), then order by our explicit priority
    candidates <- unique(c(mapped, existing))
    if (!length(candidates)) {
      # As a last-ditch fallback, if someone mirrored and raw exists, try running tests.Rraw directly
      raw <- file.path(tests_dir, "tests.Rraw")
      if (file.exists(raw)) return(normalizePath(raw, winslash = "/", mustWork = FALSE))
      return(character())
    }
    
    # Sort by explicit priority order (keep froll.R first if present)
    nm <- basename(candidates)
    ord <- order(match(nm, priority_wrappers, nomatch = length(priority_wrappers) + seq_along(nm)))
    return(unique(normalizePath(candidates[ord], winslash = "/", mustWork = FALSE)))
  }
  
  ## ---- generic logic for other packages (unchanged) ----
  base_dir <- if (isTRUE(test_pkg_data$has_testit)) {
    file.path(pkg_source_path, "tests", "testit")
  } else {
    file.path(pkg_source_path, "tests")
  }
  
  test_files_col <- clean_mapping$test_file
  if (is.null(test_files_col)) test_files_col <- character()
  candidates <- file.path(base_dir, test_files_col)
  
  if (!is.null(skip_tests) && length(skip_tests) > 0) {
    candidates <- setdiff(candidates, file.path(base_dir, skip_tests))
  }
  
  test_files <- candidates[file.exists(candidates)]
  unique(normalizePath(test_files, winslash = "/", mustWork = FALSE))
  
  return(test_files)
}
