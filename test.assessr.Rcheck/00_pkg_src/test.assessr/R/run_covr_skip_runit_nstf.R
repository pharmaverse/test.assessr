#' Run coverage with skip detection for RUnit and Bioconductor RUnit-based packages
#'
#' `run_covr_skip_runit_nstf()` orchestrates coverage for packages using the
#' Bioconductor **RUnit** layout, such as **BiocGenerics** and packages that store
#' tests in `inst/unitTests` or ship a `tests/run_unitTests.R` wrapper.
#'
#' The function:
#' 1. Builds a source-to-test mapping (using your existing `get_source_test_mapping_nstf()`).
#' 2. Removes any `run_unitTests.R` wrapper from the mapping (via `remove_run_unitTests_mapping()`).
#' 3. Prepares a development environment **before** running RUnit tests:
#'    - Detaches commonly attached BioC infrastructure packages (`IRanges`, `S4Vectors`, `BiocGenerics`)
#'      to avoid namespace/shim conflicts.
#'    - Calls `pkgload::load_all()` to load the package under test.
#'    - Clears `R_TESTS` to avoid R CMD check harness interference when running RUnit programmatically.
#' 4. Identifies problem/skip-able tests via your RUnit-aware `check_covr_skip_nstf()` (which should use
#'    `RUnit::runTestFile()` + `RUnit::getErrors()` per file).
#' 5. Builds a list of test files **under `inst/unitTests`** excluding any that should be skipped.
#' 6. Runs coverage with `covr::environment_coverage()`: tries `tests/doRUnit.R` first when
#'    present (matches R CMD check for CRAN RUnit packages); falls back to a custom runner
#'    that calls `RUnit::runTestFile()` on each test file if doRUnit.R fails or is absent.
#'
#' @param pkg_source_path Character scalar. Path to the root of the package source.
#' @param test_pkg_data A list or environment with test metadata (e.g., fields like
#'   `bioc_run_ut_path` (path to `tests/run_unitTests.R`, if present),
#'   `has_BioG_test` (logical flag), etc.).
#' @param cov_env Environment that holds the temporary coverage workspace and
#'   shared state for the run.   
#' @param framework Optional character. When provided (e.g. from
#'   \code{run_one_framework}), restricts test paths to the framework-specific
#'   directory. Use when a package has multiple frameworks (e.g. testthat + testit)
#'   to avoid mixing paths across frameworks.
#'
#' @return A `covr` coverage object on success; `NULL` on failure (via `cleanup_and_return_null()`).
#'
#' @details
#' Use this function **only** for packages that follow the Bioconductor RUnit layout:
#' - tests are located in `inst/unitTests/`, and/or
#' - a `tests/run_unitTests.R` wrapper exists that calls an internal `.test()` / `testPackage()` runner.
#'
#' @return A covr coverage object, or NULL on failure.
#' 
#' @keywords internal
run_covr_skip_runit_nstf <- function(pkg_source_path, 
                                     test_pkg_data, 
                                     cov_env,
                                     framework = NULL
                                     ) {
  
  cov_env  <- new.env(parent = globalenv())
  pkg_name <- get_pkg_name(pkg_source_path)
  
  # Ensure this really looks like a BioC/RUnit package (caller can decide too)
  # Ensure this really looks like an RUnit package (BioC or RUnit-only; caller can decide too)
  if (!is_runit_layout(pkg_source_path, test_pkg_data)) {
    message("Package does not appear to use the RUnit layout (inst/unitTests); ",
            "consider calling run_covr_skip_nstf() instead.")
  }
  
  # Assemble candidate test roots and normalize (reuses your existing helper)
  path    <- as.character(pkg_source_path)
  testdir <- file.path(path, "inst/unitTests")
  test_path_list <- get_runit_test_paths(test_pkg_data, testdir)
  
  # restrict paths 
  test_path_list <- restrict_test_paths_to_framework(test_path_list, framework)
  
  test_path <- normalizePath(
    unlist(test_path_list, use.names = TRUE),
    winslash = "/",
    mustWork = FALSE
  )
  
  message(paste0("performing source test mapping for ", pkg_name))
  mapping <- tryCatch({
    get_source_test_mapping_runit_nstf(pkg_source_path, test_path)
  }, error = function(e) {
    cleanup_and_return_null(paste0("Error in source-test mapping for ", pkg_name, " : ", e$message))
  })
  if (is.null(mapping)) return(NULL)
  
  # Clean out run_unitTests.R (BioC wrapper) if provided
  if (!is.na(test_pkg_data$bioc_run_ut_path)) {
    message(paste0("performing source test map cleaning for ", pkg_name))
    mapping <- tryCatch({
      remove_run_unitTests_mapping(mapping, test_pkg_data)
    }, error = function(e) {
      cleanup_and_return_null(paste0("Error in cleaning mapping for ", pkg_name, " : ", e$message))
    })
    if (is.null(mapping)) return(NULL)
  }
  
  # --- BioC/RUnit preparation: detach (if attached) and load_all() BEFORE running tests ---
  detach_bioc <- isTRUE(test_pkg_data$has_BioG_test)
  
  if (detach_bioc) {
    message("[BioC RUnit] preparing package before running tests...")
  } else {
    message("[RUnit] preparing package before running tests...")
  }  
  
  prep_ok <- tryCatch({
    prepare_for_bioc_runit(pkg_name, pkg_source_path, detach_bioc_infra = detach_bioc)
    TRUE
  }, error = function(e) {
    cleanup_and_return_null(paste0("Error preloading for BioC RUnit (", pkg_name, "): ", e$message))
  })
  if (is.null(prep_ok)) return(NULL)
  
  # Identify problematic/skip-able tests AFTER load_all()
  message(paste0("identifying problem tests for ", pkg_name))
  problems <- tryCatch({
    # Uses your RUnit-aware checker that calls RUnit::runTestFile() per file
    check_covr_skip_runit_nstf(pkg_source_path, mapping, test_path)
  }, error = function(e) {
    cleanup_and_return_null(paste0("Error identifying skipped/problematic tests for ", pkg_name, " : ", e$message))
  })
  
  if (is.null(problems)) return(NULL)
  
  # Decide which tests to skip
  skip_tests <- problems$test_file[
    !is.na(problems$test_file) &
      !startsWith(problems$issue_type, "warning:") &
      problems$issue_type != "No tests skipped"
  ]
  
  # create df with source files with no tests
  no_tests_df <- get_function_no_tests(mapping) 
  
  # Prepare mapping and build test file list from inst/unitTests
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  root <- file.path(pkg_source_path, "inst", "unitTests")
  test_files_clean <- file.path(root, clean_mapping$test_file)
  if (length(skip_tests) > 0) {
    test_files_clean <- setdiff(test_files_clean, file.path(root, skip_tests))
  }
  
  # Populate the coverage environment with exported & internal objects
  prep_env_ok <- tryCatch({
    pkg_all <- ls(envir = asNamespace(pkg_name), all.names = TRUE)
    for (obj in pkg_all) {
      try(assign(obj, get(obj, envir = asNamespace(pkg_name)), envir = cov_env), silent = TRUE)
    }
    datasets <- load_package_datasets(pkg_name, env = cov_env)
    message("Datasets loaded: ", paste(datasets, collapse = ", "))
    TRUE
  }, error = function(e) {
    cleanup_and_return_null(paste0("Error preparing coverage env for ", pkg_name, " : ", e$message))
  })
  if (is.null(prep_env_ok)) return(NULL)
  
  
  message(paste0("running coverage manually for ", pkg_name))
  
  
  
  coverage <- tryCatch({
    
    # 0) Guard: need at least one test file for custom runner fallback
    if (length(test_files_clean) == 0L) {
      stop("No test files provided in 'test_files_clean'.")
    }
    
    # 1) Ensure 'methods' and 'RUnit' are attached (S4 + checks)
    if (!"methods" %in% loadedNamespaces()) {
      loadNamespace("methods")  # does not attach (no library())
    }
    
    # Unload first to avoid 'package in use' issues
    detach_pkg_if_attached(pkg_name)
    
    # Load the target package from SOURCE so covr instruments the package being assessed
    # (not a different installed version)
    suppressPackageStartupMessages(pkgload::load_all(pkg_source_path, quiet = TRUE))
    
    pkg_ns <- asNamespace(pkg_name)
    
    # 2) Try doRUnit.R first when available (matches R CMD check for CRAN RUnit packages)
    cov <- NULL
    doRUnit_path <- test_pkg_data$doRUnit_path
    use_doRUnit <- !is.null(doRUnit_path) && !is.na(doRUnit_path) && file.exists(doRUnit_path)
    
    if (use_doRUnit) {
      message(paste0("Trying doRUnit.R for ", pkg_name))
      cov <- tryCatch({
        doRUnit_abs <- normalizePath(doRUnit_path, winslash = "/", mustWork = TRUE)
        # doRUnit.R expects wd = tests/ so basename(dirname(wd)) = pkg name (R CMD check layout)
        runner <- build_doRUnit_wrapper(doRUnit_abs)
        covr::environment_coverage(env = pkg_ns, test_files = runner)
      }, error = function(e) {
        message(paste0("doRUnit.R failed: ", conditionMessage(e), "; falling back to custom runner"))
        NULL
      })
      # runTests.R uses defineTestSuite default testFuncRegexp = "^test.+" which does not
      # match runit.*.R files (they use runit.* functions). Retry with runTests-style
      # runner that injects testFuncRegexp = "^runit.+".
      if (!is.null(cov) && length(test_files_clean) > 0L) {
        pct <- tryCatch(covr::percent_coverage(cov), error = function(e) NA_real_)
        if (is.numeric(pct) && !is.na(pct) && pct == 0) {
          message(paste0("doRUnit.R produced 0% coverage (runTests.R uses wrong testFuncRegexp); retrying with runTests-style runner and testFuncRegexp = \"^runit.+\""))
          cov <- NULL
        }
      }
    }
    
    # 3) Fall back if doRUnit wasn't used, failed, or produced 0%
    if (is.null(cov)) {
      tests <- normalizePath(test_files_clean, winslash = "/", mustWork = TRUE)
      # Use runTests-style runner when doRUnit existed (correct testFuncRegexp);
      # otherwise custom runner. Both run only test_files_clean (filtered list).
      if (use_doRUnit) {
        message(paste0("Using runTests-style runner for ", pkg_name, " (", length(tests), " filtered test files)"))
        runner <- build_runit_runTests_runner(pkg_name, tests, pkg_root = pkg_source_path)
      } else {
        message(paste0("Using custom runner for ", pkg_name, " (", length(tests), " test files)"))
        runner <- build_runit_custom_runner(tests, pkg_name = pkg_name, pkg_root = pkg_source_path)
      }
      cov <- covr::environment_coverage(
        env        = pkg_ns,
        test_files = runner
      )
    }
    
    # 4) If environment_coverage produced 0%, try package_coverage as fallback
    #    Uses type = "none" and code = runner so we run only filtered tests (not
    #    the full suite). package_coverage installs to temp and runs our runner
    #    in a subprocess, which can succeed when env instrumentation fails.
    if (!is.null(cov)) {
      pct <- tryCatch(covr::percent_coverage(cov), error = function(e) NA_real_)
      if (is.numeric(pct) && !is.na(pct) && pct == 0) {
        message(paste0("environment_coverage produced 0%; retrying with covr::package_coverage(type = \"none\", code = runner)"))
        detach_pkg_if_attached(pkg_name)
        try(unloadNamespace(pkg_name), silent = TRUE)
        runner_lines <- readLines(runner, warn = FALSE)
        pkg_cov <- tryCatch({
          covr::package_coverage(
            path = pkg_source_path,
            type = "none",
            code = runner_lines,
            quiet = TRUE
          )
        }, error = function(e) {
          message(paste0("package_coverage fallback failed: ", conditionMessage(e)))
          NULL
        })
        if (!is.null(pkg_cov)) {
          pkg_pct <- tryCatch(covr::percent_coverage(pkg_cov), error = function(e) NA_real_)
          if (is.numeric(pkg_pct) && !is.na(pkg_pct) && pkg_pct > 0) {
            cov <- pkg_cov
          }
        }
      }
    }
    
    cov
  }, error = function(e) {
    cleanup_and_return_null(
      paste0("Error running coverage for ", pkg_name, " : ", e$message),
      env = cov_env
    )
  })
  
  if (!is.null(coverage)) {
    
    # create coverage list
    res_cov <- tryCatch({
      coverage_list <- covr::coverage_to_list(coverage)
      list(name = pkg_name, coverage = coverage_list, errors = NA, notes = NA)
    },
    error = function(cond){
      coverage_list <- list(filecoverage = NA, totalcoverage = NA_integer_)
      list(
        name = pkg_name, coverage = coverage_list,
        errors = cond,
        notes = NA
      )
    })
    
    # set up inputs for calculating package coverage
    percent_cov <- as.numeric(res_cov$coverage$totalcoverage)
    
    functions_no_tests_df <- no_tests_df  # data.frame of functions lacking tests
    tests_passing <- test_files_clean               # character vector of passing test files
    tests_skipped <- skip_tests              # character vector of skipped test files
    
    # calculate package coverage
    calc_covr_list <- compute_total_coverage(percent_cov, 
                                             functions_no_tests_df, 
                                             tests_passing,
                                             tests_skipped
    )
    
    
    total_cov <- calc_covr_list$total_cov
    
    percent_cov_round <- round(total_cov, 2)
    message(paste0("Total Coverage for ", pkg_name, " is: ", percent_cov_round, "%"))
    
    percent_cov <- calc_covr_list$percent_cov
    n_functions_tests_passing <- calc_covr_list$n_testfiles
    n_functions_no_tests <- calc_covr_list$n_no_function_tests
    n_functions_tests_skipped <- calc_covr_list$n_skipped_files
    
    message(sprintf(
      "Approx total coverage: (%.2f%%), counts: tested_functions=%d, untested_functions=%d, skipped_tests=%d",
      percent_cov, n_functions_tests_passing, n_functions_no_tests, n_functions_tests_skipped
    ))
    
    cleanup_and_return_null(env = cov_env)
    
    # structure the return list
    covr_list <- list(
      total_cov = total_cov,
      res_cov = list(
        name = res_cov$name,
        coverage = res_cov$coverage,
        errors = res_cov$errors,
        notes = res_cov$notes
      ),
      functions_no_tests = no_tests_df,
      tests_skipped = skip_tests,
      tests_passing = test_files_clean,
      n_functions_tests_passing = n_functions_tests_passing,
      n_functions_no_tests = n_functions_no_tests,
      n_functions_tests_skipped = n_functions_tests_skipped
    )
    
  } else {
    message("No test coverage for this configuration")
    covr_list <- list(
      total_cov = 0,
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
          totalcoverage = 0
        ),
        errors = "No RUnit or BiocGenerics configuration",
        notes = NA
      )
    )
    
  } 
  return(covr_list)
}


#' Detect if a package uses the Bioconductor RUnit layout
#'
#' Heuristics:
#'  - inst/unitTests exists, OR
#'  - tests/run_unitTests.R exists, OR
#'  - explicit flag in test_pkg_data$has_BioG_test
#'
#' @keywords internal
is_bioc_runit_layout <- function(pkg_source_path, test_pkg_data = NULL) {
  is_runit_layout(pkg_source_path, test_pkg_data)
}

#' Detect if a package uses RUnit layout (BioC or RUnit-only)
#'
#' Returns TRUE when the package has RUnit tests in inst/unitTests (with or without
#' the BiocGenerics .test() runner). Heuristics:
#'  - test_pkg_data$has_BioG_test (BioC full layout), OR
#'  - test_pkg_data$has_RUnit_test (inst/unitTests, no .test required), OR
#'  - inst/unitTests exists, OR
#'  - tests/run_unitTests.R exists
#'
#' @param pkg_source_path Character. Path to package source root.
#' @param test_pkg_data List or NULL. Output from check_pkg_tests_and_snaps().
#' @return Logical. TRUE if the package uses RUnit layout.
#' @keywords internal
is_runit_layout <- function(pkg_source_path, test_pkg_data = NULL) {
  inst_unitTests <- file.path(pkg_source_path, "inst", "unitTests")
  run_ut_wrapper <- file.path(pkg_source_path, "tests", "run_unitTests.R")
  
  isTRUE(test_pkg_data$has_BioG_test) ||
    isTRUE(test_pkg_data$has_RUnit_test) ||
    dir.exists(inst_unitTests) ||
    file.exists(run_ut_wrapper)
}

#' Detach a package if attached (internal)
#'
#' Safely detaches a package from the search path and optionally unloads its namespace.
#' @keywords internal
detach_pkg_if_attached <- function(pkg, unload_namespace = TRUE, quiet = TRUE) {
  
  pkg_search_name <- paste0("package:", pkg)
  
  if (pkg_search_name %in% search()) {
    try(detach(pkg_search_name, unload = TRUE, character.only = TRUE), silent = quiet)
    if (!quiet) message("Detached ", pkg_search_name)
  }
  if (isTRUE(unload_namespace) && isNamespaceLoaded(pkg)) {
    try(unloadNamespace(pkg), silent = quiet)
    if (!quiet) message("Unloaded namespace: ", pkg)
  }
  invisible(TRUE)
}

#' Prepare a BioC/RUnit package for testing
#' Detaches common BioC infrastructure packages (if attached) and load_all() the target
#' package before RUnit tests. Also clears R_TESTS to avoid interference from the check harness.
#'
#' @param pkg_name Character. Package name.
#' @param pkg_source_path Character. Path to package source.
#' @param detach_bioc_infra Logical. If TRUE (default for BioC packages), detach
#'   IRanges, S4Vectors, BiocGenerics. Set to FALSE for RUnit-only packages.
#'   
#' @keywords internal
prepare_for_bioc_runit <- function(pkg_name, 
                                   pkg_source_path, 
                                   detach_bioc_infra
                                   ) {
  
  # Detach target if already attached
  detach_pkg_if_attached(pkg_name)
  # Detach common BioC infra (optional; skip for RUnit-only packages)
  if (isTRUE(detach_bioc_infra)) {
    for (p in c("IRanges", "S4Vectors", "BiocGenerics")) {
      detach_pkg_if_attached(p)
    }
  }
  
  # Save current options and set warn = 1 temporarily
  old_opts <- options()
  options(warn = 1)
  on.exit(options(old_opts), add = TRUE)
  
  
  # Load package from source (dev)
  pkgload::load_all(pkg_source_path, quiet = TRUE)
  # Avoid R CMD check harness interference when running RUnit programmatically
  Sys.setenv("R_TESTS" = "")
  invisible(TRUE)
}

#' Check for skipped or errored test files (BiocGenerics)
#'
#' This internal function attempts to run test files mapped to source files and
#' identifies any that produce warnings or errors. It does not handle `testit`.
#'
#' @param pkg_source_path Path to the root of the package source.
#' @param mapping A data frame with a column `test_file` listing test file names.
#' @param test_path directory  
#'
#' @return A data frame listing test files and any issues encountered.
#' @keywords internal
check_covr_skip_runit_nstf <- function(pkg_source_path, mapping, test_path) {
  
  # Guard: RUnit must be available
  if (!requireNamespace("RUnit", quietly = TRUE)) {
    stop("RUnit must be installed to run RUnit tests (install.packages('RUnit')).")
  }
  
  # Some RUnit wrappers advise clearing R_TESTS when running programmatically
  # to avoid R CMD check harness interference. Restore it on exit.
  # See general RUnit runner guidance & practice.  [1](https://www.rdocumentation.org/packages/TriMatch/versions/1.0.0/topics/as.data.frame.list)
  # Save current R_TESTS, set it to ""
  
  # Save current value
  old_R_TESTS <- Sys.getenv("R_TESTS", unset = NA_character_)
  
  is_windows <- tolower(Sys.info()[["sysname"]]) == "windows"
  
  
  if (is_windows) {
    # Windows branch: quoted name in Sys.setenv works best here
    Sys.setenv("R_TESTS" = "")
    on.exit({
      if (is.na(old_R_TESTS)) {
        Sys.unsetenv("R_TESTS")
      } else {
        Sys.setenv("R_TESTS" = old_R_TESTS)
      }
    }, add = TRUE)
  } else {
    # Linux/macOS branch: unquoted name in Sys.setenv works reliably here
    Sys.setenv(R_TESTS = "")
    on.exit({
      if (is.na(old_R_TESTS)) {
        Sys.unsetenv("R_TESTS")
      } else {
        Sys.setenv(R_TESTS = old_R_TESTS)
      }
    }, add = TRUE)
  }
  
  
  test_dir <- test_path
  
  # Use provided mapping; drop rows with NA test_file only
  clean_mapping <-
    if (anyNA(mapping$test_file)) tidyr::drop_na(mapping, test_file) else mapping
  
  # Collector
  problems <- data.frame(test_file = character(),
                         issue_type = character(),
                         stringsAsFactors = FALSE)
  
  run_one_file <- function(test) {
    
    abs <- file.path(test_dir, test)
    
    # Missing file -> flag as 'missing'
    if (!file.exists(abs)) {
      return(data.frame(test_file = test, issue_type = "missing", stringsAsFactors = FALSE))
    }
    
    # load RUnit to ensure all RUnit functions are available for tests
    # library(RUnit)
    
    # Ensure RUnit assertions are visible to legacy tests (temporary attach)
    if (!"package:RUnit" %in% search()) {
      attachNamespace("RUnit")
      on.exit({
        # Detach if we attached it
        if ("package:RUnit" %in% search()) {
          try(detach("package:RUnit", unload = TRUE, character.only = TRUE), silent = TRUE)
        }
      }, add = TRUE)
    }
    
    # Execute the file's tests using RUnit (not source())
    td <- tryCatch(
      {
        RUnit::runTestFile(
          absFileName        = abs,
          useOwnErrorHandler = TRUE,
          testFuncRegexp     = "^test.+"
        )
      },
      error = function(e) {
        # parse/runtime error before RUnit could produce a test data object
        return(structure(list(.error = TRUE), class = "RUnitTestData-ERROR"))
      }
    )
    
    # If we got a hard error, flag it
    if (inherits(td, "RUnitTestData-ERROR")) {
      return(data.frame(test_file = test, issue_type = "error", stringsAsFactors = FALSE))
    }
    
    # Summarize via RUnit API
    # returns counts: nErr, nFail, nDeactivated, etc.  
    # [2](https://bioc.r-universe.dev/BiocGenerics/BiocGenerics.pdf)
    errs <- RUnit::getErrors(td)  
    
    if (!is.null(errs$nErr) && errs$nErr > 0) {
      data.frame(test_file = test, issue_type = "error", stringsAsFactors = FALSE)
    } else if (!is.null(errs$nFail) && errs$nFail > 0) {
      data.frame(test_file = test, issue_type = "failure", stringsAsFactors = FALSE)
    } else if (!is.null(errs$nDeactivated) && errs$nDeactivated > 0) {
      data.frame(test_file = test, issue_type = "skipped", stringsAsFactors = FALSE)
    } else {
      NULL  # clean run for this file
    }
  }
  
  problem_list <- lapply(clean_mapping$test_file, run_one_file)
  problems <- do.call(rbind, problem_list)
  
  if (is.null(problems) || nrow(problems) == 0) {
    problems <- data.frame(
      test_file  = NA_character_,
      issue_type = "No tests skipped",
      stringsAsFactors = FALSE
    )
  }
  
  return(problems)
}

#' Remove mapping rows that point to run_unitTests.R
#'
#' @param mapping A data.frame with columns `source_file` and `test_file`
#'                returned by get_source_test_mapping_nstf().
#' @param test_pkg_data A list or environment containing `bioc_run_ut_path`,
#'                      e.g., "/tmp/.../BiocGenerics/tests/run_unitTests.R".
#'
#' @return The filtered mapping data.frame with run_unitTests.R rows removed.
#' @keywords internal
remove_run_unitTests_mapping <- function(mapping, test_pkg_data) {
  
  # Derive the filename from the provided path, if available.
  run_ut_name <- NA_character_
  if (!missing(test_pkg_data) &&
      !is.null(test_pkg_data$bioc_run_ut_path) &&
      !is.na(test_pkg_data$bioc_run_ut_path)) {
    run_ut_name <- basename(test_pkg_data$bioc_run_ut_path)
  }
  
  # Build a logical vector marking rows to drop
  drop_rows <- rep(FALSE, nrow(mapping))
  
  # 1) Exact filename (case-insensitive) if we have it
  if (!is.na(run_ut_name)) {
    drop_rows <- drop_rows |
      (!is.na(mapping$test_file) &
         tolower(mapping$test_file) == tolower(run_ut_name))
  }
  
  # 2) Fallback regex to be resilient to small variations
  #    Matches run_unitTests.R, run-unitTests.R, RUN_UNITTESTS.R, etc.
  drop_rows <- drop_rows |
    (!is.na(mapping$test_file) &
       grepl("run[_-]?unitTests\\.R$", mapping$test_file, ignore.case = TRUE))
  
  # Return filtered mapping
  mapping <- mapping[!drop_rows, , drop = FALSE]
  
  return(mapping)
}



#' Locate active test paths for Bioconductor RUnit layout
#'
#' Returns a *list* of one or more existing test directories suitable for
#' normalization and use in subsequent mapping (e.g., `get_source_test_mapping_nstf`).
#' Designed to work with:
#'
#'   path    <- as.character(pkg_source_path)
#'   testdir <- file.path(path, "inst/unitTests")
#'   test_path_list <- find_biocgenerics_test_paths(test_pkg_data, testdir)
#'   test_path <- normalizePath(
#'     unlist(test_path_list, use.names = TRUE),
#'     winslash = "/",
#'     mustWork = FALSE
#'   )
#'
#' @param test_pkg_data list/env of test metadata; may include:
#'   - `bioc_run_ut_path` (character): path to `tests/run_unitTests.R` wrapper, if present
#' @param testdir character: expected RUnit tests directory (typically "<pkg_root>/inst/unitTests")
#' @return named list of **existing** directories (e.g., `bioc_unitTests`, `tests_root`);
#'         if nothing is found, returns a single `fallback` entry pointing to `testdir`.
#' @keywords internal
get_runit_test_paths <- function(test_pkg_data, testdir) {
  # Normalize incoming testdir
  
  testdir <- normalizePath(testdir, winslash = "/", mustWork = FALSE)
  
  # Prepare output holder
  out <- list()
  
  # If testdir already exists and is inst/unitTests, prefer it
  if (dir.exists(testdir)) {
    out$bioc_unitTests <- testdir
  } else {
    # Otherwise, try to derive the package root and reconstruct inst/unitTests
    is_pkg_root <- function(dir) file.exists(file.path(dir, "DESCRIPTION"))
    
    # Probe upwards for a package root
    candidates <- unique(c(testdir, dirname(testdir), dirname(dirname(testdir))))
    pkg_root <- NA_character_
    for (cdir in candidates) {
      if (is_pkg_root(cdir)) {
        pkg_root <- normalizePath(cdir, winslash = "/", mustWork = FALSE)
        break
      }
    }
    
    if (!is.na(pkg_root)) {
      bioc_unit <- normalizePath(file.path(pkg_root, "inst", "unitTests"),
                                 winslash = "/", mustWork = FALSE)
      if (dir.exists(bioc_unit)) {
        out$bioc_unitTests <- bioc_unit
      }
    }
    
    # Final fallback if nothing found    # Final fallback if nothing found
    if (length(out) == 0) {
      out$fallback <- testdir
    }
  }
  
  # Single return point
  return(out)
  
}



#' Map Bioconductor and RUnit source files to test files, avoiding false positives.
#' Tightens filename and content-based matching; handles short names like "t".
#'
#' @param pkg_source_path Character; path to the package root (expects R/ under it).
#' @param test_dir        Character; path to tests directory (e.g., file.path(pkg, "tests"))
#' @return data.frame with columns: source_file, test_file, evidence, has_tests
#' @keywords internal
#' @noRd
get_source_test_mapping_runit_nstf <- function(pkg_source_path, test_dir) {
  source_dir <- file.path(pkg_source_path, "R")
  
  src_files  <- list.files(source_dir, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
  test_files <- list.files(test_dir,   pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  
  if (length(src_files) == 0) {
    stop("No R files found under: ", source_dir)
  }
  if (length(test_files) == 0) {
    warning("No R test files found under: ", test_dir)
  }
  
  src_basenames  <- basename(src_files)
  test_basenames <- basename(test_files)
  
  # Split a stem into tokens by any non-alphanumeric delimiter (underscore, dash, dot, etc.)
  tokenize_stem <- function(stem) {
    toks <- unlist(strsplit(stem, "[^A-Za-z0-9]+", perl = TRUE))
    toks[nchar(toks) > 0]
  }
  
  # Stricter filename-based match: symbol must equal a token (case-insensitive).
  # Special handling for short symbols (<=2 chars) to avoid matching "test_*" prefix.
  filename_match <- function(src_stem, test_stem) {
    toks <- tolower(tokenize_stem(test_stem))
    s    <- tolower(src_stem)
    if (nchar(s) <= 2) {
      # Only accept if token equals the symbol (e.g., test_t.R or test-t.R)
      return(s %in% toks)
    } else {
      return(s %in% toks)
    }
  }
  
  # Extract declared symbols from a source file:
  # - S4 generics via setGeneric("name")
  # - function objects via `name <- function(...)`
  extract_symbols <- function(file) {
    lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character())
    generics <- character()
    fun_defs <- character()
    
    if (length(lines)) {
      gen_lines <- grep('setGeneric\\("', lines, value = TRUE, fixed = TRUE)
      if (length(gen_lines)) {
        generics <- unique(sub('.*setGeneric\\("([A-Za-z0-9._]+)".*', '\\1', gen_lines))
      }
      fun_lines <- grep('^\\s*[A-Za-z0-9._]+\\s*<-\\s*function\\s*\\(', lines, value = TRUE, perl = TRUE)
      if (length(fun_lines)) {
        fun_defs <- unique(sub('^\\s*([A-Za-z0-9._]+)\\s*<-\\s*function\\s*\\(.*', '\\1', fun_lines, perl = TRUE))
      }
    }
    unique(c(generics, fun_defs))
  }
  
  # Determine whether a test file is *primarily* about a given symbol.
  # Evidence sources:
  #  - Filename tokens contain the symbol
  #  - test_that("...sym...") description contains symbol as a whole word
  #  - RUnit-style test function name contains symbol (e.g., test_order <- function() {})
  # We deliberately IGNORE mere calls inside assertions that use the symbol as a helper.
  test_targets_symbol <- function(tf, sym) {
    lines <- tryCatch(readLines(tf, warn = FALSE), error = function(e) character())
    if (!length(lines)) return(FALSE)
    
    s <- tolower(sym)
    
    # 1) Filename tokens
    stem <- tools::file_path_sans_ext(basename(tf))
    toks <- tolower(tokenize_stem(stem))
    if (nchar(s) <= 2) {
      if (s %in% toks) return(TRUE)
    } else {
      if (s %in% toks) return(TRUE)
    }
    
    # 2) test_that descriptions (testthat)
    #    Match test_that("...") or test_that('...')
    desc_lines <- grep('test_that\\s*\\(\\s*["\']', lines, value = TRUE, perl = TRUE)
    if (length(desc_lines)) {
      # Pull the first quoted string (description)
      desc <- gsub('^.*test_that\\s*\\(\\s*"\'["\'].*$', '\\1', desc_lines, perl = TRUE)
      has_word <- any(grepl(paste0("\\b", s, "\\b"), tolower(desc), perl = TRUE))
      if (has_word) return(TRUE)
    }
    
    # 3) RUnit style test function names
    #    Looks like: test_<name> <- function() { ... }
    runit_defs <- grep('^\\s*test_[A-Za-z0-9._-]+\\s*<-\\s*function\\s*\\(', lines, value = TRUE, perl = TRUE)
    if (length(runit_defs)) {
      names <- gsub('^\\s*(test_[A-Za-z0-9._-]+)\\s*<-\\s*function\\s*\\(.*$', '\\1', runit_defs, perl = TRUE)
      name_toks <- tolower(unlist(strsplit(names, "[^A-Za-z0-9]+", perl = TRUE)))
      if (s %in% name_toks) return(TRUE)
    }
    
    # Do NOT treat incidental usage, setGeneric/setMethod inside tests, etc., as primary evidence.
    return(FALSE)
  }
  
  out_rows <- lapply(seq_along(src_files), function(i) {
    src_full <- src_files[i]
    src_base <- src_basenames[i]
    src_stem <- tools::file_path_sans_ext(src_base)
    
    # --- Pass 1: strict filename matching ---
    fname_idx <- which(vapply(test_basenames, function(tb) {
      filename_match(src_stem, tools::file_path_sans_ext(tb))
    }, logical(1)))
    
    test_found <- if (length(fname_idx) > 0) test_basenames[fname_idx[1]] else NA_character_
    evidence   <- if (!is.na(test_found)) "filename_match" else NA_character_
    
    # --- Pass 2: content-based primary-target checks ---
    # Only if filename matching failed
    if (is.na(test_found) && length(test_files) > 0) {
      # Prefer S4 generics / defined function symbols
      symbols <- extract_symbols(src_full)
      if (!length(symbols)) symbols <- src_stem
      
      # Try each symbol until we find a test file that targets it
      for (sym in symbols) {
        tf_idx <- which(vapply(test_files, function(tf) test_targets_symbol(tf, sym), logical(1)))
        if (length(tf_idx) > 0) {
          test_found <- test_basenames[tf_idx[1]]
          evidence   <- "content_match"
          break
        }
      }
    }
    
    data.frame(
      source_file = src_base,
      test_file   = ifelse(is.na(test_found), NA_character_, test_found),
      evidence    = ifelse(is.na(test_found), NA_character_, evidence),
      has_tests   = !is.na(test_found),
      stringsAsFactors = FALSE
    )
  })
  
  
  mapping <- do.call(rbind, out_rows)
  rownames(mapping) <- NULL
  return(mapping)
}

#' Detect RUnit test function prefix from test files (test., runit., or Test.)
#'
#' Scans test file content and filenames for function definitions or naming patterns
#' to avoid regex with parentheses that can cause parsing errors when written to
#' runner scripts. Matches patterns like \code{test.plot <-} (function may be on next line).
#'
#' @param test_files Character vector of paths to RUnit test files.
#' @return Character. One of "test", "runit", "Test"; defaults to "test".
#' @keywords internal
#' @noRd
detect_runit_test_prefix <- function(test_files) {
  
  prefix_from_filename <- function(f) {
    base <- basename(f)
    
    if (grepl("^runit\\.[a-zA-Z0-9_.]+\\.R$", base, ignore.case = TRUE)) return("runit")
    if (grepl("^Test\\.[a-zA-Z0-9_.]+\\.R$", base)) return("Test")
    if (grepl("^test_[a-zA-Z0-9_.]+\\.R$", base, ignore.case = TRUE)) return("test")
    if (grepl("^test\\.[a-zA-Z0-9_.]+\\.R$", base, ignore.case = TRUE)) return("test")
    NA_character_
  }
  found <- lapply(test_files, function(f) {
    
    p <- prefix_from_filename(f)
    if (!is.na(p)) return(p)
    NA_character_
  })
  for (p in found) {
    if (!is.null(p) && !is.na(p)) return(p)
  }
  "test"
}

#' Build a wrapper script for doRUnit.R that sets the working directory
#'
#' Many doRUnit.R scripts (e.g. fBasics) derive the package name from
#' \code{basename(dirname(getwd()))}, expecting to be run from \code{tests/}
#' (R CMD check layout). This wrapper sets \code{wd} to the tests directory
#' before sourcing doRUnit.R.
#'
#' @param doRUnit_path Character. Absolute path to \code{tests/doRUnit.R}.
#'
#' @return Character. Path to the temporary wrapper script.
#'
#' @keywords internal
#' @noRd
build_doRUnit_wrapper <- function(doRUnit_path) {
  
  runner <- tempfile(fileext = ".R")
  lines <- c(
    "owd <- getwd()",
    "on.exit(setwd(owd), add = TRUE)",
    sprintf('setwd("%s")', dirname(doRUnit_path)),
    sprintf('source("%s")', doRUnit_path)
  )
  writeLines(lines, runner)
  
  return(runner)
}

#' Build a runTests-style runner with explicit testFuncRegexp
#'
#' Replicates the runTests.R flow (defineTestSuite + runTestSuite) but injects
#' \code{testFuncRegexp} so runit.*.R files are executed. Package runTests.R
#' uses RUnit default \code{testFuncRegexp = "^test.+"} which does not match
#' \code{runit.*} functions.
#'
#' @param pkg_name Character. Package name (e.g. "fBasics").
#' @param test_files Character vector of test file paths (used to detect prefix).
#' @param pkg_root Character source of package
#' @return Character. Path to the temporary runner script.
#'
#' @keywords internal
#' @noRd
build_runit_runTests_runner <- function(pkg_name, test_files, pkg_root = NULL) {
  
  prefix <- detect_runit_test_prefix(test_files)
  # runit.*.R files may use runit.* or test.* functions; match both for robustness
  # Use [.] for literal dot to avoid R "unrecognized escape" when sourcing generated script
  testFuncRegexp <- switch(prefix,
                           runit = '"^runit[.]|^test.+"',
                           Test  = '"^Test[.]|^test.+"',
                           '"^test.+"'
  )
  paths <- normalizePath(test_files, winslash = "/", mustWork = TRUE)
  run_test_line <- vapply(paths, function(p) {
    sprintf('RUnit::runTestFile("%s", useOwnErrorHandler = TRUE, testFuncRegexp = %s)', p, testFuncRegexp)
  }, character(1L))
  runner <- tempfile(fileext = ".R")
  # Set wd to package root so tests can find data files (inst/unitTests/data, etc.)
  wd_lines <- if (!is.null(pkg_root) && dir.exists(pkg_root)) {
    c(
      "owd <- getwd()",
      "on.exit(setwd(owd), add = TRUE)",
      sprintf('setwd("%s")', normalizePath(pkg_root, winslash = "/", mustWork = TRUE))
    )
  } else character(0L)
  lines <- c(
    "if (!requireNamespace('RUnit', quietly = TRUE)) stop('RUnit is required')",
    "if (!'package:RUnit' %in% search()) attachNamespace('RUnit')",
    sprintf('library(package = "%s", character.only = TRUE)', pkg_name),
    wd_lines,
    run_test_line
  )
  writeLines(lines, runner)
  
  return(runner)
}

#' Build a custom RUnit runner script
#'
#' Creates a temporary R script that loads RUnit and runs each test file via
#' \code{RUnit::runTestFile()}. The test function regex is chosen based on the
#' detected prefix (runit, Test, or test) from the test filenames.
#'
#' @param pkg_name Character. Package name (e.g. "fBasics").
#' @param tests Character vector of absolute paths to RUnit test files
#'   (e.g. under \code{inst/unitTests}).
#' @param pkg_root Character source of pack
#'
#' @return Character. Path to the temporary runner script (suitable for
#'   \code{covr::environment_coverage(test_files = ...)}).
#'
#' @keywords internal
#' @noRd
build_runit_custom_runner <- function(tests, pkg_name = NULL, pkg_root = NULL) {
  runner <- tempfile(fileext = ".R")
  prefix <- detect_runit_test_prefix(tests)
  paths <- normalizePath(tests, winslash = "/", mustWork = TRUE)
  # runit.*.R files may use runit.* or test.* functions; match both (same as build_runit_runTests_runner)
  testFuncRegexp <- switch(prefix,
                           runit = '"^runit[.]|^test.+"',
                           Test  = '"^Test[.]|^test.+"',
                           '"^test.+"'
  )
  run_test_line <- vapply(paths, function(p) {
    sprintf('RUnit::runTestFile("%s", useOwnErrorHandler = TRUE, testFuncRegexp = %s)', p, testFuncRegexp)
  }, character(1L))
  wd_lines <- if (!is.null(pkg_root) && dir.exists(pkg_root)) {
    c(
      "owd <- getwd()",
      "on.exit(setwd(owd), add = TRUE)",
      sprintf('setwd("%s")', normalizePath(pkg_root, winslash = "/", mustWork = TRUE))
    )
  } else character(0L)
  lib_line <- if (!is.null(pkg_name) && nzchar(pkg_name)) {
    sprintf('library(package = "%s", character.only = TRUE)', pkg_name)
  } else character(0L)
  runner_lines <- c(
    "if (!requireNamespace('RUnit', quietly=TRUE)) stop('RUnit is required')",
    "if (!'package:RUnit' %in% search()) attachNamespace('RUnit')",
    lib_line,
    wd_lines,
    run_test_line
  )
  writeLines(runner_lines, runner)
  return(runner)
}  