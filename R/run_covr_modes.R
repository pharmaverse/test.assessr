#' Run Coverage Analysis with Test Detection
#'
#' This function inspects the test configuration of an R package and runs 
#' code coverage analysis using any available testing framework, including 
#' `testthat`, `testit`, base R test scripts, or Bioconductor-style tests. 
#' If no recognised testing configuration is found, a default zero‑coverage
#' result is returned.
#'
#' @param pkg_source_path Character. Path to the root directory of the 
#'   package source.
#' @param covr_timeout Numeric. Timeout in seconds for running coverage 
#'   analysis. Default is 60.
#'
#' @return 
#' A named list containing coverage results, package metadata, and test
#' configuration details. The returned list includes (but is not limited to):
#' \describe{
#'   \item{pkg_name}{Character. Package name extracted from the DESCRIPTION file.}
#'   \item{pkg_ver}{Character. Package version.}
#'   \item{date_time}{Timestamp of when the analysis was run.}
#'   \item{executor}{User or environment running the analysis.}
#'   \item{sysname, version, release, machine}{System metadata.}
#'   \item{r_version}{R version used during analysis.}
#'   \item{test_framework_type}{Character. Detected testing framework type.}
#'   \item{covr_list}{A nested list containing: 
#'     \describe{
#'       \item{total_cov}{Numeric. Aggregated coverage percentage.}
#'       \item{res_cov}{File‑level and line‑level coverage details.}
#'       \item{errors, notes}{Any warnings or notes detected during testing.}
#'     }
#'   }
#'   \item{test_pkg_data}{A list describing the test configuration of the
#'     package (presence of testthat, testit, base tests, snapshots, etc.).}
#' }
#'
#' This function always returns a value. When no supported testing framework is
#' detected, a default object with zero coverage and diagnostic information is
#' returned.
#'
#' @examples
#' \donttest{
#'
#' dp <- system.file("test-data", 
#'   "test.package.0001_0.1.0.tar.gz", 
#'   package = "test.assessr")
#' 
#' # set up package
#' install_list <- set_up_pkg(dp)
#'
#' package_installed <- install_list$package_installed
#' pkg_source_path <- install_list$pkg_source_path
#'
#' # install package locally to ensure test works
#' package_installed <- install_package_local(pkg_source_path)
#' package_installed <- TRUE
#'
#' covr_mode_list <- run_covr_modes(pkg_source_path)
#'
#' }
#'
#' @export
run_covr_modes <- function(pkg_source_path,
                           covr_timeout = 60
                          ) {
 
  # --- Temporarily set CRAN repo (restore immediately on exit) ---
  old_repos <- getOption("repos")
  tmp_repos <- old_repos
  tmp_repos["CRAN"] <- "http://cran.us.r-project.org"
  options(repos = tmp_repos)
  # Ensure options are restored even if the function errors
  on.exit(options(repos = old_repos), add = TRUE)
  
  # Get package name and version
  pkg <- get_pkg_name(pkg_source_path)
  
  pkg_desc <- get_pkg_desc(pkg_source_path, 
                           fields = c("Package", 
                                      "Version"))
  pkg_name <- pkg_desc$Package
  pkg_ver <- pkg_desc$Version
 
  metadata <- get_test_metadata()
  
  # Attempt to unload package safely
  tryCatch({
    if (pkg %in% loadedNamespaces()) {
      message(paste0("Unloading ", pkg, " package to avoid coverage conflict..."))
      unloadNamespace(pkg)
    }
  }, error = function(e) {
    message(paste0("Warning: Failed to unload ", pkg, " package. Proceeding anyway."))
    message(paste0("Details: ", e$message))
  })
  
  test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
  
  # set up framework label
  framework_label <- if (isTRUE(test_pkg_data$has_testthat)) {
    "testthat"
  } else if (isTRUE(test_pkg_data$has_testit) || isTRUE(test_pkg_data$has_tests_base)) {
    "testit"
  } else if (isTRUE(test_pkg_data$has_BioG_test)) {
    "bioc"
  } else {
    "none"
  }
  
  # set up covr env to prevent writing to global env
  cov_env <- initialise_covr_env(pkg_source_path, 
                                 pkg, 
                                 framework = framework_label)
  
  #clean up covr env
  on.exit(
    cleanup_and_return_null(
      msg = sprintf("Cleaning coverage workspace for %s", pkg),
      env = cov_env
    ),
    add = TRUE
  )
  
  
  if (test_pkg_data$has_testthat | test_pkg_data$has_testit) {
    message("running standard testing framework")
    covr_list <- run_coverage(
      pkg_source_path,
      covr_timeout
    )
    
    if (is.na(covr_list$total_cov) && all(is.na(covr_list$res_cov$coverage$filecoverage))) {
      
      if (test_pkg_data$has_testthat) {
        message("running skip testing with standard testing framework")
        covr_list <- run_covr_skip_stf(pkg_source_path, test_pkg_data, cov_env)
        
      } else if (test_pkg_data$has_testit) {
        message("running skip testing with non-standard testing framework")
        covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env)
      }
    }
    
  } else if (test_pkg_data$has_tests_base) {
    message("running non-standard testing framework")
    covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env)
  } else if (test_pkg_data$has_BioG_test) {
    message("running non-standard BioGenerics testing framework")
    covr_list <- run_covr_skip_bioc_nstf(pkg_source_path, test_pkg_data, cov_env)  
  } else {
    message("No recognised standard or non-standard testing configuration")
    covr_list <- list(
      total_cov = 0,
      res_cov = list(
        name = pkg,
        coverage = list(
          filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
          totalcoverage = 0
        ),
        errors = "No recognised standard or non-standard testing configuration",
        notes = NA
      )
    )
  }
  
  # get R version 
  r_version <- as.character(getRversion())
  
  # add testing framework type
  if (test_pkg_data$has_testthat ) {
     test_framework_type <- "standard testing framework"
  } else if (test_pkg_data$has_tests_base) {
    test_framework_type <- "non-standard testing framework"
  } else if (test_pkg_data$has_testit) {
      test_framework_type <- "non-standard testing framework - testit"  
  } else if (test_pkg_data$has_BioG_test) {
    test_framework_type <- "non-standard testing framework - Bioconductor"
  } else {
    test_framework_type <- "non-standard testing framework"
  }
    
  
  # add metadata 
  covr_list <- c(list(pkg_name = pkg_name,
                      pkg_ver = pkg_ver,
                      date_time = metadata$datetime,
                      executor = metadata$executor,
                      sysname = metadata$info$sys$sysname,
                      version = metadata$info$sys$version,
                      release = metadata$info$sys$release,
                      machine = metadata$info$sys$machine,
                      r_version = r_version,
                      test_framework_type = test_framework_type), covr_list)
  
  return(covr_list)
}




#' Setup coverage environment using an existing cov_env (CRAN-safe)
#'
#' NOTE:
#' - This function no longer creates `cov_env` nor writes helper files into tests/.
#' - Pass an environment (e.g., from initialise_covr_env()) via `cov_env`.
#' - All writes by downstream helpers should target cov_env$work_dir (under tempdir()).
#'
#' @param pkg_source_path Character, package root path.
#' @param pkg_name        Character, package name.
#' @param test_path       Character, path to tests (default "tests/testthat").
#' @param cov_env         Environment created by initialise_covr_env(); must exist.
#' @return list(cov_env, datasets_loaded, root_dir, initial_state, helpers_loaded)
#' @keywords internal
setup_covr_env <- function(pkg_source_path,
                           pkg_name,
                           test_path = "tests/testthat",
                           cov_env) {
  message(paste0("Setting up coverage environment for: ", pkg_name))
  
  # --- Require an existing cov_env (this function no longer creates one) ---
  if (missing(cov_env) || !is.environment(cov_env)) {
    stop("`cov_env` must be provided and must be an environment (e.g., from initialise_covr_env()).")
  }
  
  datasets_loaded <- character()
  root_dir <- NULL
  helpers_loaded <- character()
  
  # Record initial state before setup
  initial_state <- ls(envir = cov_env, all.names = TRUE)
  
  # (helper setup moved elsewhere) — no in-memory or file helper is created here.
  
  tryCatch({
    # Load helper files (READ-ONLY from tests/, no writes there)
    message(paste0("Loading helpers into coverage environment for: ", pkg_name))
    helper_files <- list.files(test_path, pattern = "^helper.*\\.R$", full.names = TRUE)
    for (file in helper_files) {
      tryCatch({
        sys.source(file, envir = cov_env)
        helpers_loaded <- c(helpers_loaded, file)
      }, error = function(e) {
        message(paste("Failed to source helper file:", file, ":", e$message))
      })
    }
    
    # Load package code into a namespace, then copy objects into cov_env
    message(paste0("Loading package into coverage environment for: ", pkg_name))
    pkgload::load_all(pkg_source_path)
    
    pkg_all <- ls(envir = asNamespace(pkg_name), all.names = TRUE)
    for (obj in pkg_all) {
      tryCatch({
        assign(obj, get(obj, envir = asNamespace(pkg_name)), envir = cov_env)
      }, error = function(e) {
        message(paste0("Failed to assign object ", obj, " : ", e$message))
      })
    }
    
    # Locate project root (read-only)
    message(paste0("Locating project root for: ", pkg_name))
    root_dir <- find_project_root()
    if (is.null(root_dir)) {
      stop("Project root not found. Coverage cannot be run.")
    }
    
    # Load datasets into cov_env
    message(paste0("Loading datasets into coverage environment for: ", pkg_name))
    datasets_loaded <- load_package_datasets(pkg_name, env = cov_env)
    message("Datasets loaded: ", paste(datasets_loaded, collapse = ", "))
    
    # Load dependencies into env (ensure any transient writes use cov_env$work_dir)
    message(paste0("Loading dependencies into coverage environment for: ", pkg_name))
    load_dependencies_into_env(pkg_source_path, cov_env)
    
    # Unload package to avoid conflicts
    message(paste0("Unloading package: ", pkg_name))
    safe_unload_package(pkg_name)
    
  }, error = function(e) {
    cleanup_and_return_null(
      paste0("Error during setup for ", pkg_name, " : ", e$message),
      env = cov_env
    )
  })
  
  return(list(
    cov_env = cov_env,
    datasets_loaded = datasets_loaded,
    root_dir = root_dir,
    initial_state = initial_state,
    helpers_loaded = helpers_loaded
  ))
}

#' @title Load Datasets from a Package
#' @description 
#' Loads all datasets from a specified installed package into the cov environment.
#' It first attempts to load `.rda` files from the package's `data/` directory.
#' If none are found, it falls back to using `data()` to load datasets listed in the package metadata.
#'
#' @param pkg_name A character string specifying the name of the package.
#' @param env A character string specifying the covr env
#'
#' @return A character vector of dataset names that were attempted to be loaded.
#' Returns `NULL` if the package is not installed.
#'
#' @details 
#' This function is intended for internal use. It silently attempts to load each dataset
#' and suppresses errors if loading fails. Datasets are loaded into the cov environment.
#'
#' @keywords internal
#' @importFrom utils read.table
#' @importFrom utils read.csv
#' @importFrom utils data
load_package_datasets <- function(pkg_name, env = cov_env) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    message(paste("Package", pkg_name, "is not installed."))
    return(NULL)
  }
  
  pkg_path <- find.package(pkg_name)
  data_path <- file.path(pkg_path, "data")
  datasets <- character()
  
  if (dir.exists(data_path)) {
    files <- list.files(data_path, full.names = TRUE)
    for (file in files) {
      ext <- tools::file_ext(file)
      name <- tools::file_path_sans_ext(basename(file))
      tryCatch({
        if (ext == "rda") {
          load(file, envir = env)
        } else if (ext %in% c("tab", "tsv")) {
          assign(name, read.table(file, header = TRUE, sep = "\t"), envir = env)
        } else if (ext == "csv") {
          assign(name, read.csv(file, header = TRUE), envir = env)
        } else if (ext == "txt") {
          assign(name, read.table(file, header = TRUE), envir = env)
        } else if (ext == "json") {
          if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite package required")
          assign(name, jsonlite::fromJSON(file), envir = env)
        } else if (ext == "R") {
          sys.source(file, envir = env)
        }
        datasets <- c(datasets, name)
      }, error = function(e) {
        message(paste("Failed to load", file, ":", e$message))
      })
    }
  }
  
  if (length(datasets) == 0) {
    available <- data(package = pkg_name)$results[, "Item"]
    for (dataset in available) {
      tryCatch({
        data(list = dataset, package = pkg_name, envir = env)
        datasets <- c(datasets, dataset)
      }, error = function(e) {
        message(paste("Failed to load dataset", dataset, ":", e$message))
      })
    }
  }
  
  return(unique(datasets))
}


#' Remove New Globals from Environment Based on Initial Snapshot
#'
#' This function removes objects from an environment that were added after
#' a recorded initial state. Optionally filters by pattern.
#'
#' @param env Environment to clean.
#' @param initial_state Character vector of object names before setup.
#' @param patterns Optional regex patterns to filter objects.
#' @param dry_run Logical. If TRUE, only prints what would be removed.
#'
#' @return Invisibly returns a character vector of removed object names.
#'
#' @keywords internal
remove_new_globals <- function(env, initial_state, patterns = NULL, dry_run = FALSE) {
  current_state <- ls(envir = env, all.names = TRUE)
  new_objs <- setdiff(current_state, initial_state)
  
  if (!is.null(patterns)) {
    new_objs <- unique(unlist(lapply(patterns, function(p) grep(p, new_objs, value = TRUE))))
  }
  
  if (length(new_objs) == 0) {
    message("No new globals detected.")
    return(invisible(character()))
  }
  
  if (dry_run) {
    message("New globals detected (dry run):")
    print(new_objs)
  } else {
    rm(list = new_objs, envir = env)
    message("Removed new globals:")
    print(new_objs)
  }
  
  invisible(new_objs)
}



#' Clean up cov environment and return NULL
#'
#' This internal utility function is used to clean up the cov environment
#' by removing all objects and optionally displaying a message before returning `NULL`.
#' It is primarily used within `run_coverage_base()` to ensure consistent cleanup
#' in error-handling scenarios.
#'
#' @param msg Optional character string. If provided, the message will be printed before cleanup.
#' @param env A character string specifying the covr env
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
cleanup_and_return_null <- function(msg = NULL, 
                                    env = cov_env
) {
  if (!is.null(msg)) message(msg)
  
  # 1) Clear all bindings from the provided env (as you had it)
  to_rm <- tryCatch(
    base::ls(envir = env, all.names = TRUE),
    error = function(e) base::ls(envir = env) # fall back if a stub doesn't accept all.names
  )
  if (length(to_rm)) {
    # Use base::rm explicitly to avoid stub recursion/signature issues
    for (sym in to_rm) {
      if (base::exists(sym, envir = env, inherits = FALSE)) {
        base::rm(list = sym, envir = env)
      }
    }
  }
  
  # 2) ALWAYS try to remove the temporary workspace referenced by env$work_dir
  #    (moved ahead of project-root handling so it's not skipped)
  try({
    if (base::exists("work_dir", envir = env, inherits = FALSE)) {
      work_dir <- base::get("work_dir", envir = env, inherits = FALSE)
      if (is.character(work_dir) && nzchar(work_dir)) {
        # Only remove if the workspace lives under the session tempdir()
        td_norm <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
        wd_norm <- tryCatch(
          normalizePath(work_dir, winslash = "/", mustWork = FALSE),
          error = function(e) work_dir
        )
        if (startsWith(wd_norm, td_norm) && dir.exists(work_dir)) {
          unlink(work_dir, recursive = TRUE, force = TRUE)
        }
      }
    }
  }, silent = TRUE)
  
  # 3) Project-root dependent orphan cleanup (unchanged logic, but no early return)
  # Determine a "project root" compatible with the test stubs
  # (the test stubs getwd() and normalizePath()).
  root_dir <- tryCatch(
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    error = function(e) NULL
  )
  if (is.null(root_dir)) {
    message("Project root not found; skipping orphan cleanup.")
  } else {
    # MASS creates "polr.pdf" in project root folder during testing
    # survival creates "silhouette-ex.ps" in project root folder during testing
    
    # Define extensions to remove
    extensions_to_remove <- c(".pdf", ".ps")
    
    # List all files in root directory (guard against non-existent path with tryCatch)
    files_in_root <- tryCatch(
      list.files(root_dir, full.names = TRUE),
      error = function(e) character(0)
    )
    
    # Filter files by extension (e.g., "\\.pdf$|\\.ps$")
    pat <- paste0("\\", extensions_to_remove, "$", collapse = "|")
    files_to_remove <- files_in_root[grepl(pat, files_in_root)]
    
    # Attempt removal (errors ignored; behavior is tested via stubbed file.remove)
    if (length(files_to_remove)) {
      invisible(vapply(files_to_remove, function(file) {
        if (file.exists(file)) {
          try(file.remove(file), silent = TRUE)
        }
        TRUE
      }, logical(1)))
    }
  }
  
  # 4) Exit silently
  invisible(NULL)
}



#' @title Find R project root directory
#' @description Searches upward from a starting path to locate the root of an R package or project.
#' @param start character, starting path (e.g., test directory or script path)
#' @return character path to project root, or NULL if not found
#' @keywords internal
find_project_root <- function(start = getwd()) {
  
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  while (TRUE) {
    if (file.exists(file.path(current, "DESCRIPTION")) ||
        file.exists(file.path(current, ".Rproj")) ||
        file.exists(file.path(current, ".git"))) {
      return(current)
    }
    parent <- dirname(current)
    if (parent == current) break
    current <- parent
  }
  return(NULL)
}


#' get functions with no tests
#'
#' @param mapping_df - data frame with source files and test files
#'
#' @return no_tests_df - data frame with source files with no tests
#' 
#' @keywords internal
get_function_no_tests <- function(mapping_df) {
  # Filter rows where test_file is NA
  no_tests_df <- subset(mapping_df, is.na(test_file))
  
  # Check if filtered_df is empty
  # If no rows are filtered, create a single-row data frame with the comment
  if (nrow(no_tests_df) == 0) {
    no_tests_df <- data.frame(
      source_file = NA,
      test_file = NA,
      comment = "all functions have at least 1 test",
      stringsAsFactors = FALSE
    )
  } else {
    # Add a comment to the filtered rows
    no_tests_df$comment <- "No tests found"
  }
  
  return(no_tests_df)
}


#' Create an empty covr list for fallback
#'
#' This internal utility function returns a default coverage list structure
#' used when test execution or mapping fails. It ensures consistent output
#' format for downstream processing even when no valid test results are available.
#'
#' @param pkg_name A character string representing the name of the package.
#' @param error_message A character string describing the reason for the fallback.
#'
#' @return A named list containing default coverage values, the provided error message,
#' and placeholder notes.
#'
#' @keywords internal
create_empty_covr_list <- function(pkg_name, error_message) {
  list(
    total_cov = 0,
    res_cov = list(
      name = pkg_name,
      coverage = list(
        filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
        totalcoverage = 0
      ),
      errors = paste(error_message),
      notes = NA
    )
  )
}


#' Safely unload a package to avoid coverage conflicts
#'
#' This internal function attempts to unload a specified package if it is currently loaded.
#' It is used to prevent coverage conflicts when reloading the package in a custom environment.
#'
#' @param pkg Character string. The name of the package to unload.
#' @keywords internal
#' @return NULL (invisibly). Messages are printed to indicate success or failure.
safe_unload_package <- function(pkg) {
  tryCatch({
    if (pkg %in% loadedNamespaces()) {
      message(paste0("Unloading ", pkg, " package to avoid coverage conflict..."))
      unloadNamespace(pkg)
    }
  }, error = function(e) {
    message(paste0("Warning: Failed to unload ", pkg, " package. Proceeding anyway."))
    message("Details: ", e$message)
  })
}


#' Load package objects into a coverage environment
#'
#' This internal function loads all objects from a package namespace into a specified environment.
#' It is used by `create_covr_list_no_skip()` to prepare the environment for coverage analysis.
#'
#' @param pkg_source_path Path to the package source directory.
#' @param pkg Name of the package to load.
#' @param cov_env Environment into which package objects will be loaded.
#'
#' @return Invisibly returns TRUE if successful, otherwise stops with an error.
#' @keywords internal
load_package_into_env <- function(pkg_source_path, pkg, cov_env) {
  tryCatch({
    pkgload::load_all(pkg_source_path)
    pkg_all <- ls(envir = asNamespace(pkg), all.names = TRUE)
    for (obj in pkg_all) {
      tryCatch({
        assign(obj, get(obj, envir = asNamespace(pkg)), envir = cov_env)
      }, error = function(e) {
        message(paste0("Failed to assign object ", obj, " : ", e$message))
      })
    }
    invisible(TRUE)
  }, error = function(e) {
    stop(paste0("Failed to load package ", pkg, " : ", e$message))
  })
}


#' Load Testthat Helper Files into Environment
#'
#' This function searches for all files in the `tests/testthat` directory
#' that match the pattern `helper-*.R` and sources them into the provided environment.
#' This is useful when running coverage with `covr::environment_coverage()` to ensure
#' helper functions are available during test execution.
#'
#' @param env An environment into which the helper files should be sourced.
#'            Typically this is the same environment used for coverage, e.g., `cov_env`.
#' @param helper_dir Path to the directory containing helper files. Defaults to `"tests/testthat"`.
#'
#' @return Invisibly returns `TRUE` if all files are sourced successfully.
#' @keywords internal
load_test_helpers <- function(env, helper_dir = "tests/testthat") {
  helper_files <- list.files(helper_dir, pattern = "^helper.*\\.R$", full.names = TRUE)
  for (file in helper_files) {
    tryCatch({
      sys.source(file, envir = env)
    }, error = function(e) {
      message(paste("Failed to source helper file:", file, ":", e$message))
    })
  }
  invisible(TRUE)
}



#' Initialise a temporary coverage environment (framework-aware)
#' @param pkg_source_path Character; package root.
#' @param pkg             Character; package name.
#' @param framework       Character; e.g. "testthat", "testit", "bioc", "none".
#' @param work_dir        Optional explicit temp workspace; if NULL it is composed from inputs.
#' @return environment cov_env with $work_dir and $pkg_root set.
#' @keywords internal
initialise_covr_env <- function(pkg_source_path, pkg, framework = "none", work_dir = NULL) {
  stopifnot(is.character(pkg_source_path), length(pkg_source_path) == 1L)
  stopifnot(is.character(pkg),            length(pkg)            == 1L)
  stopifnot(is.character(framework),      length(framework)      == 1L)
  
  if (is.null(work_dir) || !nzchar(work_dir)) {
    work_dir <- file.path(
      tempdir(),
      sprintf("covr_%s_%s_%s", pkg, framework, Sys.getpid())
    )
  }
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  
  cov_env <- new.env(parent = baseenv())
  cov_env$work_dir <- work_dir
  cov_env$pkg_root <- normalizePath(pkg_source_path, winslash = "/", mustWork = TRUE)
  cov_env
}

