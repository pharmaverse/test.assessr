#' Run Coverage Analysis with Test Detection
#'
#' This function checks the test configuration of an R package and runs code coverage analysis
#' using `testthat`, `testit` or non-standard testing frameworks if available. 
#' If no supported test framework is found,
#' it returns a default coverage result indicating no functions were tested.
#'
#' @param pkg_source_path Character. Path to the root directory of the R package source.
#' @param covr_timeout Numeric. Timeout in seconds for running coverage analysis. Default is 60.
#' @param preserve_vars Character. preserve `old` for CRAN examples 
#'
#' @return A list with two elements:
#' \describe{
#'   \item{covr_list}{A list containing coverage results. If no supported tests are found, returns a default structure with zero coverage.}
#'   \item{test_pkg_data}{A list summarizing the test configuration, including presence of `testthat`, `testit`, snapshots, and base R test scripts.}
#' }
#'
#' @examples
#' \dontrun{
#' r = getOption("repos")
#' r["CRAN"] = "http://cran.us.r-project.org"
#' old <- options(repos = r)
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
#' options(old)
#' }
#'
#' @export
run_covr_modes <- function(pkg_source_path,
                           covr_timeout = 60,
                           preserve_vars = "old") {
 
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
  
  if (test_pkg_data$has_testthat | test_pkg_data$has_testit) {
    message("running standard testing framework")
    covr_list <- run_coverage(
      pkg_source_path,
      covr_timeout
    )
    
    if (is.na(covr_list$total_cov) && all(is.na(covr_list$res_cov$coverage$filecoverage))) {
      
      if (test_pkg_data$has_testthat) {
        message("running skip testing with standard testing framework")
        covr_list <- run_covr_skip_stf(pkg_source_path, test_pkg_data)
        
      } else if (test_pkg_data$has_testit) {
        message("running skip testing with non-standard testing framework")
        covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
      }
    }
    
  } else if (test_pkg_data$has_tests_base) {
    message("running non-standard testing framework")
    covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  } else if (test_pkg_data$has_BioG_test) {
    message("running non-standard BioGenerics testing framework")
    covr_list <- run_covr_skip_bioc_nstf(pkg_source_path, test_pkg_data)  
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
  #clean up global env
  cleanup_and_return_null(env = .GlobalEnv, preserve_vars = preserve_vars)
  
  # get R version 
  r_version <- as.character(getRversion())
  
  # add testing framework type
  if (test_pkg_data$has_testthat ) {
     test_framework_type <- "standard testing framework"
  } else if (test_pkg_data$has_testit | test_pkg_data$has_tests_base) {
    test_framework_type <- "non-standard testing framework"
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



#' @title Setup Coverage Environment
#' @description
#' Internal utility to prepare a coverage environment for a package by loading
#' helpers, package code, datasets, and dependencies. Also locates the project root,
#' records the initial environment state, and safely unloads the package to avoid conflicts.
#'
#' @param pkg_source_path Path to the source directory of the package.
#' @param pkg_name Name of the package.
#' @param test_path Path to the testthat directory. Defaults to "tests/testthat".
#'
#' @return A list containing:
#' \describe{
#'   \item{cov_env}{The prepared coverage environment.}
#'   \item{datasets_loaded}{Character vector of loaded dataset names.}
#'   \item{root_dir}{Path to the project root.}
#'   \item{initial_state}{Character vector of object names in the environment before setup.}
#'   \item{helpers_loaded}{Character vector of helper file paths that were sourced.}
#' }
#' @keywords internal
setup_covr_env <- function(pkg_source_path, pkg_name, test_path = "tests/testthat") {
  message(paste0("Setting up coverage environment for: ", pkg_name))
  cov_env <- new.env(parent = globalenv())
  datasets_loaded <- character()
  root_dir <- NULL
  helpers_loaded <- character()
  
  # Record initial state before setup
  initial_state <- ls(envir = cov_env, all.names = TRUE)
  
  # Create a special helper file 
  # to set up local_test_directory when 
  # environment_coverage() runs tests, 
  # they behave as if they were in the package’s normal test environment.
  tryCatch({
    special_helper <- file.path(test_path, "helper_covr_env.R")
    writeLines(
      c(
        "# Auto-generated helper for coverage environment",
        "setup_local_test_dir <- function(test_path = 'tests/testthat') {",
        "  testthat::local_test_directory(test_path)",
        "}"
      ),
      con = special_helper
    )
    message("Special helper file created: ", special_helper)
  }, error = function(e) {
    message("Failed to create special helper file: ", e$message)
  })
  
  tryCatch({
    # Load helpers
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
    
    # Load package code
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
    
    # Locate project root
    message(paste0("Locating project root for: ", pkg_name))
    root_dir <- find_project_root()
    if (is.null(root_dir)) {
      stop("Project root not found. Coverage cannot be run.")
    }
    
    # Load datasets
    message(paste0("Loading datasets into coverage environment for: ", pkg_name))
    datasets_loaded <- load_package_datasets(pkg_name, env = cov_env)
    message("Datasets loaded: ", paste(datasets_loaded, collapse = ", "))
    
    # Load dependencies
    message(paste0("Loading dependencies into coverage environment for: ", pkg_name))
    load_dependencies_into_env(pkg_source_path, cov_env)
    
    # Unload package to avoid conflicts
    message(paste0("Unloading package: ", pkg_name))
    safe_unload_package(pkg_name)
    
  }, error = function(e) {
    cleanup_and_return_null(paste0("Error during setup for ", pkg_name, " : ", e$message), env = cov_env)
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
#' Loads all datasets from a specified installed package into the global environment.
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
#' and suppresses errors if loading fails. Datasets are loaded into the global environment.
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



#' Clean up global environment and return NULL
#'
#' This internal utility function is used to clean up the global environment
#' by removing all objects and optionally displaying a message before returning `NULL`.
#' It is primarily used within `run_coverage_base()` to ensure consistent cleanup
#' in error-handling scenarios.
#'
#' @param msg Optional character string. If provided, the message will be printed before cleanup.
#' @param env A character string specifying the covr env
#' @param preserve_vars Character. preserve `old` for CRAN examples 
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
cleanup_and_return_null <- function(msg = NULL, 
                                    env = cov_env,
                                    preserve_vars = c("old")
                                    ) {
  if (!is.null(msg)) message(msg)
  
  # Safely clear environment, preserving specified variables
  if (!is.null(env) && is.environment(env)) {
    vars_to_remove <- setdiff(ls(envir = env), preserve_vars)
    rm(list = vars_to_remove, envir = env)
  }
  
  root_dir <- find_project_root()
  if (is.null(root_dir)) {
    message("Project root not found.")
    return(invisible())
  }
  
  # MASS creates "polr.pdf" in project root folder during testing
  # survival creates "silhouette-ex.ps" in project root folder during testing
  
  #  Define extensions to remove
  extensions_to_remove <- c(".pdf", ".ps")
  
  # List all files in root directory
  files_in_root <- list.files(root_dir, full.names = TRUE)
  
  # Filter files by extension
  files_to_remove <- files_in_root[
    grepl(paste0("\\", extensions_to_remove, "$", collapse = "|"), files_in_root)
  ]
  
  # Remove files and return removed file paths
  removed_files <- vapply(files_to_remove, function(file) {
    if (file.exists(file)) {
      try(file.remove(file), silent = TRUE)
      return(file)
    }
    return(NA_character_)
  }, character(1))
  
  # Drop NAs and return sorted paths
  sort(removed_files[!is.na(removed_files)])
  
  
  # clean up and exit silently, 
  # without returning any logicals or other values,
  invisible(sort(removed_files[!is.na(removed_files)]))
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


#' Load Imported and Suggested Packages into Environment
#'
#' This function reads the DESCRIPTION file of a package and loads all packages
#' listed under `Imports` and `Suggests` into the specified environment.
#' This ensures that functions from those packages are available when running
#' coverage with `covr::environment_coverage()`.
#'
#' @param pkg_source_path Path to the source directory of the package.
#' @param env Environment into which the packages should be loaded.
#'
#' @return Invisibly returns TRUE if all packages are loaded successfully.
#' @keywords internal
#' @importFrom utils install.packages installed.packages packageVersion compareVersion
load_dependencies_into_env <- function(pkg_source_path, env) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  
  desc_file <- file.path(pkg_source_path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("DESCRIPTION file not found at: ", desc_file)
  }
  
  desc <- read.dcf(desc_file)
  deps_raw <- c(desc[1, "Imports"], desc[1, "Suggests"])
  deps_raw <- unlist(strsplit(deps_raw, ",\\s*"))
  deps_raw <- deps_raw[deps_raw != "" & !is.na(deps_raw)]
  
  base_pkgs <- rownames(installed.packages(priority = "base"))
  
  parse_dep <- function(dep) {
    dep <- trimws(dep)
    if (dep == "" || is.na(dep)) return(NULL)
    
    matches <- regmatches(dep, regexec("^([A-Za-z0-9\\.]+)\\s*(?:\\(([^)]+)\\))?", dep))[[1]]
    if (length(matches) >= 2) {
      pkg <- trimws(matches[2])
      constraint <- if (length(matches) >= 3 && !is.na(matches[3])) trimws(matches[3]) else NULL
      return(list(pkg = pkg, constraint = constraint))
    } else {
      warning("Could not parse dependency: ", dep)
      return(NULL)
    }
  }
  
  for (dep in deps_raw) {
    parsed <- parse_dep(dep)
    if (is.null(parsed)) next
    
    pkg <- parsed$pkg
    constraint <- parsed$constraint
    
    if (pkg %in% base_pkgs || pkg == "" || is.na(pkg)) next
    
    installed <- pkg %in% rownames(utils::installed.packages())
    needs_install <- FALSE
    
    if (!installed) {
      needs_install <- TRUE
    } else if (!is.null(constraint)) {
      installed_ver <- packageVersion(pkg)
      op_ver <- regmatches(constraint, regexec("([><=]+)\\s*(.*)", constraint))[[1]]
      op <- op_ver[2]
      ver <- op_ver[3]
      
      compare_result <- compareVersion(as.character(installed_ver), ver)
      valid <- switch(op,
                      ">=" = compare_result >= 0,
                      ">"  = compare_result > 0,
                      "<=" = compare_result <= 0,
                      "<"  = compare_result < 0,
                      "==" = compare_result == 0,
                      FALSE)
      
      if (!valid) {
        message("Installed version of ", pkg, " (", installed_ver, ") does not satisfy constraint ", constraint)
        needs_install <- TRUE
      }
    }
    
    if (needs_install) {
      tryCatch({
        if (!is.null(constraint)) {
          remotes::install_version(pkg, version = ver, upgrade = "never", force = TRUE)
        } else {
          install.packages(pkg)
        }
      }, error = function(e) {
        message("Failed to install package: ", pkg, " - ", e$message)
      })
    }
    
    tryCatch({
      suppressWarnings(library(pkg, character.only = TRUE))
    }, warning = function(w) {
      message("Warning while loading ", pkg, ": ", conditionMessage(w))
    }, error = function(e) {
      message("Failed to load package: ", pkg, " - ", e$message)
    })
  }
  
  invisible(TRUE)
}
