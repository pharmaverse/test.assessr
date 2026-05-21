#' Detect which test frameworks are present in a package
#'
#' @param test_pkg_data List from \code{check_pkg_tests_and_snaps}.
#' @return Character vector of framework IDs: "testthat", "testit", "tests_base",
#'   "tinytest", "data.table", "bioconductor".
#' @keywords internal
detect_frameworks <- function(test_pkg_data) {
  fw <- character(0)
  if (isTRUE(test_pkg_data$has_testthat))         fw <- c(fw, "testthat")
  if (isTRUE(test_pkg_data$has_testit))           fw <- c(fw, "testit")
  if (isTRUE(test_pkg_data$has_tests_base))       fw <- c(fw, "tests_base")
  if (isTRUE(test_pkg_data$has_tinytest))         fw <- c(fw, "tinytest")
  if (isTRUE(test_pkg_data$has_data_table_tests)) fw <- c(fw, "data.table")
  if (isTRUE(test_pkg_data$has_BioG_test))        fw <- c(fw, "bioconductor")
  if (isTRUE(test_pkg_data$has_RUnit_test) &&
      !isTRUE(test_pkg_data$has_BioG_test))       fw <- c(fw, "runit")
  unique(fw)
}


#' Compute aggregate total coverage across multiple frameworks
#'
#' @param per_framework Named list of covr results from each framework.
#' @return Numeric. Mean of per-framework total_cov (0-100), or 0 if none.
#' @keywords internal
compute_multi_framework_total <- function(per_framework) {
  out <- 0
  if (length(per_framework) > 0) {
    covs <- vapply(per_framework, function(x) {
      tryCatch(as.numeric(x$total_cov)[1L], error = function(e) 0)
    }, FUN.VALUE = numeric(1))
    non_na_covs <- covs[!is.na(covs)]
    if (length(non_na_covs) > 0) {
      out <- mean(non_na_covs)
    }
  }
  return(out)
}


#' Get human-readable test framework type label
#'
#' @param test_pkg_data List from \code{check_pkg_tests_and_snaps}.
#' @param framework_id Character. Framework ID from \code{detect_frameworks}.
#' @return Character. Label for test_framework_type.
#' @keywords internal
get_test_framework_type <- function(test_pkg_data, framework_id) {
  switch(
    framework_id,
    testthat = "standard testing framework",
    tinytest = "non-standard testing framework - tinytest",
    "data.table" = "non-standard testing framework - data.table",
    runit = "non-standard testing framework - RUnit",
    bioconductor = "non-standard testing framework - Bioconductor",
    testit       = "non-standard testing framework - testit",
    "non-standard testing framework"
  )
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
  
  
  # Use the pre-existing binding in the package namespace
  e <- cov_env   # this resolves to test.assessr:::cov_env inside package code
  # Optionally clear only the keys you own to avoid leaking prior state across runs
  rm(list = intersect(ls(e), c("work_dir","pkg_root","package","framework","pkg_path")), envir = e)
  
  e$work_dir  <- work_dir
  e$pkg_root  <- normalizePath(pkg_source_path, winslash = "/", mustWork = TRUE)
  e$package   <- pkg
  e$framework <- framework
  e$pkg_path  <- pkg_source_path
  e
  
}

#' Decide and run the appropriate coverage path for a test framework
#'
#' Self-contained helper used by `run_one_framework()` to decide whether to run
#' standard (testthat) coverage via `run_coverage()` or non-standard (testit)
#' coverage via `run_covr_skip_nstf()`. The decision is made by inspecting files
#' under `<pkg>/tests/` directly (no reliance on `check_pkg_tests_and_snaps()`).
#'
#' Precedence (when framework == "testit"):
#'   1) If a testthat-style driver is detected -> run_coverage()   
#'   2) Else if a testit driver/structure is detected -> NSTF     
#'
#' @param framework Character scalar (e.g., "testthat", "testit").
#' @param pkg_source_path Character scalar; path to the package root.
#' @param test_pkg_data List; kept for signature compatibility.
#' @param cov_env Environment; coverage execution environment (forwarded).
#' @param covr_timeout Numeric/integer; timeout for `run_coverage()`.
#' @return list(covr_list = <coverage>, tf_type = <label>)
#' @keywords internal
#' @noRd
run_covr_decider <- function(framework,
                             pkg_source_path,
                             test_pkg_data,   
                             cov_env,
                             covr_timeout) {
  
  pkg_source_path <- normalizePath(pkg_source_path, mustWork = TRUE)
  tests_dir       <- file.path(pkg_source_path, "tests")
  test_all_file   <- file.path(tests_dir, "test-all.R")
  testthat_file   <- file.path(tests_dir, "testthat.R")
  testthat_dir    <- file.path(tests_dir, "testthat")
  testit_dir      <- file.path(tests_dir, "testit")
  
  read_safely <- function(path) {
    if (!file.exists(path)) return(character())
    tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  }
  
  # --- Detect drivers directly from test files (no external state) -----------
  txt_all   <- read_safely(test_all_file)
  txt_tthat <- read_safely(testthat_file)
  
  # testit driver: accept namespaced or bare test_pkg(...)
  detects_test_pkg <- function(x) any(grepl("\\b(?:testit\\s*::\\s*)?test_pkg\\s*\\(", x))
  # testthat driver: test_package(...) or test_check(...)
  detects_test_package <- function(x) any(grepl("(?:testthat\\s*::\\s*)?(test_package|test_check)\\s*\\(", x))
  
  has_testit_driver_text    <- detects_test_pkg(txt_all) || detects_test_pkg(txt_tthat)
  has_testthat_driver_text  <- detects_test_package(txt_all) || detects_test_package(txt_tthat)
  has_test_all              <- file.exists(test_all_file)
  has_testthat_structure    <- dir.exists(testthat_dir)
  has_testit_structure      <- dir.exists(testit_dir)
  
  # ---------------------- Decision precedence --------------------------------
  # testthat -> always standard
  if (identical(framework, "testthat")) {
    message("decider: testthat -> run_coverage()")
    covr_list <- run_coverage(pkg_source_path, covr_timeout)
    return(list(covr_list = covr_list, tf_type = "standard testing framework"))
  }
  
  # testit -> prefer "line 65" (run_coverage) for DT-like layout:
  #           tests/test-all.R present AND NO tests/testthat/ directory
  #        -> else NSTF if we see explicit testit signals
  #        -> else standard if we see explicit testthat driver
  #        -> else NSTF default
  if (identical(framework, "testit")) {
    if (has_test_all && !has_testthat_structure) {
      message("decider: tests/test-all.R present and no tests/testthat/ -> run_coverage()  [DT-style]")
      covr_list <- run_coverage(pkg_source_path, covr_timeout)
      return(list(covr_list = covr_list, tf_type = "standard testing framework - testit"))
    }
    if (has_testit_driver_text || has_testit_structure) {
      message("decider: testit driver/structure detected -> run_covr_skip_nstf()")
      covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env, framework = "testit")
      return(list(covr_list = covr_list, tf_type = "non-standard testing framework - testit"))
    }
    if (has_testthat_driver_text || has_testthat_structure) {
      message("decider: testthat-style driver/structure detected -> run_coverage()")
      covr_list <- run_coverage(pkg_source_path, covr_timeout)
      return(list(covr_list = covr_list, tf_type = "standard testing framework"))
    }
    message("decider: no explicit driver found for testit -> default run_covr_skip_nstf()")
    covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env, framework = "testit")
    return(list(covr_list = covr_list, tf_type = "non-standard testing framework - testit"))
  }
  
}

#' Run coverage for a single test framework
#'
#' @param framework Character. Framework ID.
#' @param pkg Character. Package name.
#' @param test_pkg_data List from \code{check_pkg_tests_and_snaps}.
#' @param pkg_source_path Character. Path to package source.
#' @param covr_timeout Numeric. Timeout for coverage.
#' @return List with total_cov, res_cov, test_framework_type, framework_id.
#' @keywords internal
run_one_framework <- function(framework,
                              pkg,
                              test_pkg_data,
                              pkg_source_path,
                              covr_timeout) {
  
  safe_unload_package(pkg)
  
  cov_env <- initialise_covr_env(pkg_source_path, pkg, framework = framework)
  on.exit(
    cleanup_and_return_null(
      msg = sprintf("Cleaning coverage workspace for %s", pkg),
      env = cov_env
    ),
    add = TRUE
  )
  
  
  if (framework %in% c("testthat", "testit")) {
    # Decide and execute the correct runner
    res <- run_covr_decider(
      framework       = framework,
      pkg_source_path = pkg_source_path,
      test_pkg_data   = test_pkg_data,
      cov_env         = cov_env,
      covr_timeout    = covr_timeout
    )
    
    covr_list <- res$covr_list
    tf_type   <- res$tf_type
    
    # Keep your original "skip" fallback for testthat only
    if (identical(framework, "testthat")) {
      fc <- tryCatch(covr_list$res_cov$coverage$filecoverage, error = function(e) NULL)
      if (isTRUE(is.na(covr_list$total_cov)) && (is.null(fc) || all(is.na(fc)))) {
        message("running skip testing with standard testing framework")
        covr_list <- run_covr_skip_stf(pkg_source_path, test_pkg_data, cov_env)
      }
    }
    
    
  } else if (framework == "tests_base") {
    message("running non-standard testing framework")
    covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env, framework = framework)
    tf_type <- "non-standard testing framework"
    
  } else if (framework == "tinytest") {
    message("running non-standard tinytest testing framework")
    covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data, cov_env, framework = framework)
    tf_type <- "non-standard testing framework - tinytest"
    
  } else if (framework == "data.table") {
    message("running non-standard data.table testing framework")
    covr_list <- run_covr_skip_dt_nstf(pkg_source_path, test_pkg_data, cov_env)
    tf_type <- "non-standard testing framework - data.table"
    
  }  else if (test_pkg_data$has_RUnit_test && !test_pkg_data$has_BioG_test) {
    message("running non-standard RUnit testing framework")
    covr_list <- run_covr_skip_runit_nstf(pkg_source_path, test_pkg_data, cov_env, framework = framework)    
    tf_type <- "non-standard testing framework - RUnit"  
    
  } else if (framework == "bioconductor") {
    message("running non-standard Bioconductor testing framework")
    covr_list <- run_covr_skip_bioc_nstf(pkg_source_path, test_pkg_data, cov_env, framework = framework)
    tf_type <- "non-standard testing framework - Bioconductor"
    
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
    tf_type <- "non-standard testing framework"
  }
  
  if (is.null(covr_list)) {
    covr_list <- 
      create_empty_covr_list(pkg, 
                             paste0("Coverage failed for framework: ", framework),
                             test_pkg_data = test_pkg_data)
  }
  
  covr_list$test_framework_type <- tf_type
  covr_list$framework_id <- framework
  
  
  return(covr_list)
}


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
                           covr_timeout = 60) {
  
  # --- Temporarily set CRAN repo (restore immediately on exit) ---
  old_repos <- getOption("repos")
  tmp_repos <- old_repos
  tmp_repos["CRAN"] <- "http://cran.us.r-project.org"
  options(repos = tmp_repos)
  on.exit(options(repos = old_repos), add = TRUE)
  
  # ---- metadata ------------------------------------------------------------
  pkg <- get_pkg_name(pkg_source_path)
  pkg_desc <- get_pkg_desc(pkg_source_path, fields = c("Package", "Version"))
  pkg_name <- pkg_desc$Package
  pkg_ver <- pkg_desc$Version
  metadata <- get_test_metadata()
  r_version <- as.character(getRversion())
  
  # ---- detect frameworks ---------------------------------------------------
  test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
  frameworks_found <- detect_frameworks(test_pkg_data)
  
  # ---- initialize base result structure ------------------------------------
  result <- list(
    pkg_name = pkg_name,
    pkg_ver = pkg_ver,
    date_time = metadata$datetime,
    executor = metadata$executor,
    sysname = metadata$info$sys$sysname,
    version = metadata$info$sys$version,
    release = metadata$info$sys$release,
    machine = metadata$info$sys$machine,
    r_version = r_version
  )
  
  # ---- no framework: original fallback -------------------------------------
  if (length(frameworks_found) == 0) {
    message("No recognised standard or non-standard testing configuration")
    result$test_framework_type <- "non-standard testing framework"
    result$total_cov <- 0
    result$res_cov <- list(
      name = pkg,
      coverage = list(
        filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
        totalcoverage = 0
      ),
      errors = "No recognised standard or non-standard testing configuration",
      notes = NA
    )
  }
  # ---- single framework: preserve original return shape --------------------
  else if (length(frameworks_found) == 1) {
    one <- run_one_framework(
      framework = frameworks_found[[1]],
      pkg = pkg,
      test_pkg_data = test_pkg_data,
      pkg_source_path = pkg_source_path,
      covr_timeout = covr_timeout
    )
    one$test_framework_type <- NULL
    one$framework_id <- NULL
    one$test_pkg_data <- NULL
    
    result$test_framework_type <- get_test_framework_type(test_pkg_data, frameworks_found[[1]])
    result <- c(result, one)
  }
  # ---- multiple frameworks: run each independently -------------------------
  else {
    message("Multiple test frameworks detected: ", paste(frameworks_found, collapse = ", "))
    
    per_framework <- lapply(frameworks_found, function(fr) {
      run_one_framework(
        framework = fr,
        pkg = pkg,
        test_pkg_data = test_pkg_data,
        pkg_source_path = pkg_source_path,
        covr_timeout = covr_timeout
      )
    })
    names(per_framework) <- frameworks_found
    
    # Package-level test metadata belongs only in result$test_pkg_data (below).
    # run_one_framework / create_empty_covr_list may attach copies per block.
    per_framework <- lapply(per_framework, function(x) {
      x$test_pkg_data <- NULL
      x
    })
    
    total_cov <- compute_multi_framework_total(per_framework)
    
    result$multi_framework <- TRUE
    result$frameworks <- frameworks_found
    result$total_cov <- total_cov
    result$results <- per_framework
  }
  
  # ---- add test_pkg_data just before return --------------------------------
  result$test_pkg_data <- test_pkg_data
  
  # ---- single return statement at end ---------------------------------------
  return(result)
}


#' Set up a coverage environment for isolated test execution
#'
#' Prepare an existing coverage environment for use with coverage workflows
#' that evaluate tests in an isolated environment, typically one created
#' with `new.env(parent = baseenv())`.
#'
#' The setup process:
#'
#' - loads helper files from `tests/testthat`,
#' - loads package namespace objects into `cov_env`,
#' - resolves the project root,
#' - loads package datasets into `cov_env`,
#' - loads Base R datasets and exports into `cov_env`,
#' - optionally loads `nycflights13` datasets if available,
#' - loads dependencies into `cov_env`, and
#' - unloads the package namespace afterward to reduce conflicts.
#'
#' This function does **not** create `cov_env`; it expects an existing
#' environment to be supplied.
#'
#' @param pkg_source_path Character scalar. Path to the source package root.
#' @param pkg_name Character scalar. Package name.
#' @param test_path Character scalar. Path to the test directory.
#'   Defaults to `"tests/testthat"`.
#' @param cov_env Environment. A pre-existing coverage environment, usually
#'   created by an initializer such as `initialise_covr_env()`.
#'
#' @return A named list with components:
#' \describe{
#'   \item{cov_env}{The populated coverage environment.}
#'   \item{datasets_loaded}{Character vector of dataset objects loaded into
#'   `cov_env` from the target package, `datasets`, and optionally
#'   `nycflights13`.}
#'   \item{base_r_datasets_loaded}{Character vector of datasets loaded from
#'   the `datasets` package.}
#'   \item{base_r_exports_loaded}{Character vector of exported objects copied
#'   from Base R packages other than `base` and `datasets`.}
#'   \item{root_dir}{Resolved project root path.}
#'   \item{initial_state}{Character vector of object names present in
#'   `cov_env` before setup.}
#'   \item{helpers_loaded}{Character vector of helper files successfully
#'   sourced into `cov_env`.}
#' }
#'
#' @details
#' This function is designed for workflows where `cov_env` is deliberately
#' isolated (for example with `parent = baseenv()`). In such cases, objects
#' from packages like `stats` and `datasets` are not automatically visible,
#' so they are explicitly loaded into `cov_env`.
#'
#' The package `nycflights13` is treated as optional: if it is installed,
#' its datasets are loaded into `cov_env`; otherwise it is silently skipped.
#'
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
  
  checkmate::assert_string(pkg_source_path, .var.name = "pkg_source_path")
  checkmate::assert_string(pkg_name, .var.name = "pkg_name")
  checkmate::assert_directory_exists(test_path, .var.name = "test_path")
  checkmate::assert_environment(cov_env, .var.name = "cov_env")
  
  datasets_loaded <- character()
  base_r_datasets_loaded <- character()
  base_r_exports_loaded <- character()
  root_dir <- NULL
  helpers_loaded <- character()
  
  # Record initial state before setup
  initial_state <- ls(envir = cov_env, all.names = TRUE)
  
  tryCatch({
    # ------------------------------------------------------------------
    # Load helper files (READ-ONLY from tests/, no writes there)
    # ------------------------------------------------------------------
    message(paste0("Loading helpers into coverage environment for: ", pkg_name))
    helper_files <- list.files(
      test_path,
      pattern = "^helper.*\\.R$",
      full.names = TRUE
    )
    
    for (file in helper_files) {
      tryCatch({
        sys.source(file, envir = cov_env)
        helpers_loaded <- c(helpers_loaded, file)
      }, error = function(e) {
        message("Failed to source helper file: ", file, " : ", e$message)
      })
    }
    
    # ------------------------------------------------------------------
    # Load package code into namespace, then copy objects into cov_env
    # ------------------------------------------------------------------
    message(paste0("Loading package into coverage environment for: ", pkg_name))
    pkgload::load_all(
      pkg_source_path,
      helpers = FALSE,
      attach = FALSE,
      quiet = TRUE
    )
    
    pkg_ns <- asNamespace(pkg_name)
    pkg_all <- ls(envir = pkg_ns, all.names = TRUE)
    
    for (obj in pkg_all) {
      tryCatch({
        assign(
          obj,
          get(obj, envir = pkg_ns, inherits = FALSE),
          envir = cov_env
        )
      }, error = function(e) {
        message("Failed to assign object ", obj, " : ", e$message)
      })
    }
    
    # ------------------------------------------------------------------
    # Locate project root (read-only)
    # ------------------------------------------------------------------
    message(paste0("Locating project root for: ", pkg_name))
    root_dir <- find_project_root()
    if (is.null(root_dir)) {
      stop("Project root not found. Coverage cannot be run.")
    }
    
    # ------------------------------------------------------------------
    # Load package datasets into cov_env
    # ------------------------------------------------------------------
    message(paste0("Loading package datasets into coverage environment for: ", pkg_name))
    datasets_loaded <- load_package_datasets(
      pkg_names = pkg_name,
      env = cov_env,
      source_paths = stats::setNames(list(pkg_source_path), pkg_name),
      overwrite = FALSE
    )
    
    # ------------------------------------------------------------------
    # Load Base R package surface into cov_env
    # ------------------------------------------------------------------
    message("Loading Base R packages into coverage environment")
    base_r_loaded <- load_base_r_packages_into_env(
      env = cov_env,
      overwrite = FALSE
    )
    
    base_r_exports_loaded <- base_r_loaded$exports_loaded
    base_r_datasets_loaded <- base_r_loaded$datasets_loaded
    
    # ------------------------------------------------------------------
    # Optional: load nycflights13 datasets if installed
    # ------------------------------------------------------------------
    message("Loading optional nycflights13 datasets into coverage environment")
    nycflights_loaded <- load_package_datasets(
      pkg_names = "nycflights13",
      env = cov_env,
      source_paths = NULL,
      overwrite = FALSE
    )
    
    datasets_loaded <- unique(c(
      datasets_loaded,
      base_r_datasets_loaded,
      nycflights_loaded
    ))
    
    if (length(datasets_loaded) > 0L) {
      message("Datasets loaded: ", paste(datasets_loaded, collapse = ", "))
    } else {
      message("No datasets were loaded into coverage environment.")
    }
    
    message("Base R exports loaded: ", length(base_r_exports_loaded))
    
    # ------------------------------------------------------------------
    # Load dependencies into env
    # ------------------------------------------------------------------
    message(paste0("Loading dependencies into coverage environment for: ", pkg_name))
    load_dependencies_into_env(pkg_source_path, cov_env)
    
    # ------------------------------------------------------------------
    # Optional sanity checks
    # ------------------------------------------------------------------
    if (!exists("rnorm", envir = cov_env, inherits = FALSE)) {
      message("Note: `rnorm` was not loaded into cov_env.")
    }
    if (!exists("mtcars", envir = cov_env, inherits = FALSE)) {
      message("Note: `mtcars` was not loaded into cov_env.")
    }
    if (!exists("iris", envir = cov_env, inherits = FALSE)) {
      message("Note: `iris` was not loaded into cov_env.")
    }
    
    # ------------------------------------------------------------------
    # Unload package to avoid conflicts after copying objects into cov_env
    # ------------------------------------------------------------------
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
    base_r_datasets_loaded = base_r_datasets_loaded,
    base_r_exports_loaded = base_r_exports_loaded,
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
#' @param pkg_names A character string specifying the name of the package.
#' @param env A character string specifying the covr env
#' @param source_paths A character string specifying the source paths
#' @param overwrite A logical specifying whether datasets should be overwritten
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
load_package_datasets <- function(pkg_names,
                                  env,
                                  source_paths = NULL,
                                  overwrite = FALSE) {
  checkmate::assert_character(
    pkg_names,
    min.len = 1,
    any.missing = FALSE,
    .var.name = "pkg_names"
  )
  checkmate::assert_environment(env, .var.name = "env")
  checkmate::assert_flag(overwrite, .var.name = "overwrite")
  
  # source_paths can be:
  # - NULL
  # - a named character vector/list, e.g. c("mypkg" = "/path/to/src")
  if (!is.null(source_paths)) {
    if (!(is.character(source_paths) || is.list(source_paths))) {
      stop("`source_paths` must be NULL, a named character vector, or a named list.")
    }
    if (is.null(names(source_paths)) || any(names(source_paths) == "")) {
      stop("`source_paths` must be named by package name.")
    }
    source_paths <- as.list(source_paths)
  }
  
  loaded <- character()
  
  load_from_file <- function(file, env, overwrite = FALSE) {
    ext <- tolower(tools::file_ext(file))
    before <- ls(envir = env, all.names = TRUE)
    
    out <- tryCatch({
      if (ext %in% c("rda", "rdata")) {
        load(file, envir = env)
        
      } else if (ext %in% c("tab", "tsv")) {
        nm <- tools::file_path_sans_ext(basename(file))
        if (overwrite || !exists(nm, envir = env, inherits = FALSE)) {
          assign(
            nm,
            utils::read.table(file, header = TRUE, sep = "\t"),
            envir = env
          )
        }
        
      } else if (ext == "csv") {
        nm <- tools::file_path_sans_ext(basename(file))
        if (overwrite || !exists(nm, envir = env, inherits = FALSE)) {
          assign(
            nm,
            utils::read.csv(file, header = TRUE),
            envir = env
          )
        }
        
      } else if (ext == "txt") {
        nm <- tools::file_path_sans_ext(basename(file))
        if (overwrite || !exists(nm, envir = env, inherits = FALSE)) {
          assign(
            nm,
            utils::read.table(file, header = TRUE),
            envir = env
          )
        }
        
      } else if (ext == "json") {
        nm <- tools::file_path_sans_ext(basename(file))
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
          stop("jsonlite package required to load JSON datasets.")
        }
        if (overwrite || !exists(nm, envir = env, inherits = FALSE)) {
          assign(
            nm,
            jsonlite::fromJSON(file),
            envir = env
          )
        }
        
      } else if (ext == "r") {
        sys.source(file, envir = env)
        
      } else {
        return(character(0))
      }
      
      after <- ls(envir = env, all.names = TRUE)
      setdiff(after, before)
    }, error = function(e) {
      message("Failed to load file ", file, " : ", e$message)
      character(0)
    })
    
    out
  }
  
  load_from_source_package <- function(pkg, pkg_source_path, env, overwrite = FALSE) {
    if (is.null(pkg_source_path) || !nzchar(pkg_source_path)) {
      return(character(0))
    }
    
    data_path <- file.path(pkg_source_path, "data")
    if (!dir.exists(data_path)) {
      return(character(0))
    }
    
    files <- list.files(data_path, full.names = TRUE)
    out <- character()
    
    for (file in files) {
      new_objs <- load_from_file(file, env = env, overwrite = overwrite)
      out <- c(out, new_objs)
    }
    
    unique(out)
  }
  
  load_from_installed_package <- function(pkg, env, overwrite = FALSE) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Package ", pkg, " is not installed; skipping installed datasets.")
      return(character(0))
    }
    
    info <- utils::data(package = pkg)$results
    if (is.null(info) || NROW(info) == 0L) {
      return(character(0))
    }
    
    item_col <- intersect(c("Item", "item"), colnames(info))
    if (length(item_col) == 0L) {
      message("Could not identify dataset column for package: ", pkg)
      return(character(0))
    }
    item_col <- item_col[[1]]
    
    dataset_items <- unique(info[, item_col])
    out <- character()
    
    for (dataset in dataset_items) {
      if (!overwrite && exists(dataset, envir = env, inherits = FALSE)) {
        out <- c(out, paste0(pkg, "::", dataset))
        next
      }
      
      before <- ls(envir = env, all.names = TRUE)
      
      ok <- tryCatch({
        utils::data(list = dataset, package = pkg, envir = env)
        TRUE
      }, error = function(e) {
        message("Failed to load dataset ", pkg, "::", dataset, " : ", e$message)
        FALSE
      })
      
      if (isTRUE(ok)) {
        after <- ls(envir = env, all.names = TRUE)
        new_objs <- setdiff(after, before)
        
        # In most cases utils::data() creates an object with the dataset name,
        # but record actual new objects if names differ.
        if (exists(dataset, envir = env, inherits = FALSE)) {
          out <- c(out, paste0(pkg, "::", dataset))
        } else if (length(new_objs) > 0L) {
          out <- c(out, paste0(pkg, "::", new_objs))
        }
      }
    }
    
    unique(out)
  }
  
  for (pkg in unique(pkg_names)) {
    pkg_loaded <- character()
    
    # 1) Try source package data first if a source path is provided for this package
    pkg_source_path <- NULL
    if (!is.null(source_paths) && pkg %in% names(source_paths)) {
      pkg_source_path <- source_paths[[pkg]]
    }
    
    source_loaded <- load_from_source_package(
      pkg = pkg,
      pkg_source_path = pkg_source_path,
      env = env,
      overwrite = overwrite
    )
    pkg_loaded <- c(pkg_loaded, source_loaded)
    
    # 2) Also try installed package datasets
    installed_loaded <- load_from_installed_package(
      pkg = pkg,
      env = env,
      overwrite = overwrite
    )
    pkg_loaded <- c(pkg_loaded, installed_loaded)
    
    loaded <- unique(c(loaded, pkg_loaded))
  }
  
  unique(loaded)
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
                                    env = cov_env) {
  if (!is.null(msg)) message(msg)
  
  # --- NEW: Snapshot coverage context BEFORE clearing env ---
  pkg_root_val <- try({
    if (base::exists("pkg_root", envir = env, inherits = FALSE)) {
      base::get("pkg_root", envir = env, inherits = FALSE)
    } else {
      NULL
    }
  }, silent = TRUE)
  if (inherits(pkg_root_val, "try-error")) pkg_root_val <- NULL
  
  work_dir_val <- try({
    if (base::exists("work_dir", envir = env, inherits = FALSE)) {
      base::get("work_dir", envir = env, inherits = FALSE)
    } else {
      NULL
    }
  }, silent = TRUE)
  if (inherits(work_dir_val, "try-error")) work_dir_val <- NULL
  # --- END NEW ---
  
  # 1) Clear all bindings from the provided env (as before)
  to_rm <- tryCatch(
    base::ls(envir = env, all.names = TRUE),
    error = function(e) base::ls(envir = env) # fall back if a stub doesn't accept all.names
  )
  if (length(to_rm)) {
    for (sym in to_rm) {
      if (base::exists(sym, envir = env, inherits = FALSE)) {
        base::rm(list = sym, envir = env)
      }
    }
  }
  
  # 2) ALWAYS try to remove the temporary workspace referenced by work_dir_val
  try({
    if (is.character(work_dir_val) && nzchar(work_dir_val)) {
      td_norm <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
      wd_norm <- tryCatch(
        normalizePath(work_dir_val, winslash = "/", mustWork = FALSE),
        error = function(e) work_dir_val
      )
      # Only remove if the workspace lives under the session tempdir()
      if (startsWith(paste0(wd_norm, "/"), paste0(td_norm, "/")) && dir.exists(work_dir_val)) {
        unlink(work_dir_val, recursive = TRUE, force = TRUE)
      }
    }
  }, silent = TRUE)
  
  # 3) Orphan cleanup with a safer scope
  try({
    safe_roots <- character(0)
    
    # 3a) Preferred root: pkg_root_val (from snapshot)
    pkg_root <- NULL
    if (is.character(pkg_root_val) && length(pkg_root_val) == 1L && nzchar(pkg_root_val) && dir.exists(pkg_root_val)) {
      pkg_root <- tryCatch(
        normalizePath(pkg_root_val, winslash = "/", mustWork = FALSE),
        error = function(e) NULL
      )
    }
    if (!is.null(pkg_root)) {
      safe_roots <- c(safe_roots, pkg_root)
    }
    
    # 3b) Include getwd() only if it is a descendant of pkg_root (safe & complete)
    cwd_norm <- tryCatch(
      normalizePath(getwd(), winslash = "/", mustWork = FALSE),
      error = function(e) NULL
    )
    if (!is.null(pkg_root) && !is.null(cwd_norm)) {
      pkg_root_slash <- paste0(pkg_root, "/")
      cwd_slash <- paste0(cwd_norm, "/")
      if (startsWith(cwd_slash, pkg_root_slash)) {
        safe_roots <- c(safe_roots, cwd_norm)
      }
    }
    
    # 3c) Include work_dir_val if it is under tempdir()
    td_norm <- tryCatch(
      normalizePath(tempdir(), winslash = "/", mustWork = FALSE),
      error = function(e) NULL
    )
    if (is.character(work_dir_val) && nzchar(work_dir_val)) {
      wd_norm <- tryCatch(
        normalizePath(work_dir_val, winslash = "/", mustWork = FALSE),
        error = function(e) work_dir_val
      )
      if (!is.null(td_norm) &&
          startsWith(paste0(wd_norm, "/"), paste0(td_norm, "/")) &&
          dir.exists(work_dir_val)) {
        safe_roots <- c(safe_roots, wd_norm)
      }
    }
    
    safe_roots <- unique(safe_roots[!is.na(safe_roots) & nzchar(safe_roots)])
    
    if (!length(safe_roots)) {
      message("No safe roots found for orphan cleanup; skipping.")
    } else {
      # Known offenders from MASS and survival
      known_names <- c("polr.pdf", "silhouette-ex.ps")
      
      # Extensions removable only in ephemeral roots (e.g., temp work dirs)
      extensions_to_remove <- c(".pdf", ".ps")
      pat_ext <- paste0("\\", extensions_to_remove, "$", collapse = "|")
      
      for (root in safe_roots) {
        files_in_root <- tryCatch(
          list.files(root, full.names = TRUE),
          error = function(e) character(0)
        )
        
        # Always allow deleting the known filenames inside pkg_root or its subdir
        by_name <- files_in_root[basename(files_in_root) %in% known_names]
        
        # Allow deleting any *.pdf/*.ps only if the root is under tempdir()
        by_ext <- character(0)
        if (!is.null(td_norm) &&
            startsWith(paste0(root, "/"), paste0(td_norm, "/"))) {
          by_ext <- files_in_root[grepl(pat_ext, files_in_root)]
        }
        
        files_to_remove <- unique(c(by_name, by_ext))
        
        if (length(files_to_remove)) {
          invisible(vapply(files_to_remove, function(file) {
            if (file.exists(file)) {
              try(file.remove(file), silent = TRUE)
            }
            TRUE
          }, logical(1)))
        }
      }
    }
  }, silent = TRUE)
  
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
#' @param test_pkg_data Optional list from \code{check_pkg_tests_and_snaps()};
#'   use \code{NULL} when unavailable (default).
#'
#' @return A named list containing default coverage values, the provided error message,
#' and placeholder notes.
#'
#' @keywords internal
create_empty_covr_list <- function(pkg_name, error_message, test_pkg_data = NULL) {
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
    ),
    test_pkg_data = test_pkg_data
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

#' Load Base R packages into an isolated coverage environment
#'
#' Populate an isolated environment (typically created with
#' `new.env(parent = baseenv())`) with objects needed to mimic the
#' standard Base R package surface during coverage or test evaluation.
#'
#' The `base` package itself is already available via `baseenv()` and is
#' therefore not copied into `env`. The function instead:
#'
#' - loads datasets from the `datasets` package into `env`, and
#' - copies exported objects from other Base R packages into `env`
#'   (e.g. `stats`, `utils`, `graphics`, `grDevices`, `methods`, etc.).
#'
#' This is useful when tests are evaluated in a deliberately isolated
#' environment whose parent is `baseenv()`, because objects from packages
#' such as `stats` (e.g. `rnorm`) and `datasets` (e.g. `mtcars`, `iris`)
#' are not automatically visible there.
#'
#' @param env An environment to populate.
#' @param overwrite Logical; if `TRUE`, overwrite objects already present in
#'   `env`. If `FALSE` (default), existing objects are left unchanged.
#'
#' @return A named list with two character vectors:
#' \describe{
#'   \item{exports_loaded}{Exported objects copied from Base R packages
#'   other than `base` and `datasets`.}
#'   \item{datasets_loaded}{Dataset objects loaded from the `datasets`
#'   package.}
#' }
#'
#' @details
#' The Base R package set handled here is:
#' \itemize{
#'   \item `base`
#'   \item `compiler`
#'   \item `datasets`
#'   \item `graphics`
#'   \item `grDevices`
#'   \item `grid`
#'   \item `methods`
#'   \item `parallel`
#'   \item `splines`
#'   \item `stats`
#'   \item `stats4`
#'   \item `tcltk`
#'   \item `tools`
#'   \item `utils`
#' }
#'
#' Because `base` is already reachable through `baseenv()`, it is excluded
#' from the explicit copy step.
#'
#' @keywords internal
load_base_r_packages_into_env <- function(env,
                                          overwrite = FALSE) {
  checkmate::assert_environment(env, .var.name = "env")
  checkmate::assert_flag(overwrite, .var.name = "overwrite")
  
  # Base R package set (excluding "translations" per user request)
  base_r_pkgs <- c(
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "utils"
  )
  
  # `base` is already available via parent = baseenv()
  export_pkgs <- setdiff(base_r_pkgs, c("base", "datasets"))
  
  exports_loaded <- load_package_exports_into_env(
    pkg_names = export_pkgs,
    env = env,
    overwrite = overwrite
  )
  
  datasets_loaded <- load_package_datasets(
    pkg_names = "datasets",
    env = env,
    source_paths = NULL,
    overwrite = overwrite
  )
  
  list(
    exports_loaded = exports_loaded,
    datasets_loaded = datasets_loaded
  )
}


#' Load exported objects from installed packages into an environment
#'
#' Copy exported objects (typically functions, but also exported constants or
#' other objects) from one or more installed package namespaces into a target
#' environment.
#'
#' This is useful when evaluating code in an isolated environment, such as one
#' created with `new.env(parent = baseenv())`, where package exports that are
#' normally available on the search path (for example from `stats` or `utils`)
#' are not automatically visible.
#'
#' The function does not attach packages to the search path. Instead, it copies
#' exported objects directly into `env`.
#'
#' @param pkg_names Character vector of package names whose exported objects
#'   should be copied into `env`.
#' @param env Environment into which exported objects should be assigned.
#' @param overwrite Logical; if `TRUE`, overwrite objects already present in
#'   `env`. If `FALSE` (default), existing objects are left unchanged.
#'
#' @return A character vector of fully qualified object names that were loaded,
#'   in the form `"pkg::name"`.
#'
#' @details
#' For each package in `pkg_names`, this function:
#' \enumerate{
#'   \item checks whether the package namespace is available,
#'   \item retrieves the package's exported names,
#'   \item obtains each exported object with `getExportedValue()`,
#'   \item assigns the object into `env`.
#' }
#'
#' Packages that are not installed or cannot be loaded are skipped with a
#' message.
#' 
#' @keywords internal
load_package_exports_into_env <- function(pkg_names,
                                          env,
                                          overwrite = FALSE) {
  checkmate::assert_character(
    pkg_names,
    min.len = 1,
    any.missing = FALSE,
    .var.name = "pkg_names"
  )
  checkmate::assert_environment(env, .var.name = "env")
  checkmate::assert_flag(overwrite, .var.name = "overwrite")
  
  loaded <- character()
  
  for (pkg in unique(pkg_names)) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Package ", pkg, " is not installed or not available; skipping export load.")
      next
    }
    
    exports <- tryCatch(
      getNamespaceExports(pkg),
      error = function(e) {
        message("Failed to retrieve exports for package ", pkg, " : ", e$message)
        character(0)
      }
    )
    
    if (length(exports) == 0L) {
      next
    }
    
    for (nm in exports) {
      if (!overwrite && exists(nm, envir = env, inherits = FALSE)) {
        next
      }
      
      ok <- tryCatch({
        obj <- getExportedValue(pkg, nm)
        assign(nm, obj, envir = env)
        TRUE
      }, error = function(e) {
        message("Failed to load export ", pkg, "::", nm, " : ", e$message)
        FALSE
      })
      
      if (isTRUE(ok)) {
        loaded <- c(loaded, paste0(pkg, "::", nm))
      }
    }
  }
  
  unique(loaded)
}

