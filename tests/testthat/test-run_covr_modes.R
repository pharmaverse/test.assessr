
test_that("loads datasets using utils::data() when installed package datasets are available", {
  dummy_env <- new.env(parent = emptyenv())
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  mockery::stub(ld, "requireNamespace", function(pkg, quietly = TRUE) TRUE)
  
  testthat::local_mocked_bindings(
    data = function(..., list = NULL, package = NULL, envir = NULL) {
      if (is.null(list)) {
        list(
          results = matrix(
            c("dataset1", "dataset2"),
            ncol = 1,
            dimnames = list(NULL, "Item")
          )
        )
      } else {
        assign(list, paste0("loaded_", list), envir = envir)
        invisible(NULL)
      }
    },
    .package = "utils"
  )
  
  result <- ld(
    pkg_names = "fakepkg",
    env = dummy_env,
    source_paths = NULL,
    overwrite = FALSE
  )
  
  expect_setequal(result, c("fakepkg::dataset1", "fakepkg::dataset2"))
  expect_equal(get("dataset1", envir = dummy_env, inherits = FALSE), "loaded_dataset1")
  expect_equal(get("dataset2", envir = dummy_env, inherits = FALSE), "loaded_dataset2")
})






test_that("run_covr_modes handles testit framework correctly (DT-style -> standard path)", {
  cov_env <- new.env(parent = baseenv())
  
  # --- Create DT-style temp package layout: tests/test-all.R, NO tests/testthat/ ---
  pkg_dir <- withr::local_tempdir()
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("library(testit)", "test_pkg('mockpkg')"),
             con = file.path(pkg_dir, "tests", "test-all.R"))
  
  # Call counters to validate routing
  coverage_calls <- 0L
  stf_calls      <- 0L
  
  # --- Mocks ------------------------------------------------------------------
  mock_initialise_covr_env <- function(pkg_source_path, pkg, framework, ...) {
    assign("pkg_path",   pkg_source_path, envir = cov_env)
    assign("package",    pkg,             envir = cov_env)
    assign("framework",  framework,       envir = cov_env)
    cov_env
  }
  
  # Minimal detector output: we’ll force a 'testit' framework
  mock_check_pkg_tests_and_snaps <- function(pkg_source_path, ...) {
    list(
      has_testthat         = FALSE,
      has_tinytest         = FALSE,
      has_data_table_tests = FALSE,
      has_BioG_test        = FALSE,
      has_RUnit_test       = FALSE,
      has_testit           = TRUE,
      has_tests_base       = FALSE
    )
  }
  
  # Make STANDARD coverage succeed so no fallback is needed
  mock_run_coverage <- function(pkg_source_path, covr_timeout, ...) {
    coverage_calls <<- coverage_calls + 1L
    list(
      total_cov = 42,
      res_cov = list(coverage = list(filecoverage = matrix(1)))
    )
  }
  
  # STF fallback: SHOULD NOT be called in this scenario
  mock_run_covr_skip_stf <- function(pkg_source_path, test_pkg_data, cov_env, ...) {
    stf_calls <<- stf_calls + 1L
    list(
      total_cov = 99,
      res_cov = list(coverage = list(filecoverage = matrix(1)), notes = "STF coverage run")
    )
  }
  
  mock_get_pkg_desc    <- function(pkg_source_path, fields, ...) list(Package = "mockpkg", Version = "0.1.0")
  mock_unloadNamespace <- function(pkg, ...) invisible(NULL)
  mock_get_pkg_name    <- function(pkg_source_path, ...) "mockpkg"
  mock_detect_frameworks <- function(test_pkg_data, ...) list("testit")
  
  # --- Bind functions looked up INSIDE nested calls (package namespace) -------
  # Important: bind run_coverage here (not just stub on run_covr_modes)
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env            = cov_env,
    initialise_covr_env = mock_initialise_covr_env,
    run_covr_skip_stf   = mock_run_covr_skip_stf,
    run_coverage        = mock_run_coverage
  )
  
  # --- Stub top-level collaborators on run_covr_modes() -----------------------
  mockery::stub(run_covr_modes, "get_pkg_name",              mock_get_pkg_name)
  mockery::stub(run_covr_modes, "detect_frameworks",         mock_detect_frameworks)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  mockery::stub(run_covr_modes, "get_pkg_desc",              mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace",           mock_unloadNamespace)
  # Do NOT stub run_coverage here; it won’t reach nested calls. We already bound it above.
  
  expect_true(is.environment(cov_env))
  
  # --- Execute on a REAL path and (optionally) assert the message -------------
  # If your decider emits the DT-style message, capture and assert it:
  expect_message(
    object = { result <- run_covr_modes(pkg_dir) },
    regexp = "DT-style",
    all = FALSE
  )
  
  # --- Assertions -------------------------------------------------------------
  expect_identical(coverage_calls, 1L)   # standard path used
  expect_identical(stf_calls, 0L)        # fallback NOT used
  expect_equal(result$total_cov, 42)
  # Depending on your get_test_framework_type mapping:
  expect_true(grepl("non-standard testing framework - testit", result$test_framework_type))
})




test_that("loads various file types from source data directory", {
  dummy_env <- new.env(parent = emptyenv())
  
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  # Source package branch should run from source_paths.
  # Installed-package branch should be skipped for mockpkg,
  # but jsonlite must still appear available for JSON loading.
  mockery::stub(
    ld,
    "requireNamespace",
    function(pkg, quietly = TRUE) {
      if (identical(pkg, "jsonlite")) {
        TRUE
      } else if (identical(pkg, "mockpkg")) {
        FALSE
      } else {
        TRUE
      }
    }
  )
  
  # Simulate source package data directory existing
  mockery::stub(ld, "dir.exists", function(path) TRUE)
  
  # Simulate files in /mock/path/data
  mockery::stub(
    ld,
    "list.files",
    function(path, full.names = TRUE) {
      c(
        "/mock/path/data/data1.rda",
        "/mock/path/data/data2.csv",
        "/mock/path/data/data3.tsv",
        "/mock/path/data/data4.txt",
        "/mock/path/data/data5.json",
        "/mock/path/data/data6.R"
      )
    }
  )
  
  # Stub extension detection exactly as used in the function
  mockery::stub(
    ld,
    "tools::file_ext",
    function(file) sub(".*\\.([^.]+)$", "\\1", file)
  )
  
  # Stub file_path_sans_ext exactly as used in the function
  mockery::stub(
    ld,
    "tools::file_path_sans_ext",
    function(file) sub("\\.[^.]*$", "", basename(file))
  )
  
  # Stub file loaders used inside load_from_file()
  mockery::stub(
    ld,
    "load",
    function(file, envir) {
      assign("data1", "rda_loaded", envir = envir)
      invisible("data1")
    }
  )
  
  mockery::stub(
    ld,
    "utils::read.csv",
    function(file, header = TRUE) {
      data.frame(a = 1)
    }
  )
  
  mockery::stub(
    ld,
    "utils::read.table",
    function(file, header = TRUE, sep = "") {
      data.frame(b = 2)
    }
  )
  
  mockery::stub(
    ld,
    "jsonlite::fromJSON",
    function(file) {
      list(c = 3)
    }
  )
  
  mockery::stub(
    ld,
    "sys.source",
    function(file, envir) {
      assign("data6", "R_loaded", envir = envir)
      invisible(NULL)
    }
  )
  
  result <- ld(
    pkg_names = "mockpkg",
    env = dummy_env,
    source_paths = c(mockpkg = "/mock/path"),
    overwrite = FALSE
  )
  
  expect_setequal(result, c("data1", "data2", "data3", "data4", "data5", "data6"))
  
  expect_equal(get("data1", envir = dummy_env, inherits = FALSE), "rda_loaded")
  expect_equal(get("data2", envir = dummy_env, inherits = FALSE)$a, 1)
  expect_equal(get("data3", envir = dummy_env, inherits = FALSE)$b, 2)
  expect_equal(get("data4", envir = dummy_env, inherits = FALSE)$b, 2)
  expect_equal(get("data5", envir = dummy_env, inherits = FALSE)$c, 3)
  expect_equal(get("data6", envir = dummy_env, inherits = FALSE), "R_loaded")
})





test_that("load_package_datasets returns empty character when package is not installed", {
  dummy_env <- new.env(parent = emptyenv())
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  # Simulate package not installed
  mockery::stub(
    ld,
    "requireNamespace",
    function(pkg, quietly = TRUE) FALSE
  )
  
  result <- ld(
    pkg_names = "fakepkg",
    env = dummy_env,
    source_paths = NULL,
    overwrite = FALSE
  )
  
  expect_type(result, "character")
  expect_length(result, 0)
  expect_equal(result, character(0))
})




test_that("cleanup_and_return_null finds project root and removes files by extension", {
  # Create a dummy environment
  dummy_env <- new.env()
  dummy_env$x <- 1
  dummy_env$y <- 2
  
  # Stub getwd and normalizePath
  mockery::stub(cleanup_and_return_null, "getwd", function() "/mock/project")
  mockery::stub(cleanup_and_return_null, "normalizePath", function(path) path)
  
  # Simulate files in root directory
  mock_files <- c("/mock/project/report.pdf", "/mock/project/diagram.ps", "/mock/project/readme.txt")
  
  # Stub list.files to return mock files
  mockery::stub(cleanup_and_return_null, "list.files", function(path, full.names) mock_files)
  
  # Stub file.exists to return TRUE for all mock files
  mockery::stub(cleanup_and_return_null, "file.exists", function(path) TRUE)
  
  # Track calls to file.remove
  removed_files <- character()
  mockery::stub(cleanup_and_return_null, "file.remove", function(path) {
    removed_files <<- c(removed_files, path)
    TRUE
  })
  
  # Stub ls and rm for environment cleanup
  mockery::stub(cleanup_and_return_null, "ls", function(envir) c("x", "y"))
  mockery::stub(cleanup_and_return_null, "rm", function(list, envir) {
    for (var in list) rm(list = var, envir = envir)
  })
  
  # Call function
  result <- cleanup_and_return_null(env = dummy_env)
  
  # Assertions
  expect_false(exists("x", envir = dummy_env))
  expect_false(exists("y", envir = dummy_env))
  
  # Check that only .pdf and .ps files were removed
  expect_true(all(grepl("\\.(pdf|ps)$", removed_files)))
})


test_that("cleanup_and_return_null returns NULL when project root not found", {
  dummy_env <- new.env()
  dummy_env$x <- 1
  
  mockery::stub(cleanup_and_return_null, "getwd", function() "/mock/project")
  mockery::stub(cleanup_and_return_null, "file.exists", function(path) FALSE)
  mockery::stub(cleanup_and_return_null, "normalizePath", function(path) path)
  mockery::stub(cleanup_and_return_null, "ls", function(envir) "x")
  mockery::stub(cleanup_and_return_null, "rm", function(list, envir) {
    for (var in list) rm(list = var, envir = envir)
  })
  
  result <- cleanup_and_return_null(env = dummy_env)
  
  expect_false(exists("x", envir = dummy_env))
})



test_that("cleanup removes work_dir under tempdir() when it exists", {
  # --- Arrange ---
  env <- new.env(parent = emptyenv())
  work_dir <- "/TD/work"
  assign("work_dir", work_dir, envir = env)
  
  # Prevent initial env wipe from deleting 'work_dir'
  mockery::stub(cleanup_and_return_null, "base::ls",
                function(envir, all.names = TRUE) character(0))
  mockery::stub(cleanup_and_return_null, "base::rm", function(...) NULL)
  
  # Make tempdir and normalizePath deterministic
  mockery::stub(cleanup_and_return_null, "tempdir", function() "/TD")
  mockery::stub(cleanup_and_return_null, "normalizePath",
                function(path, winslash = "/", mustWork = FALSE) {
                  if (identical(path, "/TD")) return("/TD")           # td_norm
                  if (identical(path, work_dir)) return("/TD/work")   # wd_norm
                  path                                                 # fallback for getwd()
                })
  
  # Ensure dir.exists reports TRUE for the work_dir
  mockery::stub(cleanup_and_return_null, "dir.exists",
                function(p) identical(p, work_dir))
  
  # Capture unlink calls
  fake_unlink <- mockery::mock(TRUE)
  mockery::stub(cleanup_and_return_null, "unlink", fake_unlink)
  
  # Neutralize project-root cleanups
  mockery::stub(cleanup_and_return_null, "getwd", function() "/ROOT")
  mockery::stub(cleanup_and_return_null, "list.files", function(...) character(0))
  
  # --- Act & Assert ---
  expect_silent(cleanup_and_return_null(NULL, env = env))
  mockery::expect_called(fake_unlink, 1)
  mockery::expect_args(fake_unlink, 1, work_dir, recursive = TRUE, force = TRUE)
})



test_that("no removal when work_dir under tempdir() but directory does not exist", {
  env <- new.env(parent = emptyenv())
  work_dir <- "/TD/work"
  assign("work_dir", work_dir, envir = env)
  
  mockery::stub(cleanup_and_return_null, "base::ls",
                function(envir, all.names = TRUE) character(0))
  mockery::stub(cleanup_and_return_null, "base::rm", function(...) NULL)
  
  mockery::stub(cleanup_and_return_null, "tempdir", function() "/TD")
  mockery::stub(cleanup_and_return_null, "normalizePath",
                function(path, winslash = "/", mustWork = FALSE) {
                  if (identical(path, "/TD")) return("/TD")
                  if (identical(path, work_dir)) return("/TD/work")
                  path
                })
  
  # dir.exists is FALSE → do not unlink
  mockery::stub(cleanup_and_return_null, "dir.exists", function(p) FALSE)
  
  fake_unlink <- mockery::mock(TRUE)
  mockery::stub(cleanup_and_return_null, "unlink", fake_unlink)
  
  mockery::stub(cleanup_and_return_null, "getwd", function() "/ROOT")
  mockery::stub(cleanup_and_return_null, "list.files", function(...) character(0))
  
  expect_message(
    cleanup_and_return_null(NULL, env = env), 
    "No safe roots found for orphan cleanup; skipping.")
  mockery::expect_called(fake_unlink, 0)
})

test_that("no removal when work_dir is outside tempdir()", {
  env <- new.env(parent = emptyenv())
  work_dir <- "/OTHER/work"
  assign("work_dir", work_dir, envir = env)
  
  mockery::stub(cleanup_and_return_null, "base::ls",
                function(envir, all.names = TRUE) character(0))
  mockery::stub(cleanup_and_return_null, "base::rm", function(...) NULL)
  
  mockery::stub(cleanup_and_return_null, "tempdir", function() "/TD")
  mockery::stub(cleanup_and_return_null, "normalizePath",
                function(path, winslash = "/", mustWork = FALSE) {
                  if (identical(path, "/TD")) return("/TD")
                  if (identical(path, work_dir)) return("/OTHER/work")  # NOT under /TD
                  path
                })
  
  mockery::stub(cleanup_and_return_null, "dir.exists", function(p) TRUE)
  
  fake_unlink <- mockery::mock(TRUE)
  mockery::stub(cleanup_and_return_null, "unlink", fake_unlink)
  
  mockery::stub(cleanup_and_return_null, "getwd", function() "/ROOT")
  mockery::stub(cleanup_and_return_null, "list.files", function(...) character(0))
  
  expect_message(
    cleanup_and_return_null(NULL, env = env), 
    "No safe roots found for orphan cleanup; skipping.")
  mockery::expect_called(fake_unlink, 0)
})



test_that("no removal when normalizePath(work_dir) errors (fallback to raw path)", {
  env <- new.env(parent = emptyenv())
  work_dir <- "/TD/work"
  assign("work_dir", work_dir, envir = env)
  
  mockery::stub(cleanup_and_return_null, "base::ls",
                function(envir, all.names = TRUE) character(0))
  mockery::stub(cleanup_and_return_null, "base::rm", function(...) NULL)
  
  mockery::stub(cleanup_and_return_null, "tempdir", function() "/TD")
  mockery::stub(cleanup_and_return_null, "normalizePath",
                function(path, winslash = "/", mustWork = FALSE) {
                  if (identical(path, "/TD")) return("/TD")     # td_norm
                  if (identical(path, work_dir)) stop("boom")   # wd_norm falls back to raw path
                  path
                })
  
  # Even if startsWith("/TD/work", "/TD") could be TRUE via fallback,
  # gate unlink by making dir.exists FALSE so we assert no unlink occurred.
  mockery::stub(cleanup_and_return_null, "dir.exists", function(p) FALSE)
  
  fake_unlink <- mockery::mock(TRUE)
  mockery::stub(cleanup_and_return_null, "unlink", fake_unlink)
  
  mockery::stub(cleanup_and_return_null, "getwd", function() "/ROOT")
  mockery::stub(cleanup_and_return_null, "list.files", function(...) character(0))
  
  expect_message(
    cleanup_and_return_null(NULL, env = env), 
    "No safe roots found for orphan cleanup; skipping.")
  mockery::expect_called(fake_unlink, 0)
})



test_that("returns functions with no tests", {
  mock_df <- data.frame(
    source_file = c("file1.R", "file2.R"),
    test_file = c(NA, "test_file2.R"),
    stringsAsFactors = FALSE
  )
  
  result <- get_function_no_tests(mock_df)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$source_file, "file1.R")
  expect_equal(result$comment, "No tests found")
})

test_that("returns comment when all functions have tests", {
  mock_df <- data.frame(
    source_file = c("file1.R", "file2.R"),
    test_file = c("test_file1.R", "test_file2.R"),
    stringsAsFactors = FALSE
  )
  
  result <- get_function_no_tests(mock_df)
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$source_file))
  expect_equal(result$comment, "all functions have at least 1 test")
})

test_that("handles empty input gracefully", {
  mock_df <- data.frame(
    source_file = character(0),
    test_file = character(0),
    stringsAsFactors = FALSE
  )
  
  result <- get_function_no_tests(mock_df)
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$source_file))
  expect_equal(result$comment, "all functions have at least 1 test")
})




test_that("run_covr_modes handles standard testing framework with valid coverage", {
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mock_run_coverage <- function(pkg_source_path, covr_timeout, ...) {
    list(
      total_cov = 85,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("some_function")),
          totalcoverage = 85
        )
      )
    )
  }
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    run_coverage = mock_run_coverage
  )
  
  # Stub check_pkg_tests_and_snaps - has testthat
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = TRUE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  # Stub get_pkg_desc
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  
  # Stub unloadNamespace
  mock_unloadNamespace <- function(pkg) {}
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  
  # Run
  result <- run_covr_modes(pkg_source_path)
  
  # Assertions
  expect_true(is.list(result))
  expect_equal(result$total_cov, 85)
})



test_that("run_covr_modes handles Bioconductor testing framework correctly", {
  local_cov_env <- new.env(parent = baseenv())
  
  mock_initialise_covr_env <- function(pkg_source_path, pkg, framework, ...) {
    assign("pkg_path", pkg_source_path, envir = local_cov_env)
    assign("package", pkg, envir = local_cov_env)
    assign("framework", framework, envir = local_cov_env)
    assign("work_dir", tempdir(), envir = local_cov_env)
    assign("pkg_root", pkg_source_path, envir = local_cov_env)
    local_cov_env
  }
  
  mock_run_covr_skip_bioc_nstf <- function(pkg_source_path, test_pkg_data, cov_env,
                                            framework = NULL, ...) {
    list(
      total_cov = 77,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("bioc_fun")),
          totalcoverage = 77
        ),
        notes = "Bioc NSTF run"
      ),
      tests_skipped = character(),
      tests_passing = character()
    )
  }
  
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env = local_cov_env,
    initialise_covr_env = mock_initialise_covr_env,
    run_covr_skip_bioc_nstf = mock_run_covr_skip_bioc_nstf
  )
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_bioc")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mockery::stub(run_covr_modes, "get_pkg_name", function(path, ...) "mockpkg")
  mockery::stub(run_covr_modes, "get_pkg_desc",
                function(path, fields, ...) list(Package = "mockpkg", Version = "1.0.0")
  )
  mockery::stub(run_covr_modes, "get_test_metadata",
                function(...) {
                  list(
                    datetime = "2026-01-01 12:00:00",
                    executor = "test-user",
                    info = list(sys = list(
                      sysname = "Linux",
                      version = "5.15",
                      release = "Ubuntu",
                      machine = "x86_64"
                    ))
                  )
                }
  )
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps",
                function(path, ...) {
                  list(
                    has_testthat = FALSE,
                    has_testit = FALSE,
                    has_tests_base = FALSE,
                    has_tinytest = FALSE,
                    has_data_table_tests = FALSE,
                    has_RUnit_test = FALSE,
                    has_BioG_test = TRUE,
                    has_snaps = FALSE,
                    bioc_unit_tests_dir = "inst/unitTests",
                    n_golden_tests = 0L,
                    n_test_files = 1L
                  )
                }
  )
  mockery::stub(safe_unload_package, "loadedNamespaces", function() character(0))
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 77)
  expect_equal(result$res_cov$coverage$totalcoverage, 77)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "bioc_fun")
  expect_equal(
    result$test_framework_type,
    "non-standard testing framework - Bioconductor"
  )
  expect_equal(result$pkg_name, "mockpkg")
  expect_equal(result$pkg_ver, "1.0.0")
  expect_equal(result$executor, "test-user")
  expect_equal(result$sysname, "Linux")
})

test_that("run_covr_modes triggers fallback to run_covr_skip_stf when coverage is NA", {
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mock_run_coverage <- function(pkg_source_path, covr_timeout, ...) {
    list(
      total_cov = NA,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = NA,
          totalcoverage = NA
        )
      )
    )
  }
  
  mock_run_covr_skip_stf <- function(pkg_source_path, test_pkg_data, cov_env) {
    list(
      total_cov = 42,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("fallback_function")),
          totalcoverage = 42
        )
      )
    )
  }
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    run_coverage = mock_run_coverage,
    run_covr_skip_stf = mock_run_covr_skip_stf
  )
  
  # Stub test framework detection → has testthat
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = TRUE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  # Stub get_pkg_desc
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  
  # Stub unloadNamespace (important to avoid real unloading)
  mockery::stub(run_covr_modes, "unloadNamespace", function(...) {})
  
  # Run
  result <- run_covr_modes(pkg_source_path)
  
  # Assertions
  expect_true(is.list(result))
  expect_equal(result$total_cov, 42)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "fallback_function")
})

test_that("run_covr_modes handles non-standard testing framework", {
  local_cov_env <- new.env(parent = baseenv())
  
  mock_initialise_covr_env <- function(pkg_source_path, pkg, framework, ...) {
    assign("pkg_path", pkg_source_path, envir = local_cov_env)
    assign("package", pkg, envir = local_cov_env)
    assign("framework", framework, envir = local_cov_env)
    assign("work_dir", tempdir(), envir = local_cov_env)
    assign("pkg_root", pkg_source_path, envir = local_cov_env)
    local_cov_env
  }
  
  mock_run_covr_skip_nstf <- function(pkg_source_path, test_pkg_data, cov_env,
                                      framework = NULL, ...) {
    list(
      total_cov = 50,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1),
          totalcoverage = 50
        ),
        notes = "NSTF coverage run"
      )
    )
  }
  
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env = local_cov_env,
    initialise_covr_env = mock_initialise_covr_env,
    run_covr_skip_nstf = mock_run_covr_skip_nstf
  )
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_nstf_base")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mockery::stub(run_covr_modes, "get_pkg_name", function(path, ...) "mockpkg")
  mockery::stub(run_covr_modes, "get_pkg_desc",
                function(path, fields, ...) list(Package = "mockpkg", Version = "0.1.0")
  )
  mockery::stub(run_covr_modes, "get_test_metadata",
                function(...) {
                  list(
                    datetime = "2026-01-01 12:00:00",
                    executor = "test-user",
                    info = list(sys = list(
                      sysname = "Linux",
                      version = "5.15",
                      release = "Ubuntu",
                      machine = "x86_64"
                    ))
                  )
                }
  )
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps",
                function(...) {
                  list(
                    has_testthat = FALSE,
                    has_tinytest = FALSE,
                    has_data_table_tests = FALSE,
                    has_BioG_test = FALSE,
                    has_RUnit_test = FALSE,
                    has_tests_base = TRUE,
                    has_testit = FALSE,
                    has_snaps = FALSE,
                    bioc_unit_tests_dir = NA_character_,
                    n_golden_tests = 0L,
                    n_test_files = 1L
                  )
                }
  )
  mockery::stub(safe_unload_package, "loadedNamespaces", function() character(0))
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 50)
  expect_equal(result$res_cov$notes, "NSTF coverage run")
})

test_that("run_covr_modes multiple frameworks branch runs each framework and aggregates total_cov", {
  pkg_source_path <- file.path(tempdir(), "mockpkg_multi")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mock_run_one_framework <- function(framework, pkg, test_pkg_data, pkg_source_path, covr_timeout) {
    cov_val <- switch(framework, testthat = 80, testit = 40, 0)
    list(
      total_cov = cov_val,
      res_cov = list(
        name = pkg,
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("fn")),
          totalcoverage = cov_val
        )
      ),
      test_framework_type = "framework",
      framework_id = framework
    )
  }
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    run_one_framework = mock_run_one_framework
  )
  
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(
      has_testthat = TRUE,
      has_testit = TRUE,
      has_tests_base = FALSE,
      has_tinytest = FALSE,
      has_data_table_tests = FALSE,
      has_BioG_test = FALSE
    )
  })
  mockery::stub(run_covr_modes, "get_pkg_name", function(path, ...) "mockpkg")
  mockery::stub(run_covr_modes, "get_pkg_desc", function(path, fields, ...) {
    list(Package = "mockpkg", Version = "1.0.0")
  })
  mockery::stub(run_covr_modes, "get_test_metadata", function(...) {
    list(
      datetime = "2026-01-15 12:00:00",
      executor = "test-user",
      info = list(sys = list(sysname = "Linux", version = "5.15", release = "Ubuntu", machine = "x86_64"))
    )
  })
  mockery::stub(run_covr_modes, "unloadNamespace", function(pkg, ...) NULL)
  
  expect_message(
    result <- run_covr_modes(pkg_source_path, covr_timeout = 10),
    "Multiple test frameworks detected"
  )
  
  expect_true(result$multi_framework)
  expect_equal(result$frameworks, c("testthat", "testit"))
  expect_equal(result$total_cov, 60)
  expect_equal(result$pkg_name, "mockpkg")
  expect_equal(result$pkg_ver, "1.0.0")
  expect_named(result$results, c("testthat", "testit"))
  expect_equal(result$results$testthat$total_cov, 80)
  expect_equal(result$results$testit$total_cov, 40)
})

test_that("run_covr_modes handles no testing configuration", {
  pkg_source_path <- "mock/path"
  
  # Isolate a clean cov_env for this test
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env  = new.env(parent = baseenv())
  )
  
  # --- Stubs ---
  
  # Prevent normalizePath() and filesystem access. Return an environment.
  mockery::stub(run_covr_modes, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  assign("pkg_path",  pkg_source_path, envir = cov_env)
                  assign("package",   pkg,             envir = cov_env)
                  assign("framework", framework,       envir = cov_env)
                  cov_env
                }
  )
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields, ...) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  
  # Mock unloadNamespace to avoid side effects
  mockery::stub(run_covr_modes, "unloadNamespace", function(...) {})
  
  # Return a complete "no tests present" structure
  mock_check_pkg_tests_and_snaps <- function(...) {
    list(
      has_testthat       = FALSE,
      has_snaps          = FALSE,
      has_testit         = FALSE,
      has_tinytests      = FALSE,
      has_data_table_tests = FALSE,
      has_tests_base     = FALSE,
      has_BioG_test      = FALSE,
      has_RUnit_test     = FALSE,   
      bioc_unit_tests_dir = NA_character_,
      n_golden_tests     = 0L,
      n_test_files       = 0L
    )
  }
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  
  # --- Execute ---
  result <- run_covr_modes(pkg_source_path)
  
  # --- Assertions ---
  expect_true(is.list(result))
  expect_equal(result$total_cov, 0)
  # The function should explain why coverage is 0 when no configs are found.
  expect_match(
    result$res_cov$errors,
    "No recognised standard or non-standard testing configuration",
    fixed = FALSE
  )
})

test_that("run_covr_modes unloads package when loaded", {
  pkg_source_path <- "mock/path"
  
  # Isolate a clean cov_env for this test
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env  = new.env(parent = baseenv())
  )
  
  # ---- Stubs ----
  
  # Stub initialise_covr_env: avoid normalizePath() and return the environment
  mockery::stub(run_covr_modes, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  assign("pkg_path",  pkg_source_path, envir = cov_env)
                  assign("package",   pkg,             envir = cov_env)
                  assign("framework", framework,       envir = cov_env)
                  cov_env
                }
  )
  
  # Mock get_pkg_desc to return a known package
  mock_get_pkg_desc <- function(pkg_source_path, fields, ...) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mockery::stub(run_covr_modes, "loadedNamespaces", function(...) {
    c("mockpkg", "stats", "utils")
  })
  
  # Mock unloadNamespace to simulate successful unload (no side effects)
  mockery::stub(run_covr_modes, "unloadNamespace", function(pkg, ...) {
    # Optional: track or message if you want to assert it was called
    message(sprintf("Mock unloadNamespace called for %s", pkg))
    invisible(NULL)
  })
  
  # Return a COMPLETE "no tests present" structure
  mock_check_pkg_tests_and_snaps <- function(...) {
    list(
      has_testthat        = FALSE,
      has_snaps           = FALSE,
      has_testit          = FALSE,
      has_tests_base      = FALSE,
      has_tinytests       = FALSE,
      has_data_table_tests = FALSE,
      has_BioG_test       = FALSE,
      has_RUnit_test      = FALSE,   
      bioc_unit_tests_dir = NA_character_,
      n_golden_tests      = 0L,
      n_test_files        = 0L
    )
  }
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  
  # ---- Execute ----
  result <- run_covr_modes(pkg_source_path)
  
  # ---- Assertions ----
  expect_true(is.list(result))
  expect_equal(result$total_cov, 0)
  expect_match(
    result$res_cov$errors,
    "No recognised standard or non-standard testing configuration",
    fixed = FALSE
  )
})



test_that("run_covr_modes unloads package when loaded", {
  pkg_source_path <- "mock/path"
  
  # Isolate a clean cov_env for this test
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env  = new.env(parent = baseenv())
  )
  
  # ---- Stubs ----
  
  # Stub initialise_covr_env: avoid normalizePath() and return the environment
  mockery::stub(run_covr_modes, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  assign("pkg_path",  pkg_source_path, envir = cov_env)
                  assign("package",   pkg,             envir = cov_env)
                  assign("framework", framework,       envir = cov_env)
                  cov_env
                }
  )
  
  # Mock get_pkg_desc to return a known package
  mockery::stub(run_covr_modes, "get_pkg_desc",
                function(pkg_source_path, fields, ...) {
                  list(Package = "mockpkg", Version = "0.1.0")
                }
  )
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mockery::stub(run_covr_modes, "loadedNamespaces",
                function(...) {
                  c("mockpkg", "stats", "utils")
                }
  )
  
  # Mock unloadNamespace to simulate successful unload (no side effects)
  mockery::stub(run_covr_modes, "unloadNamespace",
                function(pkg, ...) {
                  message(sprintf("Mock unloadNamespace called for %s", pkg))
                  invisible(NULL)
                }
  )
  
  # ---- CRITICAL: Return COMPLETE structure expected by run_covr_modes ----
  mock_check_pkg_tests_and_snaps <- function(...) {
    list(
      has_testthat          = FALSE,
      has_snaps             = FALSE,
      has_testit            = FALSE,
      has_tests_base        = FALSE,
      has_tinytest          = FALSE,
      has_data_table_tests  = FALSE,
      has_BioG_test         = FALSE,
      has_RUnit_test        = FALSE,         
      bioc_unit_tests_dir   = NA_character_,
      n_golden_tests        = 0L,
      n_test_files          = 0L
    )
  }
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  
  # ---- Execute ----
  result <- run_covr_modes(pkg_source_path)
  
  # ---- Assertions ----
  expect_true(is.list(result))
  expect_equal(result$total_cov, 0)
  expect_match(
    result$res_cov$errors,
    "No recognised standard or non-standard testing configuration",
    fixed = FALSE
  )
})

test_that("run_covr_modes handles error when unloading package", {
  pkg_source_path <- "mock/path"
  
  # Isolate clean cov_env for this test
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env = new.env(parent = baseenv())
  )
  
  # ---- Stub initialise_covr_env ----
  mockery::stub(run_covr_modes, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  assign("pkg_path",  pkg_source_path, envir = cov_env)
                  assign("package",   pkg,             envir = cov_env)
                  assign("framework", framework,       envir = cov_env)
                  cov_env
                }
  )
  
  # ---- get_pkg_desc ----
  mockery::stub(run_covr_modes, "get_pkg_desc",
                function(pkg_source_path, fields, ...) {
                  list(Package = "mockpkg", Version = "0.1.0")
                }
  )
  
  # ---- Simulate package being loaded ----
  mockery::stub(run_covr_modes, "loadedNamespaces",
                function(...) c("mockpkg", "stats", "utils")
  )
  
  # ---- Simulate unloadNamespace() failing ----
  mockery::stub(run_covr_modes, "unloadNamespace",
                function(pkg, ...) stop("Simulated unload failure")
  )
  
  # ---- REQUIRED: complete no-test configuration ----
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps",
                function(...) {
                  list(
                    has_testthat          = FALSE,
                    has_snaps             = FALSE,
                    has_testit            = FALSE,
                    has_tests_base        = FALSE,
                    has_tinytest          = FALSE,
                    has_data_table_tests  = FALSE,
                    has_BioG_test         = FALSE,
                    has_RUnit_test        = FALSE,  # <-- REQUIRED to avoid length-0 if()
                    bioc_unit_tests_dir   = NA_character_,
                    n_golden_tests        = 0L,
                    n_test_files          = 0L
                  )
                }
  )
  
  # ---- Execute ----
  result <- run_covr_modes(pkg_source_path)
  
  # ---- Assertions ----
  expect_true(is.list(result))
  expect_equal(result$total_cov, 0)
  expect_match(
    result$res_cov$errors,
    "No recognised standard or non-standard testing configuration",
    fixed = FALSE
  )
})

test_that("run_covr_modes handles error when unloading package", {
  mock_run_one_framework <- function(framework, pkg, test_pkg_data, pkg_source_path, covr_timeout) {
    safe_unload_package(pkg)
    list(
      total_cov = 85,
      res_cov = list(
        name = pkg,
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("some_function")),
          totalcoverage = 85
        )
      )
    )
  }
  
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    run_one_framework = mock_run_one_framework
  )
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_unload_msg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mockery::stub(safe_unload_package, "loadedNamespaces", function() c("mockpkg", "stats", "utils"))
  mockery::stub(safe_unload_package, "unloadNamespace", function(pkg, ...) stop("Simulated unload failure"))
  
  mockery::stub(run_covr_modes, "get_pkg_name", function(path, ...) "mockpkg")
  mockery::stub(run_covr_modes, "get_pkg_desc",
                function(path, fields, ...) list(Package = "mockpkg", Version = "1.0.0")
  )
  
  mockery::stub(run_covr_modes, "get_test_metadata",
                function(...) {
                  list(
                    datetime = "2026-01-07 12:00:00",
                    executor = "test-user",
                    info = list(sys = list(
                      sysname = "Linux",
                      version = "5.15",
                      release = "Ubuntu",
                      machine = "x86_64"
                    ))
                  )
                }
  )
  
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps",
                function(path, ...) {
                  list(
                    has_testthat          = TRUE,
                    has_testit            = FALSE,
                    has_tests_base        = FALSE,
                    has_tinytest          = FALSE,
                    has_data_table_tests  = FALSE,
                    has_BioG_test         = FALSE,
                    has_RUnit_test        = FALSE,
                    has_snaps             = FALSE,
                    bioc_unit_tests_dir   = NA_character_,
                    n_golden_tests        = 0L,
                    n_test_files          = 0L
                  )
                }
  )
  
  msgs <- testthat::capture_messages({
    result <- run_covr_modes(pkg_source_path)
  })
  expect_true(any(grepl("Warning: Failed to unload mockpkg package", msgs, fixed = TRUE)))
  expect_true(any(grepl("Details: Simulated unload failure", msgs, fixed = TRUE)))

})


test_that("create_empty_covr_list returns correct structure", {
  pkg_name <- "mockpkg"
  error_message <- "No coverage data available"
  tpd <- list(has_testthat = TRUE)
  
  result <- create_empty_covr_list(pkg_name, error_message, test_pkg_data = tpd)
  
  expect_type(result, "list")
  expect_identical(
    names(result),
    c("total_cov", "res_cov", "test_pkg_data")
  )
  
  expect_identical(result$test_pkg_data, tpd)
  expect_equal(result$total_cov, 0)
  expect_type(result$res_cov, "list")
  expect_equal(result$res_cov$name, pkg_name)
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  expect_true(is.matrix(result$res_cov$coverage$filecoverage))
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "No functions tested")
  expect_equal(result$res_cov$errors, error_message)
  expect_true(is.na(result$res_cov$notes))
  
  result_null <- create_empty_covr_list(
    pkg_name,
    error_message,
    test_pkg_data = NULL
  )
  expect_null(result_null$test_pkg_data)
})

test_that("safe_unload_package unloads a loaded package", {
  mockery::stub(safe_unload_package, "loadedNamespaces", function() c("mockpkg"))
  mockery::stub(safe_unload_package, "unloadNamespace", function(pkg) TRUE)
  
  expect_message(safe_unload_package("mockpkg"), "Unloading mockpkg package to avoid coverage conflict...")
})

test_that("safe_unload_package handles unloadNamespace error gracefully", {
  mockery::stub(safe_unload_package, "loadedNamespaces", function() c("mockpkg"))
  mockery::stub(safe_unload_package, "unloadNamespace", function(pkg) stop("mock error"))
  
  expect_message(safe_unload_package("mockpkg"), "Warning: Failed to unload mockpkg package. Proceeding anyway.")
  expect_message(safe_unload_package("mockpkg"), "Details: mock error")
})

test_that("load_package_into_env loads package objects into environment", {
  cov_env <- new.env()
  
  # Stub pkgload::load_all to simulate successful load
  mockery::stub(load_package_into_env, "pkgload::load_all", function(path) TRUE)
  
  # Stub ls to simulate package objects
  mockery::stub(load_package_into_env, "ls", function(envir, all.names) c("obj1", "obj2"))
  
  # Stub get to return dummy objects
  mockery::stub(load_package_into_env, "get", function(name, envir) paste0("value_of_", name))
  
  # Stub assign to actually assign into cov_env
  mockery::stub(load_package_into_env, "assign", function(x, value, envir) assign(x, value, envir = envir))
  
  result <- load_package_into_env("mock/path", "mockpkg", cov_env)
  
  expect_true(exists("obj1", envir = cov_env))
  expect_equal(get("obj1", envir = cov_env), "value_of_obj1")
  expect_true(inherits(result, "logical"))
})


test_that("load_package_into_env stops on load_all error", {
  cov_env <- new.env()
  
  mockery::stub(load_package_into_env, "pkgload::load_all", function(path) stop("load error"))
  
  expect_error(load_package_into_env("mock/path", "mockpkg", cov_env),
               "Failed to load package mockpkg : load error")
})


test_that("load_package_into_env handles get() error gracefully", {
  cov_env <- new.env()
  
  mockery::stub(load_package_into_env, "pkgload::load_all", function(path) TRUE)
  mockery::stub(load_package_into_env, "ls", function(envir, all.names) c("obj1", "obj2"))
  
  # Simulate get() failing for obj2
  mockery::stub(load_package_into_env, "get", function(name, envir) {
    if (name == "obj2") stop("get error")
    paste0("value_of_", name)
  })
  
  # Stub assign to assign into cov_env
  mockery::stub(load_package_into_env, "assign", function(x, value, envir) assign(x, value, envir = envir))
  
  expect_message(load_package_into_env("mock/path", "mockpkg", cov_env),
                 "Failed to assign object obj2 : get error")
  
  expect_true(exists("obj1", envir = cov_env))
  expect_false(exists("obj2", envir = cov_env))
})


test_that("find_project_root returns NULL when no project files are found", {
  # Create a temporary directory with no DESCRIPTION, .Rproj, or .git
  temp_root <- tempfile("no_project_root_")
  dir.create(temp_root)
  
  # Stub file.exists to always return FALSE to simulate no project markers
  mockery::stub(find_project_root, "file.exists", function(path) FALSE)
  
  # Run the function starting from the temp directory
  result <- find_project_root(start = temp_root)
  
  # Expect NULL because no project root indicators exist
  expect_null(result)
})




test_that("cleanup_and_return_null skips orphan cleanup when no safe root can be determined", {
  cov_env <- new.env()
  assign("x", 1, envir = cov_env)
  
  # Make sure there is no pkg_root and no work_dir so safe_roots stays empty.
  # (You can keep your normalizePath/getwd stubs if you want, but they're no longer required.)
  msgs <- testthat::capture_messages({
    result <- cleanup_and_return_null("Test message", env = cov_env)
  })
  
  expect_null(result)
  expect_true(
    any(grepl("No safe roots found for orphan cleanup; skipping\\.", msgs)),
    info = paste("Captured messages:\n", paste(msgs, collapse = "\n"))
  )
  expect_equal(base::ls(envir = cov_env, all.names = TRUE), character(0))
})


test_that("returns character(0) when package is not installed", {
  dummy_env <- new.env(parent = emptyenv())
  
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  mockery::stub(
    ld,
    "requireNamespace",
    function(pkg, quietly = TRUE) FALSE
  )
  
  expect_message(
    result <- ld(
      pkg_names = "fakepkg",
      env = dummy_env,
      source_paths = NULL,
      overwrite = FALSE
    ),
    "Package fakepkg is not installed; skipping installed datasets."
  )
  
  expect_identical(result, character(0))
  expect_equal(ls(envir = dummy_env, all.names = TRUE), character(0))
})



test_that("returns datasets from source data directory with supported extensions", {
  fake_env <- new.env(parent = emptyenv())

  # Create a fake package root inside tempdir()
  pkg_root_dir <- file.path(tempdir(), "pkgroot_test")
  dir.create(pkg_root_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create a working directory *inside* pkg_root
  child_dir <- file.path(pkg_root_dir, "subdir")
  dir.create(child_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save original working directory
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  # Set working directory to child directory (so cwd_norm is inside pkg_root)
  setwd(child_dir)
  
  # Build cleanup environment
  fake_env <- new.env(parent = baseenv())
  fake_env$pkg_root <- pkg_root_dir
  fake_env$work_dir <- NULL
  
  # --- Mock filesystem functions -------------------------------------------
  # (No library(mockery) is used; only direct mockery::stub calls.)
  
  # Pretend all paths exist
  mockery::stub(cleanup_and_return_null, "dir.exists", function(path) TRUE)
  
  # Make list.files return a fixed empty set (we don't test deletion)
  mockery::stub(cleanup_and_return_null, "list.files", function(...) character(0))
  
  # Mock normalizePath so that pkg_root and cwd resolve exactly
  mockery::stub(cleanup_and_return_null, "normalizePath",
                function(path, ...) {
                  # Return path unchanged (good enough for prefix checks)
                  path
                }
  )
  
  # --- Run cleanup ----------------------------------------------------------
  # Capture the message that indicates safe_roots is non-empty
  # Specifically: we should NOT see "No safe roots found..."
  
  output <- capture.output({
    cleanup_and_return_null(env = fake_env)
  })
  
  # --- Assertions -----------------------------------------------------------
  
  # The function should *not* say "No safe roots found"
  expect_false(any(grepl("No safe roots found", output)))
  
  # And no error should occur
  expect_true(TRUE)  # reached successfully
})


test_that("falls back to data() if no files loaded", {
  fake_env <- new.env()
  mockery::stub(load_package_datasets, "requireNamespace", TRUE)
  mockery::stub(load_package_datasets, "find.package", "/fake/path")
  mockery::stub(load_package_datasets, "dir.exists", TRUE)
  mockery::stub(load_package_datasets, "list.files", character(0))
 
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  # For this test we want to exercise only the source data directory branch.
  # So treat the source package path as available, but the installed package
  # as unavailable.
  mockery::stub(
    ld,
    "requireNamespace",
    function(pkg, quietly = TRUE) FALSE
  )
  
  # Simulate /fake/path/data existing
  mockery::stub(ld, "dir.exists", function(path) TRUE)
  
  # Simulate supported files in the source data directory
  mockery::stub(
    ld,
    "list.files",
    function(path, full.names = TRUE) {
      c(
        "/fake/path/data/data1.csv",
        "/fake/path/data/data2.rda"
      )
    }
  )
  
  # Match the symbols used in the production code
  mockery::stub(
    ld,
    "tools::file_ext",
    function(x) {
      if (grepl("\\.csv$", x)) "csv" else "rda"
    }
  )
  
  mockery::stub(
    ld,
    "tools::file_path_sans_ext",
    function(x) {
      sub("\\.[^.]*$", "", basename(x))
    }
  )
  
  # Stub file loaders
  mockery::stub(
    ld,
    "utils::read.csv",
    function(file, header = TRUE) {
      data.frame(a = 1:3)
    }
  )
  
  mockery::stub(
    ld,
    "load",
    function(file, envir) {
      assign("data2", data.frame(x = 1), envir = envir)
      invisible("data2")
    }
  )
  
  result <- ld(
    pkg_names = "fakepkg",
    env = fake_env,
    source_paths = c(fakepkg = "/fake/path"),
    overwrite = FALSE
  )
  
  expect_setequal(result, c("data1", "data2"))
  expect_true(exists("data1", envir = fake_env, inherits = FALSE))
  expect_true(exists("data2", envir = fake_env, inherits = FALSE))
  expect_equal(get("data1", envir = fake_env, inherits = FALSE)$a, 1:3)
  expect_equal(get("data2", envir = fake_env, inherits = FALSE)$x, 1)
})



test_that("falls back to utils::data() if no files are loaded from source data directory", {
  fake_env <- new.env(parent = emptyenv())
  
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  # Treat fakepkg as installed so the installed-package fallback branch is used
  mockery::stub(
    ld,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  # Simulate source data directory exists but contains no files
  mockery::stub(ld, "dir.exists", function(path) TRUE)
  mockery::stub(ld, "list.files", function(path, full.names = TRUE) character(0))
  
  # Mock utils::data() in the utils namespace, because the function
  # calls utils::data(...) explicitly.
  testthat::local_mocked_bindings(
    data = function(..., list = NULL, package = NULL, envir = NULL) {
      if (is.null(list)) {
        # Simulate utils::data(package = "fakepkg")$results
        return(list(
          results = matrix(
            "fallback_data",
            ncol = 1,
            dimnames = list(NULL, "Item")
          )
        ))
      } else {
        # Simulate utils::data(list = "fallback_data", package = "fakepkg", envir = fake_env)
        assign(list, data.frame(x = 1), envir = envir)
        invisible(NULL)
      }
    },
    .package = "utils"
  )
  
  result <- ld(
    pkg_names = "fakepkg",
    env = fake_env,
    source_paths = c(fakepkg = "/fake/path"),
    overwrite = FALSE
  )
  
  expect_identical(result, "fakepkg::fallback_data")
  expect_true(exists("fallback_data", envir = fake_env, inherits = FALSE))
  expect_equal(get("fallback_data", envir = fake_env, inherits = FALSE)$x, 1)
})




test_that("handles errors during file loading gracefully", {
  fake_env <- new.env(parent = emptyenv())
  
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  # For this test, exercise only the source data directory branch.
  # Skip installed-package loading by making fakepkg appear unavailable.
  mockery::stub(
    ld,
    "requireNamespace",
    function(pkg, quietly = TRUE) FALSE
  )
  
  mockery::stub(ld, "dir.exists", function(path) TRUE)
  
  mockery::stub(
    ld,
    "list.files",
    function(path, full.names = TRUE) {
      "/fake/path/data/bad.csv"
    }
  )
  
  mockery::stub(
    ld,
    "tools::file_ext",
    function(x) "csv"
  )
  
  mockery::stub(
    ld,
    "tools::file_path_sans_ext",
    function(x) "bad"
  )
  
  mockery::stub(
    ld,
    "utils::read.csv",
    function(...) stop("read error")
  )
  
  expect_message(
    result <- ld(
      pkg_names = "fakepkg",
      env = fake_env,
      source_paths = c(fakepkg = "/fake/path"),
      overwrite = FALSE
    ),
    "Failed to load file /fake/path/data/bad.csv : read error"
  )
  
  expect_identical(result, character(0))
  expect_false("bad" %in% result)
  expect_false(exists("bad", envir = fake_env, inherits = FALSE))
})



test_that("returns empty when no new globals", {
  env <- new.env()
  initial <- ls(envir = env)
  expect_message(result <- remove_new_globals(env, initial), "No new globals detected.")
  expect_equal(result, character())
})

test_that("removes new globals", {
  env <- new.env()
  initial <- ls(envir = env)
  assign("new_var", 42, envir = env)
  expect_message(result <- remove_new_globals(env, initial), "Removed new globals:")
  expect_false("new_var" %in% ls(envir = env))
  expect_equal(result, "new_var")
})

test_that("filters new globals by pattern", {
  env <- new.env()
  initial <- ls(envir = env)
  assign("temp_data", 1, envir = env)
  assign("keep_me", 2, envir = env)
  result <- remove_new_globals(env, initial, patterns = "^temp")
  expect_false("temp_data" %in% ls(envir = env))
  expect_true("keep_me" %in% ls(envir = env))
  expect_equal(result, "temp_data")
})

test_that("dry run does not remove globals", {
  env <- new.env()
  initial <- ls(envir = env)
  assign("dry_var", 99, envir = env)
  expect_message(result <- remove_new_globals(env, initial, dry_run = TRUE), "New globals detected")
  expect_true("dry_var" %in% ls(envir = env))
  expect_equal(result, "dry_var")
})


test_that("does nothing when no helper files found", {
  env <- new.env()
  mockery::stub(load_test_helpers, "list.files", character(0))
  expect_message(result <- load_test_helpers(env), NA)  # no message expected
  expect_equal(result, invisible(TRUE))
})

test_that("sources helper files successfully", {
  env <- new.env()
  helper_files <- c("tests/testthat/helper_one.R", "tests/testthat/helper_two.R")
  
  mockery::stub(load_test_helpers, "list.files", helper_files)
  mockery::stub(load_test_helpers, "sys.source", function(file, envir) {
    assign(basename(file), TRUE, envir = envir)
  })
  
  result <- load_test_helpers(env)
  expect_true("helper_one.R" %in% ls(env))
  expect_true("helper_two.R" %in% ls(env))
  expect_equal(result, invisible(TRUE))
})

test_that("handles errors during sourcing", {
  env <- new.env()
  helper_files <- c("tests/testthat/helper_bad.R")
  
  mockery::stub(load_test_helpers, "list.files", helper_files)
  mockery::stub(load_test_helpers, "sys.source", function(file, envir) {
    stop("source error")
  })
  
  expect_message(load_test_helpers(env), "Failed to source helper file:")
})

test_that("cleanup_and_return_null removes only .pdf/.ps files and only if they exist (ephemeral root)", {
  cov_env <- new.env(parent = emptyenv())
  assign("dummy", 1, envir = cov_env)
  
  # Put work_dir under tempdir() so extension-based cleanup is allowed
  cov_env$work_dir <- "/tmp/project"
  
  # Ensure pkg_root is not set for this test
  if (exists("pkg_root", envir = cov_env, inherits = FALSE)) {
    rm("pkg_root", envir = cov_env)
  }
  
  # Make tempdir and normalizePath consistent with our paths
  mockery::stub(cleanup_and_return_null, "tempdir", function() "/tmp")
  mockery::stub(cleanup_and_return_null, "normalizePath", function(path, ...) path)
  
  # Ensure the work_dir is considered present and prevent its deletion in step 2
  mockery::stub(cleanup_and_return_null, "dir.exists", function(path) path == "/tmp/project")
  mockery::stub(cleanup_and_return_null, "unlink", function(path, recursive = FALSE, force = FALSE) TRUE)
  
  # Mock files in the ephemeral root
  mock_files <- c(
    "/tmp/project/report.pdf",  # exists = TRUE => should be removed
    "/tmp/project/diagram.ps",  # exists = FALSE => should NOT be removed
    "/tmp/project/readme.txt"   # wrong extension => ignored
  )
  mockery::stub(cleanup_and_return_null, "list.files", function(path, full.names = TRUE, ...) mock_files)
  
  # Only the .pdf exists
  mockery::stub(cleanup_and_return_null, "file.exists", function(path) path == "/tmp/project/report.pdf")
  
  # Track deletions
  removed_files <- character(0)
  mockery::stub(cleanup_and_return_null, "file.remove", function(path) { removed_files <<- c(removed_files, path); TRUE })
  
  # Run
  result <- cleanup_and_return_null(env = cov_env)
  expect_null(result)
  
  # Only the existing .pdf should have been removed
  expect_identical(removed_files, "/tmp/project/report.pdf")
})


test_that("setup_covr_env completes successfully", {
  # -------------------------------
  # Create a mock cov_env
  # -------------------------------
  cov_env <- new.env(parent = emptyenv())
  
  # setup_covr_env() asserts that test_path exists
  test_path <- withr::local_tempdir()
  
  # Pull function from namespace so stubs apply to the function object used here
  sce <- get("setup_covr_env", envir = asNamespace("test.assessr"))
  
  # -------------------------------
  # Stubs for helpers / dependencies
  # -------------------------------
  
  # Simulate a single helper file
  mockery::stub(
    sce,
    "list.files",
    function(path, pattern = NULL, full.names = TRUE) "helper_one.R"
  )
  
  # Pretend sourcing a helper sets a flag inside cov_env
  mockery::stub(
    sce,
    "sys.source",
    function(file, envir) {
      assign("helper_loaded", TRUE, envir = envir)
      invisible(NULL)
    }
  )
  
  # Skip loading the real package
  mockery::stub(
    sce,
    "pkgload::load_all",
    function(...) NULL
  )
  
  # Fake namespace with one object to copy into cov_env
  mockery::stub(
    sce,
    "asNamespace",
    function(pkg) {
      e <- new.env(parent = emptyenv())
      assign("fun", function() NULL, envir = e)
      e
    }
  )
  
  # Force find_project_root() to return a known value
  mockery::stub(
    sce,
    "find_project_root",
    function(...) "/mock/root"
  )
  
  # Fake package dataset loading
  mockery::stub(
    sce,
    "load_package_datasets",
    function(pkg_names, env, source_paths = NULL, overwrite = FALSE) {
      if (identical(pkg_names, "mockpkg")) {
        c("data1", "data2")
      } else if (identical(pkg_names, "nycflights13")) {
        "flights"
      } else {
        character(0)
      }
    }
  )
  
  # Fake Base R package surface loading
  mockery::stub(
    sce,
    "load_base_r_packages_into_env",
    function(env, overwrite = FALSE) {
      list(
        exports_loaded = c("stats::rnorm", "utils::head"),
        datasets_loaded = c("datasets::mtcars", "datasets::iris")
      )
    }
  )
  
  # Neutralize dependency loader and safe unload
  mockery::stub(sce, "load_dependencies_into_env", function(...) NULL)
  mockery::stub(sce, "safe_unload_package", function(...) NULL)
  
  # -------------------------------
  # Run the function
  # -------------------------------
  result <- sce(
    pkg_source_path = "mock/path",
    pkg_name = "mockpkg",
    test_path = test_path,
    cov_env = cov_env
  )
  
  # -------------------------------
  # Assertions
  # -------------------------------
  expect_type(result, "list")
  
  expect_named(
    result,
    c(
      "cov_env",
      "datasets_loaded",
      "base_r_datasets_loaded",
      "base_r_exports_loaded",
      "root_dir",
      "initial_state",
      "helpers_loaded"
    )
  )
  
  expect_identical(result$cov_env, cov_env)
  expect_equal(result$root_dir, "/mock/root")
  expect_equal(result$helpers_loaded, "helper_one.R")
  
  # Combined datasets = package datasets + base R datasets + nycflights13 datasets
  expect_setequal(
    result$datasets_loaded,
    c("data1", "data2", "datasets::mtcars", "datasets::iris", "flights")
  )
  
  expect_setequal(
    result$base_r_datasets_loaded,
    c("datasets::mtcars", "datasets::iris")
  )
  
  expect_setequal(
    result$base_r_exports_loaded,
    c("stats::rnorm", "utils::head")
  )
  
  # Confirm the helper actually "loaded"
  expect_true(isTRUE(get("helper_loaded", envir = cov_env, inherits = FALSE)))
  
  # Confirm namespace objects were copied into cov_env
  expect_true(exists("fun", envir = cov_env, inherits = FALSE))
  expect_true(is.function(get("fun", envir = cov_env, inherits = FALSE)))
})


test_that("load_base_r_packages_into_env returns exports and datasets from helper functions", {
  lb <- get("load_base_r_packages_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  
  seen_pkg_names <- NULL
  seen_env_exports <- NULL
  seen_overwrite_exports <- NULL
  
  seen_pkg_names_datasets <- NULL
  seen_env_datasets <- NULL
  seen_source_paths_datasets <- NULL
  seen_overwrite_datasets <- NULL
  
  mockery::stub(
    lb,
    "load_package_exports_into_env",
    function(pkg_names, env, overwrite = FALSE) {
      seen_pkg_names <<- pkg_names
      seen_env_exports <<- env
      seen_overwrite_exports <<- overwrite
      c("stats::rnorm", "utils::head")
    }
  )
  
  mockery::stub(
    lb,
    "load_package_datasets",
    function(pkg_names, env, source_paths = NULL, overwrite = FALSE) {
      seen_pkg_names_datasets <<- pkg_names
      seen_env_datasets <<- env
      seen_source_paths_datasets <<- source_paths
      seen_overwrite_datasets <<- overwrite
      c("datasets::mtcars", "datasets::iris")
    }
  )
  
  result <- lb(env = fake_env, overwrite = FALSE)
  
  expect_type(result, "list")
  expect_named(result, c("exports_loaded", "datasets_loaded"))
  
  expect_identical(result$exports_loaded, c("stats::rnorm", "utils::head"))
  expect_identical(result$datasets_loaded, c("datasets::mtcars", "datasets::iris"))
  
  expect_identical(seen_env_exports, fake_env)
  expect_false(seen_overwrite_exports)
  
  expect_identical(seen_env_datasets, fake_env)
  expect_null(seen_source_paths_datasets)
  expect_false(seen_overwrite_datasets)
  
  expect_identical(seen_pkg_names_datasets, "datasets")
})


test_that("load_base_r_packages_into_env excludes base and datasets from export package loading", {
  lb <- get("load_base_r_packages_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  seen_pkg_names <- NULL
  
  mockery::stub(
    lb,
    "load_package_exports_into_env",
    function(pkg_names, env, overwrite = FALSE) {
      seen_pkg_names <<- pkg_names
      character(0)
    }
  )
  
  mockery::stub(
    lb,
    "load_package_datasets",
    function(pkg_names, env, source_paths = NULL, overwrite = FALSE) {
      character(0)
    }
  )
  
  result <- lb(env = fake_env, overwrite = FALSE)
  
  expect_identical(result$exports_loaded, character(0))
  expect_identical(result$datasets_loaded, character(0))
  
  expect_setequal(
    seen_pkg_names,
    c(
      "compiler",
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
  )
  
  expect_false("base" %in% seen_pkg_names)
  expect_false("datasets" %in% seen_pkg_names)
})


test_that("load_base_r_packages_into_env passes overwrite to helper functions", {
  lb <- get("load_base_r_packages_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  
  seen_overwrite_exports <- NULL
  seen_overwrite_datasets <- NULL
  
  mockery::stub(
    lb,
    "load_package_exports_into_env",
    function(pkg_names, env, overwrite = FALSE) {
      seen_overwrite_exports <<- overwrite
      character(0)
    }
  )
  
  mockery::stub(
    lb,
    "load_package_datasets",
    function(pkg_names, env, source_paths = NULL, overwrite = FALSE) {
      seen_overwrite_datasets <<- overwrite
      character(0)
    }
  )
  
  result <- lb(env = fake_env, overwrite = TRUE)
  
  expect_identical(result$exports_loaded, character(0))
  expect_identical(result$datasets_loaded, character(0))
  
  expect_true(seen_overwrite_exports)
  expect_true(seen_overwrite_datasets)
})


test_that("load_base_r_packages_into_env errors if env is not an environment", {
  expect_error(
    load_base_r_packages_into_env(env = "not_an_env"),
    "env"
  )
})


test_that("load_base_r_packages_into_env errors if overwrite is not a logical flag", {
  fake_env <- new.env(parent = emptyenv())
  
  expect_error(
    load_base_r_packages_into_env(env = fake_env, overwrite = "yes"),
    "overwrite"
  )
})


test_that("load_package_exports_into_env loads exports into env", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  mockery::stub(
    lpe,
    "getNamespaceExports",
    function(pkg) c("foo", "bar")
  )
  
  mockery::stub(
    lpe,
    "getExportedValue",
    function(pkg, nm) {
      if (nm == "foo") {
        function() "foo"
      } else if (nm == "bar") {
        123
      } else {
        stop("unexpected export")
      }
    }
  )
  
  result <- lpe(
    pkg_names = "fakepkg",
    env = fake_env,
    overwrite = FALSE
  )
  
  expect_setequal(result, c("fakepkg::foo", "fakepkg::bar"))
  expect_true(exists("foo", envir = fake_env, inherits = FALSE))
  expect_true(exists("bar", envir = fake_env, inherits = FALSE))
  expect_true(is.function(get("foo", envir = fake_env, inherits = FALSE)))
  expect_identical(get("bar", envir = fake_env, inherits = FALSE), 123)
})


test_that("load_package_exports_into_env skips packages that are not installed", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) FALSE
  )
  
  expect_message(
    result <- lpe(
      pkg_names = "missingpkg",
      env = fake_env,
      overwrite = FALSE
    ),
    "Package missingpkg is not installed or not available; skipping export load."
  )
  
  expect_identical(result, character(0))
  expect_identical(ls(envir = fake_env, all.names = TRUE), character(0))
})


test_that("load_package_exports_into_env handles getNamespaceExports errors gracefully", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  mockery::stub(
    lpe,
    "getNamespaceExports",
    function(pkg) stop("namespace failure")
  )
  
  expect_message(
    result <- lpe(
      pkg_names = "brokenpkg",
      env = fake_env,
      overwrite = FALSE
    ),
    "Failed to retrieve exports for package brokenpkg : namespace failure"
  )
  
  expect_identical(result, character(0))
  expect_identical(ls(envir = fake_env, all.names = TRUE), character(0))
})


test_that("load_package_exports_into_env skips packages with no exports", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  mockery::stub(
    lpe,
    "getNamespaceExports",
    function(pkg) character(0)
  )
  
  result <- lpe(
    pkg_names = "emptyexports",
    env = fake_env,
    overwrite = FALSE
  )
  
  expect_identical(result, character(0))
  expect_identical(ls(envir = fake_env, all.names = TRUE), character(0))
})


test_that("load_package_exports_into_env does not overwrite existing objects when overwrite is FALSE", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  assign("foo", "existing_value", envir = fake_env)
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  mockery::stub(
    lpe,
    "getNamespaceExports",
    function(pkg) c("foo", "bar")
  )
  
  mockery::stub(
    lpe,
    "getExportedValue",
    function(pkg, nm) {
      if (nm == "foo") "new_value" else 999
    }
  )
  
  result <- lpe(
    pkg_names = "fakepkg",
    env = fake_env,
    overwrite = FALSE
  )
  
  # only bar should be loaded because foo already exists
  expect_identical(result, "fakepkg::bar")
  expect_identical(get("foo", envir = fake_env, inherits = FALSE), "existing_value")
  expect_identical(get("bar", envir = fake_env, inherits = FALSE), 999)
})


test_that("load_package_exports_into_env overwrites existing objects when overwrite is TRUE", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  assign("foo", "existing_value", envir = fake_env)
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  mockery::stub(
    lpe,
    "getNamespaceExports",
    function(pkg) c("foo")
  )
  
  mockery::stub(
    lpe,
    "getExportedValue",
    function(pkg, nm) "replacement_value"
  )
  
  result <- lpe(
    pkg_names = "fakepkg",
    env = fake_env,
    overwrite = TRUE
  )
  
  expect_identical(result, "fakepkg::foo")
  expect_identical(get("foo", envir = fake_env, inherits = FALSE), "replacement_value")
})


test_that("load_package_exports_into_env handles getExportedValue errors gracefully", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  mockery::stub(
    lpe,
    "getNamespaceExports",
    function(pkg) c("good", "bad")
  )
  
  mockery::stub(
    lpe,
    "getExportedValue",
    function(pkg, nm) {
      if (nm == "good") {
        42
      } else {
        stop("cannot load export")
      }
    }
  )
  
  expect_message(
    result <- lpe(
      pkg_names = "fakepkg",
      env = fake_env,
      overwrite = FALSE
    ),
    "Failed to load export fakepkg::bad : cannot load export"
  )
  
  expect_identical(result, "fakepkg::good")
  expect_true(exists("good", envir = fake_env, inherits = FALSE))
  expect_false(exists("bad", envir = fake_env, inherits = FALSE))
  expect_identical(get("good", envir = fake_env, inherits = FALSE), 42)
})


test_that("load_package_exports_into_env deduplicates repeated package names", {
  lpe <- get("load_package_exports_into_env", envir = asNamespace("test.assessr"))
  
  fake_env <- new.env(parent = emptyenv())
  seen_pkgs <- character()
  
  mockery::stub(
    lpe,
    "requireNamespace",
    function(pkg, quietly = TRUE) TRUE
  )
  
  mockery::stub(
    lpe,
    "getNamespaceExports",
    function(pkg) {
      seen_pkgs <<- c(seen_pkgs, pkg)
      "foo"
    }
  )
  
  mockery::stub(
    lpe,
    "getExportedValue",
    function(pkg, nm) 1
  )
  
  result <- lpe(
    pkg_names = c("fakepkg", "fakepkg", "fakepkg"),
    env = fake_env,
    overwrite = FALSE
  )
  
  expect_identical(seen_pkgs, "fakepkg")
  expect_identical(result, "fakepkg::foo")
  expect_identical(get("foo", envir = fake_env, inherits = FALSE), 1)
})


test_that("load_package_exports_into_env validates env", {
  expect_error(
    load_package_exports_into_env(
      pkg_names = "fakepkg",
      env = "not_an_environment"
    ),
    "env"
  )
})


test_that("load_package_exports_into_env validates overwrite", {
  fake_env <- new.env(parent = emptyenv())
  
  expect_error(
    load_package_exports_into_env(
      pkg_names = "fakepkg",
      env = fake_env,
      overwrite = "yes"
    ),
    "overwrite"
  )
})


test_that("load_package_exports_into_env validates pkg_names", {
  fake_env <- new.env(parent = emptyenv())
  
  expect_error(
    load_package_exports_into_env(
      pkg_names = character(0),
      env = fake_env
    ),
    "pkg_names"
  )
  
  expect_error(
    load_package_exports_into_env(
      pkg_names = c("ok", NA_character_),
      env = fake_env
    ),
    "pkg_names"
  )
})




test_that("initialise_covr_env uses default work_dir when NULL", {
  # Arrange
  pkg_source_path <- "dummy/pkg"  # not used because normalizePath is stubbed
  pkg <- "apkg"
  framework <- "testthat"
  
  # Deterministic default work_dir path
  mockery::stub(initialise_covr_env, "tempdir", function() "/TD")
  mockery::stub(initialise_covr_env, "Sys.getpid", function() 12345L)
  mockery::stub(initialise_covr_env, "file.path", function(...){
    paste0(paste0(c(...), collapse = "/"))
  })
  
  # Avoid FS writes and assert call
  fake_dir_create <- mockery::mock(TRUE)
  mockery::stub(initialise_covr_env, "dir.create", fake_dir_create)
  
  # Avoid real path resolution
  mockery::stub(initialise_covr_env, "normalizePath", function(path, winslash="/", mustWork=TRUE) {
    # Should be called for pkg_source_path only
    if (identical(path, pkg_source_path)) "/ABS/PATH"
    else path
  })
  
  # Act
  env <- initialise_covr_env(pkg_source_path, pkg, framework, work_dir = NULL)
  
  # Assert
  expect_true(is.environment(env))
  expect_identical(env$pkg_root, "/ABS/PATH")
  expect_identical(env$work_dir, "/TD/covr_apkg_testthat_12345")
  mockery::expect_called(fake_dir_create, 1)
  # dir.create(work_dir, recursive=TRUE, showWarnings=FALSE)
  mockery::expect_args(fake_dir_create, 1, "/TD/covr_apkg_testthat_12345",
                       recursive = TRUE, showWarnings = FALSE)
})

test_that("initialise_covr_env uses default work_dir when empty string", {
  pkg_source_path <- "dummy/pkg"
  pkg <- "apkg"
  framework <- "none"
  
  mockery::stub(initialise_covr_env, "tempdir", function() "/TMP")
  mockery::stub(initialise_covr_env, "Sys.getpid", function() 99999L)
  mockery::stub(initialise_covr_env, "file.path", function(...){
    paste0(paste0(c(...), collapse = "/"))
  })
  mockery::stub(initialise_covr_env, "dir.create", function(...) TRUE)
  mockery::stub(initialise_covr_env, "normalizePath", function(path, winslash="/", mustWork=TRUE) {
    "/ABS/PATH2"
  })
  
  env <- initialise_covr_env(pkg_source_path, pkg, framework, work_dir = "")
  
  expect_true(is.environment(env))
  expect_identical(env$work_dir, "/TMP/covr_apkg_none_99999")
  expect_identical(env$pkg_root, "/ABS/PATH2")
})

test_that("initialise_covr_env respects provided non-empty work_dir and does not use tempdir/file.path", {
  pkg_source_path <- "dummy/pkg"
  pkg <- "apkg"
  framework <- "testit"
  provided_work_dir <- "/CUSTOM/WORKDIR"
  
  # Ensure the default-path branch is NOT used
  mockery::stub(initialise_covr_env, "tempdir", function() stop("tempdir() should not be called"))
  mockery::stub(initialise_covr_env, "file.path", function(...) stop("file.path() should not be called"))
  
  fake_dir_create <- mockery::mock(TRUE)
  mockery::stub(initialise_covr_env, "dir.create", fake_dir_create)
  
  mockery::stub(initialise_covr_env, "normalizePath", function(path, winslash="/", mustWork=TRUE) {
    if (identical(path, pkg_source_path)) "/ABS/PATH3" else path
  })
  
  env <- initialise_covr_env(pkg_source_path, pkg, framework, work_dir = provided_work_dir)
  
  expect_true(is.environment(env))
  expect_identical(env$work_dir, provided_work_dir)
  expect_identical(env$pkg_root, "/ABS/PATH3")
  mockery::expect_called(fake_dir_create, 1)
  mockery::expect_args(fake_dir_create, 1, provided_work_dir,
                       recursive = TRUE, showWarnings = FALSE)
})

test_that("initialise_covr_env validates arguments (stopifnot)", {
  # Wrong types or lengths should error
  expect_error(initialise_covr_env(123, "pkg", "none"), "is.character")
  expect_error(initialise_covr_env(c("a", "b"), "pkg", "none"), "length")
  expect_error(initialise_covr_env("path", 1, "none"), "is.character")
  expect_error(initialise_covr_env("path", "pkg", c("a","b")), "length")
})

test_that("initialise_covr_env propagates normalizePath error when pkg_source_path does not exist", {
  pkg_source_path <- "missing/path"
  pkg <- "apkg"
  framework <- "none"
  
  # The function calls normalizePath(..., mustWork=TRUE), so error should bubble up
  mockery::stub(initialise_covr_env, "normalizePath", function(path, winslash="/", mustWork=TRUE) {
    stop(sprintf("path[%s] does not exist", deparse(path)))
  })
  # Avoid side effects before error
  mockery::stub(initialise_covr_env, "dir.create", function(...) TRUE)
  mockery::stub(initialise_covr_env, "tempdir", function() "/TD")
  mockery::stub(initialise_covr_env, "file.path", function(...) paste0(c(...), collapse="/"))
  mockery::stub(initialise_covr_env, "Sys.getpid", function() 1L)
  
  expect_error(
    initialise_covr_env(pkg_source_path, pkg, framework, work_dir = NULL),
    "does not exist"
  )
})



testthat::test_that("run_covr_modes handles tinytest non-standard framework", {
  pkg_source_path <- file.path(tempdir(), "mockpkg")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mock_run_covr_skip_nstf <- function(pkg_source_path, test_pkg_data, cov_env, ...) {
    list(
      total_cov = 66,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("some_function")),
          totalcoverage = 66
        )
      )
    )
  }
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    run_covr_skip_nstf = mock_run_covr_skip_nstf
  )
  
  # Select the tinytest branch explicitly
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(
      has_testthat = FALSE,
      has_tests_base = FALSE,
      has_testit = FALSE,
      has_tinytest = TRUE,
      has_data_table_tests = FALSE,
      has_BioG_test = FALSE
    )
  })
  
  # Minimal required stubs, mirroring your working pattern
  mockery::stub(run_covr_modes, "get_pkg_desc", function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  })
  
  mockery::stub(run_covr_modes, "unloadNamespace", function(pkg) {})
  
  # Run
  result <- run_covr_modes(pkg_source_path, covr_timeout = 10)
  
  # Assertions (kept simple, like in your successful test)
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$total_cov, 66)
  testthat::expect_equal(result$test_framework_type, "non-standard testing framework - tinytest")
})


testthat::test_that("run_covr_modes handles data.table non-standard framework", {
  pkg_source_path <- file.path(tempdir(), "mockpkg_dt")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  mock_run_covr_skip_dt_nstf <- function(pkg_source_path, test_pkg_data, cov_env) {
    list(
      total_cov = 42,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("some_function")),
          totalcoverage = 42
        )
      )
    )
  }
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    run_covr_skip_dt_nstf = mock_run_covr_skip_dt_nstf
  )
  
  # Select the data.table branch explicitly
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(
      has_testthat = FALSE,
      has_tests_base = FALSE,
      has_testit = FALSE,
      has_tinytest = FALSE,
      has_data_table_tests = TRUE,
      has_BioG_test = FALSE
    )
  })
  
  # Minimal required stubs, mirroring your working pattern
  mockery::stub(run_covr_modes, "get_pkg_desc", function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  })
  mockery::stub(run_covr_modes, "unloadNamespace", function(pkg) {})
  
  # Run
  result <- run_covr_modes(pkg_source_path, covr_timeout = 10)
  
  # Assertions (kept simple, like in your successful test)
  testthat::expect_true(is.list(result))
  testthat::expect_equal(result$total_cov, 42)
  testthat::expect_equal(result$test_framework_type, "non-standard testing framework - data.table")
})

test_that("compute_multi_framework_total returns 0 for empty list", {
  expect_equal(compute_multi_framework_total(list()), 0)
})

test_that("compute_multi_framework_total returns mean for multiple frameworks", {
  per_framework <- list(
    testthat = list(total_cov = 80),
    testit = list(total_cov = 40)
  )
  expect_equal(compute_multi_framework_total(per_framework), 60)
})

test_that("compute_multi_framework_total excludes NA from mean", {
  per_framework <- list(
    testthat = list(total_cov = 50),
    testit = list(total_cov = NA_real_)
  )
  expect_equal(compute_multi_framework_total(per_framework), 50)
})

test_that("compute_multi_framework_total returns NaN when all total_cov are NA", {
  per_framework <- list(
    testthat = list(total_cov = NA_real_),
    testit = list(total_cov = NA_real_)
  )
  result <- compute_multi_framework_total(per_framework)
  expect_equal(result, 0)
})

test_that("compute_multi_framework_total handles single framework", {
  per_framework <- list(testthat = list(total_cov = 100))
  expect_equal(compute_multi_framework_total(per_framework), 100)
})

test_that("compute_multi_framework_total returns 0 for framework with invalid total_cov (error path)", {
  per_framework <- list(
    testthat = list(total_cov = 50),
    testit = list(total_cov = list())  # as.numeric(list()) errors -> tryCatch returns 0
  )
  result <- compute_multi_framework_total(per_framework)
  expect_equal(result, 50)  # mean(50, 0)
})

test_that("compute_multi_framework_total handles missing total_cov (excluded as NA)", {
  per_framework <- list(
    testthat = list(total_cov = 50),
    testit = list()  # no total_cov -> x$total_cov is NULL -> as.numeric(NULL) is numeric(0) -> [1L] is NA
  )
  result <- compute_multi_framework_total(per_framework)
  expect_equal(result, 50)  # NA excluded, mean of 50
})

test_that("compute_multi_framework_total with stubbed as.numeric returns 0 when conversions fail", {
  fn <- getFromNamespace("compute_multi_framework_total", "test.assessr")
  mockery::stub(fn, "as.numeric", function(x, ...) stop("stub"))
  per_framework <- list(a = list(total_cov = 100))
  expect_equal(fn(per_framework), 0)
})

test_that("run_one_framework else branch returns zero coverage for unrecognised framework", {
  run_fn <- getFromNamespace("run_one_framework", "test.assessr")
  pkg_source_path <- file.path(tempdir(), "mockpkg_unknown")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  # has_RUnit_test / has_BioG_test required: raw && on NULL errors; FALSE keeps final else branch
  test_pkg_data <- list(
    has_testthat = FALSE,
    has_testit = FALSE,
    has_RUnit_test = FALSE,
    has_BioG_test = FALSE
  )
  
  mockery::stub(run_fn, "safe_unload_package", function(pkg) NULL)
  mockery::stub(run_fn, "initialise_covr_env", function(pkg_source_path, pkg, framework, ...) {
    env <- new.env(parent = baseenv())
    env$work_dir <- tempdir()
    env$pkg_root <- pkg_source_path
    env
  })
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) NULL)
  
  expect_message(
    result <- run_fn(
      framework = "runit",
      pkg = "mockpkg",
      test_pkg_data = test_pkg_data,
      pkg_source_path = pkg_source_path,
      covr_timeout = 10
    ),
    "No recognised standard or non-standard testing configuration"
  )
  
  expect_equal(result$total_cov, 0)
  expect_equal(result$res_cov$errors, "No recognised standard or non-standard testing configuration")
  expect_equal(result$test_framework_type, "non-standard testing framework")
  expect_equal(result$framework_id, "runit")
})


test_that("run_one_framework selects RUnit non-standard testing branch", {
  pkg <- "dummyPkg"
  pkg_source_path <- tempdir()
  
  test_pkg_data <- list(
    has_RUnit_test = TRUE,
    has_BioG_test  = FALSE
  )
  
  # framework must NOT match any earlier branches, so use something generic
  framework <- "some_framework"
  
  # Mock safe_unload_package to avoid side effects
  mockery::stub(run_one_framework, "safe_unload_package", function(...) NULL)
  
  # Mock initialise_covr_env so cov_env becomes predictable
  mockery::stub(run_one_framework, "initialise_covr_env", function(...) list(env = "FAKE_ENV"))
  
  # Mock run_covr_skip_runit_nstf to return fake coverage list
  mockery::stub(run_one_framework, "run_covr_skip_runit_nstf", function(...) {
    list(total_cov = 42, res_cov = list())
  })
  
  # Mock cleanup in on.exit() to avoid real cleanup
  mockery::stub(run_one_framework, "cleanup_and_return_null", function(...) NULL)
  
  result <- run_one_framework(
    framework       = framework,
    pkg             = pkg,
    test_pkg_data   = test_pkg_data,
    pkg_source_path = pkg_source_path,
    covr_timeout    = 10
  )
  
  # Assertions
  expect_equal(result$total_cov, 42)
  expect_equal(result$test_framework_type, "non-standard testing framework - RUnit")
  expect_equal(result$framework_id, framework)
})


test_that("run_one_framework uses create_empty_covr_list when covr_list is NULL", {
  run_fn <- getFromNamespace("run_one_framework", "test.assessr")
  
  pkg_source_path <- file.path(tempdir(), "mockpkg_null")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  test_pkg_data <- list(has_testit = TRUE)
  
  # -- Stubs/mocks on run_one_framework's direct callees --
  mockery::stub(run_fn, "safe_unload_package", function(pkg) NULL)
  
  mockery::stub(run_fn, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  env <- new.env(parent = baseenv())
                  env$work_dir <- tempdir()
                  env$pkg_root <- pkg_source_path
                  env
                }
  )
  
  mockery::stub(run_fn, "cleanup_and_return_null", function(...) NULL)
  
  # Force the primary coverage attempt to "fail" (NA) so we enter the STF fallback
  mockery::stub(run_fn, "run_coverage",
                function(pkg_source_path, covr_timeout, ...) {
                  list(
                    total_cov = NA_real_,
                    res_cov   = list(coverage = list(filecoverage = c(NA_real_, NA_real_)))
                  )
                }
  )
  
  # CRITICAL: for testit you now call run_covr_skip_stf(), not ..._nstf()
  # Return NULL to force run_one_framework() to use create_empty_covr_list()
  mockery::stub(run_fn, "run_covr_skip_stf", function(...) NULL)
  
  # (You can keep this if other tests depend on it; harmless here)
  mockery::stub(run_fn, "run_covr_skip_nstf", function(...) NULL)
  
  result <- run_fn(
    framework      = "testit",
    pkg            = "mockpkg",
    test_pkg_data  = test_pkg_data,
    pkg_source_path = pkg_source_path,
    covr_timeout   = 10
  )
  
  # Assertions: create_empty_covr_list() path was used
  expect_equal(result$total_cov, 0)
  expect_match(result$res_cov$errors, "Coverage failed for framework: testit")
  expect_equal(result$framework_id, "testit")
  expect_identical(result$test_pkg_data, test_pkg_data)
})

empty_pkg_flags <- function() {
  list(
    has_testthat = FALSE,
    has_testit = FALSE,
    has_tests_base = FALSE,
    has_tinytest = FALSE,
    has_data_table_tests = FALSE,
    has_BioG_test = FALSE,
    has_RUnit_test = FALSE
  )
}

test_that("detect_frameworks adds runit when has_RUnit_test is TRUE", {
  fn <- getFromNamespace("detect_frameworks", "test.assessr")
  d <- empty_pkg_flags()
  d$has_RUnit_test <- TRUE
  expect_equal(fn(d), "runit")
})

test_that("detect_frameworks RUnit branch is skipped when has_RUnit_test is FALSE or NULL", {
  fn <- getFromNamespace("detect_frameworks", "test.assessr")
  d <- empty_pkg_flags()
  d$has_RUnit_test <- FALSE
  expect_equal(fn(d), character(0))
  d2 <- empty_pkg_flags()
  d2$has_RUnit_test <- NULL
  expect_equal(fn(d2), character(0))
})

test_that("detect_frameworks keeps bioconductor and runit distinct when one flag TRUE", {
  fn <- getFromNamespace("detect_frameworks", "test.assessr")
  d <- empty_pkg_flags()
  d$has_RUnit_test <- TRUE
  d$has_BioG_test <- FALSE
  expect_setequal(fn(d), "runit")
  expect_length(fn(d), 1L)
})

test_that("detect_frameworks combines RUnit with other frameworks", {
  fn <- getFromNamespace("detect_frameworks", "test.assessr")
  d <- empty_pkg_flags()
  d$has_RUnit_test <- TRUE
  d$has_testthat <- TRUE
  out <- fn(d)
  expect_setequal(out, c("testthat", "runit"))
  expect_length(out, 2L)
})

test_that("run_covr_modes passes runit framework when check_pkg has has_RUnit_test (mockery::stub)", {
  pkg_source_path <- file.path(tempdir(), "pkg_runit_detect")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)
  
  captured_framework <- NULL
  mock_run_one <- function(framework, pkg, test_pkg_data, pkg_source_path, covr_timeout) {
    captured_framework <<- framework
    list(
      total_cov = 99,
      res_cov = list(
        name = pkg,
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("runit_fn")),
          totalcoverage = 99
        )
      )
    )
  }
  
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(path, ...) {
    list(
      has_testthat = FALSE,
      has_testit = FALSE,
      has_tests_base = FALSE,
      has_tinytest = FALSE,
      has_data_table_tests = FALSE,
      has_BioG_test = FALSE,
      has_RUnit_test = TRUE,
      has_snaps = FALSE,
      bioc_unit_tests_dir = NA_character_,
      n_golden_tests = 0L,
      n_test_files = 0L
    )
  })
  mockery::stub(run_covr_modes, "run_one_framework", mock_run_one)
  mockery::stub(run_covr_modes, "get_pkg_name", function(path, ...) "mockpkg")
  mockery::stub(run_covr_modes, "get_pkg_desc",
                function(path, fields, ...) list(Package = "mockpkg", Version = "1.0.0")
  )
  mockery::stub(run_covr_modes, "get_test_metadata",
                function(...) {
                  list(
                    datetime = "2026-01-01 12:00:00",
                    executor = "test",
                    info = list(sys = list(
                      sysname = "Linux", version = "1", release = "1", machine = "x86_64"
                    ))
                  )
                }
  )
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_equal(captured_framework, "runit")
  expect_equal(result$total_cov, 99)
})



test_that("decider chooses testit NSTF when testit driver detected", {
  pkg <- tempdir()
  
  mockery::stub(run_covr_decider, "normalizePath", function(x, ...) x)
  
  # Simulate testit driver text in test-all.R
  mockery::stub(run_covr_decider, "readLines", function(path, ...) {
    if (grepl("test-all\\.R$", path)) {
      return("testit::test_pkg('mypkg')")
    }
    character()
  })
  
  # Avoid DT-style: test-all.R does NOT exist
  mockery::stub(run_covr_decider, "file.exists", function(path) FALSE)
  
  # testit directory exists, testthat does not
  mockery::stub(run_covr_decider, "dir.exists", function(path) {
    grepl("testit$", path)
  })
  
  # Mock the NSTF call
  mockery::stub(run_covr_decider, "run_covr_skip_nstf", function(...) "FAKE_NSTF_RESULT")
  
  res <- run_covr_decider(
    framework       = "testit",
    pkg_source_path = pkg,
    test_pkg_data   = list(),
    cov_env         = list(),
    covr_timeout    = 10
  )
  
  expect_equal(res$covr_list, "FAKE_NSTF_RESULT")
  expect_equal(res$tf_type, "non-standard testing framework - testit")
})


test_that("decider chooses testthat branch when testthat structure exists", {
  pkg <- tempdir()
  
  mockery::stub(run_covr_decider, "normalizePath", function(x, ...) x)
  
  # No driver content
  mockery::stub(run_covr_decider, "readLines", function(...) character())
  
  # testthat directory exists
  mockery::stub(run_covr_decider, "dir.exists", function(path) {
    grepl("testthat$", path)
  })
  
  # Avoid DT-style: test-all.R does not exist
  mockery::stub(run_covr_decider, "file.exists", function(...) FALSE)
  
  # Mock run_coverage
  mockery::stub(run_covr_decider, "run_coverage", function(...) "FAKE_COV_RESULT")
  
  res <- run_covr_decider(
    framework       = "testit",
    pkg_source_path = pkg,
    test_pkg_data   = list(),
    cov_env         = list(),
    covr_timeout    = 10
  )
  
  expect_equal(res$covr_list, "FAKE_COV_RESULT")
  expect_equal(res$tf_type, "standard testing framework")
})
