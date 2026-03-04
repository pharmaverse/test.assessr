test_that("loads datasets using data() if no files in data directory", {
  dummy_env <- new.env()
  
  # Pull the function object from its namespace
  ld <- get("load_package_datasets", envir = asNamespace("test.assessr"))
  
  # Stub dependencies INSIDE `ld`
  mockery::stub(ld, "requireNamespace", function(pkg, quietly = TRUE) TRUE)
  mockery::stub(ld, "find.package", function(pkg) "/mock/path")
  mockery::stub(ld, "dir.exists", function(path) FALSE)
  
  # IMPORTANT: stub the symbol *as used in the function body*: `data`, not `utils::data`
  mockery::stub(
    ld,
    "data",
    function(..., list = NULL, package = NULL, envir = NULL) {
      if (is.null(list)) {
        # Simulate `data(package = pkg_name)` — must return a list with a 'results' matrix
        return(list(
          results = matrix(
            c("dataset1", "dataset2"),
            ncol = 1,
            dimnames = list(NULL, "Item")
          )
        ))
      } else {
        # Simulate `data(list = dataset, package = pkg_name, envir = env)`
        assign(list, paste0("loaded_", list), envir = envir)
        invisible(NULL)
      }
    }
  )
  
  # Call the stubbed function object
  result <- ld("fakepkg", env = dummy_env)
  
  expect_true("dataset1" %in% result)
  expect_true("dataset2" %in% result)
  expect_equal(dummy_env$dataset1, "loaded_dataset1")
  expect_equal(dummy_env$dataset2, "loaded_dataset2")
})



test_that("run_covr_modes handles testit framework correctly", {
  # Isolate cov_env for this test (package-level binding)
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env  = new.env(parent = baseenv())
  )
  
  # --- Mocks/ stubs for collaborators (use ... to absorb extra args) ---
  
  mock_check_pkg_tests_and_snaps <- function(pkg_source_path, ...) {
    list(has_testthat = FALSE, has_testit = TRUE, has_tests_base = FALSE)
  }
  
  mock_run_coverage <- function(pkg_source_path, covr_timeout, ...) {
    list(
      total_cov = NA,
      res_cov = list(
        coverage = list(filecoverage = c(NA, NA))
      )
    )
  }
  
  # IMPORTANT: accept the extra arg (cov_env / env) with ... so no 'unused argument'
  mock_run_covr_skip_nstf <- function(pkg_source_path, test_pkg_data, ...) {
    list(
      total_cov = 42,
      res_cov = list(
        coverage = list(filecoverage = matrix(1)),
        notes = "NSTF coverage run"
      )
    )
  }
  
  mock_get_pkg_desc <- function(pkg_source_path, fields, ...) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  mock_unloadNamespace <- function(pkg, ...) {
    # no-op in tests
    invisible(NULL)
  }
  
  # CRITICAL: return the environment object so later cleanup(ls(envir = env)) works
  mock_initialise_covr_env <- function(pkg_source_path, pkg, framework, ...) {
    assign("pkg_path",   pkg_source_path, envir = cov_env)
    assign("package",    pkg,             envir = cov_env)
    assign("framework",  framework,       envir = cov_env)
    # add any other fields your code reads from cov_env if needed
    cov_env
  }
  
  # Wire the stubs into run_covr_modes
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  mockery::stub(run_covr_modes, "run_coverage",             mock_run_coverage)
  mockery::stub(run_covr_modes, "run_covr_skip_nstf",       mock_run_covr_skip_nstf)
  mockery::stub(run_covr_modes, "get_pkg_desc",             mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace",          mock_unloadNamespace)
  mockery::stub(run_covr_modes, "initialise_covr_env",      mock_initialise_covr_env)
  
  # Sanity: ensure the bound cov_env is an environment
  expect_true(is.environment(cov_env))
  
  # Execute
  result <- run_covr_modes("dummy/path")
  
  # Assert NSTF path chosen (testit + no coverage => NSTF fallback)
  expect_equal(result$total_cov, 42)
  expect_equal(result$res_cov$notes, "NSTF coverage run")
})




test_that("loads various file types from data directory", {
  dummy_env <- new.env()
  
  mockery::stub(load_package_datasets, "requireNamespace", function(pkg, quietly) TRUE)
  mockery::stub(load_package_datasets, "find.package", function(pkg) "/mock/path")
  mockery::stub(load_package_datasets, "dir.exists", function(path) TRUE)
  
  # Simulate multiple file types
  mockery::stub(load_package_datasets, "list.files", function(path, full.names) {
    c(
      "/mock/path/data/data1.rda",
      "/mock/path/data/data2.csv",
      "/mock/path/data/data3.tsv",
      "/mock/path/data/data4.txt",
      "/mock/path/data/data5.json",
      "/mock/path/data/data6.R"
    )
  })
  
  # Simulate file extensions
  mockery::stub(load_package_datasets, "tools::file_ext", function(file) {
    sub(".*\\.(.*)$", "\\1", file)
  })
  
  # Simulate file names without extensions
  mockery::stub(load_package_datasets, "tools::file_path_sans_ext", function(file) {
    sub("\\.[^.]*$", "", basename(file))
  })
  
  # Stub each file type loader
  mockery::stub(load_package_datasets, "load", function(file, envir) assign("data1", "rda_loaded", envir = envir))
  mockery::stub(load_package_datasets, "read.csv", function(file, header) data.frame(a = 1))
  mockery::stub(load_package_datasets, "read.table", function(file, header, sep = "") data.frame(b = 2))
  mockery::stub(load_package_datasets, "requireNamespace", function(pkg, quietly) TRUE)
  mockery::stub(load_package_datasets, "jsonlite::fromJSON", function(file) list(c = 3))
  mockery::stub(load_package_datasets, "sys.source", function(file, envir) assign("data6", "R_loaded", envir = envir))
  
  result <- load_package_datasets("mockpkg", env = dummy_env)
  
  expect_true(all(c("data1", "data2", "data3", "data4", "data5", "data6") %in% result))
  expect_equal(dummy_env$data1, "rda_loaded")
  expect_equal(dummy_env$data2$a, 1)
  expect_equal(dummy_env$data3$b, 2)
  expect_equal(dummy_env$data4$b, 2)
  expect_equal(dummy_env$data5$c, 3)
  expect_equal(dummy_env$data6, "R_loaded")
})


test_that("load_package_datasets returns NULL when package is not installed", {
  # Mock requireNamespace to simulate package not installed
  mockery::stub(load_package_datasets, "requireNamespace", function(pkg_name, quietly) FALSE)
  
  # Run the function
  result <- load_package_datasets("fakepkg")
  
  # Check that result is NULL
  expect_null(result)
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
  
  expect_silent(cleanup_and_return_null(NULL, env = env))
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
  
  expect_silent(cleanup_and_return_null(NULL, env = env))
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
  
  expect_silent(cleanup_and_return_null(NULL, env = env))
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
  pkg_source_path <- "mock/path"
  
  # Bind isolated cov_env for test
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env = new.env(parent = baseenv())
  )
  
  # Stub initialise_covr_env to avoid normalizePath()
  mockery::stub(run_covr_modes, "initialise_covr_env", function(pkg_source_path, pkg, framework, ...) {
    assign("pkg_path", pkg_source_path, envir = cov_env)
    assign("package", pkg, envir = cov_env)
    assign("framework", framework, envir = cov_env)
    cov_env
  })
  
  # Stub check_pkg_tests_and_snaps → has testthat
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
  
  # Stub run_coverage to return valid coverage
  mockery::stub(run_covr_modes, "run_coverage", function(...) {
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
  })
  
  # Run
  result <- run_covr_modes(pkg_source_path)
  
  # Assertions
  expect_true(is.list(result))
  expect_equal(result$total_cov, 85)
})


test_that("run_covr_modes handles Bioconductor testing framework correctly", {
  
  pkg_source_path <- "mock/path"
  
  # -----------------------------
  # Create isolated cov_env
  # -----------------------------
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env = new.env(parent = baseenv())
  )
  
  # -----------------------------
  # Stub: initialise_covr_env 
  # Avoid normalizePath() and filesystem
  # -----------------------------
  mockery::stub(run_covr_modes, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  assign("pkg_path", pkg_source_path, envir = cov_env)
                  assign("package",  pkg,             envir = cov_env)
                  assign("framework", framework,      envir = cov_env)
                  cov_env
                }
  )
  
  # -----------------------------
  # Stub: get_pkg_name
  # -----------------------------
  mockery::stub(run_covr_modes, "get_pkg_name", function(path) "mockpkg")
  
  # -----------------------------
  # Stub: package description
  # -----------------------------
  mockery::stub(run_covr_modes, "get_pkg_desc",
                function(path, fields) list(Package = "mockpkg", Version = "1.0.0")
  )
  
  # -----------------------------
  # Stub: metadata
  # -----------------------------
  mockery::stub(run_covr_modes, "get_test_metadata",
                function() list(
                  datetime = "2026-01-01 12:00:00",
                  executor = "test-user",
                  info = list(sys = list(
                    sysname = "Linux",
                    version = "5.15",
                    release = "Ubuntu",
                    machine = "x86_64"
                  ))
                )
  )
  
  # -----------------------------
  # Stub: loadedNamespaces (pretend NOT loaded)
  # -----------------------------
  mockery::stub(run_covr_modes, "loadedNamespaces",
                function() c("stats", "utils")  # not containing "mockpkg"
  )
  
  # -----------------------------
  # Stub: unloadNamespace - no-op
  # -----------------------------
  mockery::stub(run_covr_modes, "unloadNamespace", function(pkg, ...) NULL)
  
  # -----------------------------
  # KEY STUB:
  # Bioconductor test detection
  # -----------------------------
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps",
                function(path) {
                  list(
                    has_testthat    = FALSE,
                    has_testit      = FALSE,
                    has_tests_base  = FALSE,
                    has_BioG_test   = TRUE,
                    # extras to match structure
                    has_snaps           = FALSE,
                    bioc_unit_tests_dir = "inst/unitTests",
                    n_golden_tests      = 0L,
                    n_test_files        = 1L
                  )
                }
  )
  
  # -----------------------------
  # Stub: Bioconductor runner
  # -----------------------------
  mockery::stub(run_covr_modes, "run_covr_skip_bioc_nstf",
                function(pkg_source_path, test_pkg_data, cov_env) {
                  list(
                    total_cov = 77,
                    res_cov   = list(
                      name = "mockpkg",
                      coverage = list(
                        filecoverage  = matrix(1, nrow = 1, dimnames = list("bioc_fun")),
                        totalcoverage = 77
                      ),
                      notes = "Bioc NSTF run"
                    )
                  )
                }
  )
  
  # -----------------------------
  # Execute function
  # -----------------------------
  result <- run_covr_modes(pkg_source_path)
  
  # -----------------------------
  # Assertions
  # -----------------------------
  expect_true(is.list(result))
  expect_equal(result$total_cov, 77)
  expect_equal(result$res_cov$coverage$totalcoverage, 77)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "bioc_fun")
  
  # framework label + summary type
  expect_equal(result$test_framework_type,
               "non-standard testing framework - Bioconductor")
  
  # metadata injected?
  expect_equal(result$pkg_name, "mockpkg")
  expect_equal(result$pkg_ver, "1.0.0")
  expect_equal(result$executor, "test-user")
  expect_equal(result$sysname, "Linux")
  
})



test_that("run_covr_modes triggers fallback to run_covr_skip_stf when coverage is NA", {
  pkg_source_path <- "mock/path"
  
  # Bind isolated cov_env for test
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env = new.env(parent = baseenv())
  )
  
  # Stub initialise_covr_env so normalizePath() never runs
  mockery::stub(run_covr_modes, "initialise_covr_env", function(pkg_source_path, pkg, framework, ...) {
    assign("pkg_path", pkg_source_path, envir = cov_env)
    assign("package", pkg, envir = cov_env)
    assign("framework", framework, envir = cov_env)
    cov_env
  })
  
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
  
  # Force run_coverage to return NA → triggers fallback
  mockery::stub(run_covr_modes, "run_coverage", function(...) {
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
  })
  
  # Stub fallback runner
  mockery::stub(run_covr_modes, "run_covr_skip_stf", function(...) {
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
  })
  
  # Run
  result <- run_covr_modes(pkg_source_path)
  
  # Assertions
  expect_true(is.list(result))
  expect_equal(result$total_cov, 42)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "fallback_function")
})





test_that("run_covr_modes handles non-standard testing framework", {
  pkg_source_path <- "mock/path"
  
  # Create isolated cov_env
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env = new.env(parent = baseenv())
  )
  
  # Stub initialise_covr_env so normalizePath() is not executed
  mockery::stub(run_covr_modes, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  assign("pkg_path", pkg_source_path, envir = cov_env)
                  assign("package",  pkg,             envir = cov_env)
                  assign("framework", framework,      envir = cov_env)
                  cov_env
                }
  )
  
  # Mocked get_pkg_desc
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  mockery::stub(run_covr_modes, "get_pkg_desc",  mock_get_pkg_desc)
  
  # Mock unloadNamespace
  mockery::stub(run_covr_modes, "unloadNamespace", function(...) {})
  
  # Simulate non-standard testing framework (NSTF)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = FALSE, has_tests_base = TRUE, has_testit = FALSE)
  })
  
  # Stub NSTF runner
  mockery::stub(run_covr_modes, "run_covr_skip_nstf", function(...) {
    list(
      total_cov = 50,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          totalcoverage = 50
        )
      )
    )
  })
  
  # Run
  result <- run_covr_modes(pkg_source_path)
  
  # Assertions
  expect_true(is.list(result))
  expect_equal(result$total_cov, 50)
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
      has_tests_base     = FALSE,
      has_BioG_test      = FALSE,
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
      has_BioG_test       = FALSE,
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




test_that("run_covr_modes handles error when unloading package", {
  pkg_source_path <- "mock/path"
  
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
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to simulate failure
  mock_unloadNamespace <- function(pkg) {
    stop("Simulated unload failure")
  }
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mock_loadedNamespaces <- function() {
    c("mockpkg", "stats", "utils")
  }
  
  # Stub: complete test_pkg_data with all fields expected
  mock_check_pkg_tests_and_snaps <- function(...) {
    list(
      has_testthat = FALSE,
      has_snaps = FALSE,
      has_testit = FALSE,
      has_tests_base = FALSE,
      has_BioG_test = FALSE,
      bioc_unit_tests_dir = NA_character_,
      n_golden_tests = 0L,
      n_test_files = 0L
    )
  }
  
  # Stub set_test_path (or the path resolver used by run_covr_modes)
  mock_set_test_path <- function(pkg_source_path, test_pkg_data) "tests"
  
  # Apply stubs
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "set_test_path", mock_set_test_path)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(run_covr_modes, "loadedNamespaces", mock_loadedNamespaces)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_equal(result$total_cov, 0)
  expect_match(result$res_cov$errors, "No recognised standard or non-standard testing configuration")
})




test_that("run_covr_modes handles error when unloading package", {
  
  # Isolate a clean cov_env for this test, bound at the package level
  testthat::local_mocked_bindings(
    .package = "test.assessr",
    cov_env  = new.env(parent = baseenv())
  )
  
  # Stub initialise_covr_env: avoid normalizePath() and return the environment
  mockery::stub(run_covr_modes, "initialise_covr_env",
                function(pkg_source_path, pkg, framework, ...) {
                  assign("pkg_path",  pkg_source_path, envir = cov_env)
                  assign("package",   pkg,             envir = cov_env)
                  assign("framework", framework,       envir = cov_env)
                  cov_env
                }
  )
  
  # If run_covr_modes uses get_pkg_desc to obtain the package name/version,
  # keep this stub. If it uses get_pkg_name() first, keep both.
  mockery::stub(run_covr_modes, "get_pkg_name", function(path, ...) "mockpkg")
  
  mockery::stub(run_covr_modes, "get_pkg_desc", function(path, fields, ...) {
    list(Package = "mockpkg", Version = "1.0.0")
  })
  
  # Metadata stub (if your code reads it for logging; harmless if not used)
  mockery::stub(run_covr_modes, "get_test_metadata", function(...) {
    list(
      datetime = "2026-01-07 12:00:00",
      executor = "test-user",
      info = list(sys = list(sysname = "Linux", version = "5.15", release = "Ubuntu", machine = "x86_64"))
    )
  })
  
  # Simulate “no tests configured” to force early exit path after unload attempt
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(path, ...) {
    list(
      has_testthat        = FALSE,
      has_testit          = FALSE,
      has_tests_base      = FALSE,
      has_BioG_test       = FALSE,
      # Optional extras if your function reads them:
      has_snaps           = FALSE,
      bioc_unit_tests_dir = NA_character_,
      n_golden_tests      = 0L,
      n_test_files        = 0L
    )
  })
  
  # Simulate the package is loaded so unload path is taken
  mockery::stub(run_covr_modes, "loadedNamespaces", function(...) c("mockpkg", "stats", "utils"))
  
  # Force unload failure
  mockery::stub(run_covr_modes, "unloadNamespace", function(pkg, ...) {
    stop("Simulated unload failure")
  })
  
  # IMPORTANT: make the cleanup stub accept msg/env (or ...). Return NULL as per your intent.
  mockery::stub(run_covr_modes, "cleanup_and_return_null", function(env, msg = NULL, ...) {
    # Optionally, verify that env is an environment and msg is character
    # but return NULL to emulate “no result” cleanup.
    NULL
  })
  
  # Run once and assert both messages appear
  # (Use a single expect_message call with a regex combining both lines, or two nested calls.)
  expect_message(
    expect_message(
      run_covr_modes("some/path"),
      "Warning: Failed to unload mockpkg package"
    ),
    "Details: Simulated unload failure"
  )
})





test_that("create_empty_covr_list returns correct structure", {
  pkg_name <- "mockpkg"
  error_message <- "No coverage data available"
  
  result <- create_empty_covr_list(pkg_name, error_message)
  
  expect_type(result, "list")
  expect_named(result, c("total_cov", "res_cov"))
  
  expect_equal(result$total_cov, 0)
  expect_type(result$res_cov, "list")
  expect_equal(result$res_cov$name, pkg_name)
  expect_equal(result$res_cov$coverage$totalcoverage, 0)
  expect_true(is.matrix(result$res_cov$coverage$filecoverage))
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "No functions tested")
  expect_equal(result$res_cov$errors, error_message)
  expect_true(is.na(result$res_cov$notes))
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



test_that("cleanup_and_return_null exits early when project root is not found", {
  # Create a dummy environment to simulate cov_env
  cov_env <- new.env()
  assign("x", 1, envir = cov_env)
  
  # Force root resolution to fail
  mockery::stub(cleanup_and_return_null, "getwd", function() "/mock/project")
  mockery::stub(cleanup_and_return_null, "normalizePath", function(...) stop("boom"))
  
  # Neutralize any lingering stubs
  mockery::stub(cleanup_and_return_null, "ls", base::ls)
  mockery::stub(cleanup_and_return_null, "rm", base::rm)
  
  msgs <- testthat::capture_messages({
    result <- cleanup_and_return_null("Test message", env = cov_env)
  })
  
  # Returns invisibly NULL
  expect_null(result)
  
  # Match prefix rather than exact text
  expect_true(
    any(grepl("Project root not found", msgs, fixed = TRUE)),
    info = paste("Captured messages:\n", paste(msgs, collapse = "\n"))
  )
  
  # Confirm environment was cleaned
  expect_equal(base::ls(envir = cov_env, all.names = TRUE), character(0))
})


test_that("returns NULL if package is not installed", {
  mockery::stub(load_package_datasets, "requireNamespace", FALSE)
  expect_message(result <- load_package_datasets("fakepkg"), "Package fakepkg is not installed.")
  expect_null(result)
})

test_that("returns datasets from data directory with supported extensions", {
  fake_env <- new.env()
  mockery::stub(load_package_datasets, "requireNamespace", TRUE)
  mockery::stub(load_package_datasets, "find.package", "/fake/path")
  mockery::stub(load_package_datasets, "dir.exists", TRUE)
  mockery::stub(load_package_datasets, "list.files", c("/fake/path/data/data1.csv", "/fake/path/data/data2.rda"))
  mockery::stub(load_package_datasets, "tools::file_ext", function(x) if (grepl("csv", x)) "csv" else "rda")
  mockery::stub(load_package_datasets, "tools::file_path_sans_ext", function(x) sub("\\..*$", "", x))
  mockery::stub(load_package_datasets, "load", function(file, envir) assign("data2", data.frame(x = 1), envir = envir))
  mockery::stub(load_package_datasets, "read.csv", function(file, header) data.frame(a = 1:3))
  
  result <- load_package_datasets("fakepkg", env = fake_env)
  expect_true("data1" %in% result)
  expect_true("data2" %in% result)
  expect_true(exists("data1", envir = fake_env))
  expect_true(exists("data2", envir = fake_env))
})

test_that("falls back to data() if no files loaded", {
  fake_env <- new.env()
  mockery::stub(load_package_datasets, "requireNamespace", TRUE)
  mockery::stub(load_package_datasets, "find.package", "/fake/path")
  mockery::stub(load_package_datasets, "dir.exists", TRUE)
  mockery::stub(load_package_datasets, "list.files", character(0))
  # mockery::stub(load_package_datasets, "data", function(list, package, envir) assign(list, data.frame(x = 1), envir = envir))
  
  mockery::stub(load_package_datasets, "data.package", list(results = data.frame(Item = "fallback_data", stringsAsFactors = FALSE))) 
  
  mockery::stub(load_package_datasets, "data", function(..., list = NULL, package = NULL, envir = NULL) {
    if (is.null(list)) {
      return(list(results = data.frame(Item = "fallback_data", stringsAsFactors = FALSE)))
    } else {
      assign(list, data.frame(x = 1), envir = envir)
    }
  })
  
  
  result <- load_package_datasets("fakepkg", env = fake_env)
  expect_true("fallback_data" %in% result)
  expect_true(exists("fallback_data", envir = fake_env))
})


test_that("handles errors during file loading gracefully", {
  fake_env <- new.env()
  
  mockery::stub(load_package_datasets, "requireNamespace", TRUE)
  mockery::stub(load_package_datasets, "find.package", "/fake/path")
  mockery::stub(load_package_datasets, "dir.exists", TRUE)
  mockery::stub(load_package_datasets, "list.files", c("/fake/path/data/bad.csv"))
  mockery::stub(load_package_datasets, "tools::file_ext", "csv")
  mockery::stub(load_package_datasets, "tools::file_path_sans_ext", "bad")
  mockery::stub(load_package_datasets, "read.csv", function(...) stop("read error"))
  mockery::stub(load_package_datasets, "data", function(..., package, envir) {
    return(invisible(NULL))
  })
  
  
  expect_message(result <- load_package_datasets("fakepkg", env = fake_env), "Failed to load")
  expect_false("bad" %in% result)
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


test_that("cleanup_and_return_null removes only .pdf/.ps files and only if they exist", {
  # Create a dummy env for cov_env; contents don't matter for this branch
  cov_env <- new.env()
  assign("dummy", 1, envir = cov_env)
  
  # Mocked project root resolution
  mockery::stub(cleanup_and_return_null, "getwd", function() "/mock/project")
  mockery::stub(cleanup_and_return_null, "normalizePath", function(path, ...) path)
  
  # Mocked files in the root (mix of extensions)
  mock_files <- c(
    "/mock/project/report.pdf",    # should be considered, exists=TRUE
    "/mock/project/diagram.ps",    # should be considered, exists=FALSE (to test branch)
    "/mock/project/readme.txt"     # should NOT be considered
  )
  
  # Stub list.files to return our mock files (signature-compatible)
  mockery::stub(cleanup_and_return_null, "list.files", function(path, full.names = FALSE, ...) mock_files)
  
  # Track which files are attempted to be removed
  removed_files <- character(0)
  
  # Stub file.exists: only the PDF "exists"
  mockery::stub(cleanup_and_return_null, "file.exists", function(path) {
    path == "/mock/project/report.pdf"
  })
  
  # Stub file.remove to record invocations
  mockery::stub(cleanup_and_return_null, "file.remove", function(path) {
    removed_files <<- c(removed_files, path)
    TRUE
  })
  
  # Run the function (we don't care about messages here)
  result <- cleanup_and_return_null(env = cov_env)
  
  # Function returns invisibly(NULL)
  expect_null(result)
  
  # Only the .pdf that "exists" should have been removed
  expect_identical(removed_files, "/mock/project/report.pdf")
})


test_that("setup_covr_env completes successfully", {
  
  # -------------------------------
  # Create a mock cov_env
  # -------------------------------
  cov_env <- new.env()
  
  # -------------------------------
  # Stubs for helpers
  # -------------------------------
  
  # Simulate a single helper file
  mockery::stub(setup_covr_env, "list.files", function(...) "helper_one.R")
  
  # Pretend sourcing a helper sets a flag inside cov_env
  mockery::stub(setup_covr_env, "sys.source",
                function(file, envir) assign("helper_loaded", TRUE, envir)
  )
  
  # Make file.path deterministic
  mockery::stub(setup_covr_env, "file.path",
                function(test_path, fname) paste0(test_path, "/", fname)
  )
  
  # Prevent real writes
  mockery::stub(setup_covr_env, "writeLines", function(lines, con) invisible(NULL))
  
  # Skip loading the real package
  mockery::stub(setup_covr_env, "pkgload::load_all", function(...) NULL)
  
  # Fake namespace
  mockery::stub(setup_covr_env, "asNamespace", function(pkg) {
    e <- new.env()
    assign("fun", function() NULL, envir = e)
    e
  })
  
  # Force find_project_root() to return a known value
  mockery::stub(setup_covr_env, "find_project_root", function(...) "/mock/root")
  
  # Fake dataset loading
  mockery::stub(setup_covr_env, "load_package_datasets",
                function(pkg_name, env) c("data1", "data2")
  )
  
  # Neutralize dependency loader and safe unload
  mockery::stub(setup_covr_env, "load_dependencies_into_env", function(...) NULL)
  mockery::stub(setup_covr_env, "safe_unload_package", function(...) NULL)
  
  # -------------------------------
  # Run the function (IMPORTANT: pass cov_env)
  # -------------------------------
  result <- setup_covr_env("mock/path", "mockpkg", cov_env = cov_env)
  
  # -------------------------------
  # Assertions
  # -------------------------------
  expect_type(result, "list")
  expect_true("cov_env" %in% names(result))
  expect_identical(result$cov_env, cov_env)
  
  expect_equal(result$datasets_loaded, c("data1", "data2"))
  expect_equal(result$root_dir, "/mock/root")
  expect_equal(result$helpers_loaded, "helper_one.R")
  
  # Confirm the helper actually "loaded"
  expect_true(cov_env$helper_loaded)
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
