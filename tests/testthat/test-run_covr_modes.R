

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
  # Mocked test_pkg_data with has_testit = TRUE
  mock_check_pkg_tests_and_snaps <- function(pkg_source_path) {
    list(has_testthat = FALSE, has_testit = TRUE, has_tests_base = FALSE)
  }
  
  # Mocked covr_list with NA coverage
  mock_run_coverage <- function(pkg_source_path, covr_timeout) {
    list(
      total_cov = NA,
      res_cov = list(
        coverage = list(filecoverage = c(NA, NA))
      )
    )
  }
  
  # Stub for run_covr_skip_nstf
  mock_run_covr_skip_nstf <- function(pkg_source_path, test_pkg_data) {
    list(
      total_cov = 42,
      res_cov = list(
        coverage = list(filecoverage = matrix(1)),
        notes = "NSTF coverage run"
      )
    )
  }
  
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  mock_unloadNamespace <- function(pkg) {
    message(paste0("Mock unloadNamespace called for ", pkg))
  }
  
  # Stub all dependencies
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  mockery::stub(run_covr_modes, "run_coverage", mock_run_coverage)
  mockery::stub(run_covr_modes, "run_covr_skip_nstf", mock_run_covr_skip_nstf)
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  
  
  # Run the function
  result <- run_covr_modes("dummy/path")
  
  # Check that the NSTF path was taken
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
  
  # Stub check_pkg_tests_and_snaps to simulate standard testthat presence
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = TRUE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  mock_unloadNamespace <- function(pkg) {
    message(paste0("Mock unloadNamespace called for ", pkg))
  }
  
  # Stub run_coverage to return complete mock coverage data
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
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 85)
})


test_that("run_covr_modes triggers fallback to run_covr_skip_stf when coverage is NA", {
  pkg_source_path <- "mock/path"
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  mock_unloadNamespace <- function(pkg) {
    message(paste0("Mock unloadNamespace called for ", pkg))
  }
  
  # Stub check_pkg_tests_and_snaps to simulate standard testthat presence
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = TRUE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  # Stub run_coverage to return NA values to trigger fallback
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
  
  # Stub run_covr_skip_stf to return fallback coverage data
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
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 42)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "fallback_function")
})



test_that("run_covr_modes handles non-standard testing framework", {
  pkg_source_path <- "mock/path"
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to avoid side effects
  mock_unloadNamespace <- function(pkg) {
    message(paste0("Mock unloadNamespace called for ", pkg))
  }
  
  # Stub check_pkg_tests_and_snaps to simulate non-standard test presence
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = FALSE, has_tests_base = TRUE, has_testit = FALSE)
  })
  
  # Stub run_covr_skip_nstf to return mock coverage data
  mockery::stub(run_covr_modes, "run_covr_skip_nstf", function(...) {
    list(total_cov = 50, res_cov = list(name = "mockpkg", coverage = list(totalcoverage = 50)))
  })
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 50)
})

test_that("run_covr_modes handles no testing configuration", {
  pkg_source_path <- "mock/path"
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to avoid side effects
  mock_unloadNamespace <- function(pkg) {
    message(paste0("Mock unloadNamespace called for ", pkg))
  }
  
  # return a complete test_pkg_data ----
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
  
  # Stub dependencies correctly
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  
  # Run the function
  result <- run_covr_modes(pkg_source_path)
  
  # Assertions
  expect_true(is.list(result))
  expect_equal(result$total_cov, 0)
  expect_match(result$res_cov$errors, "No recognised standard or non-standard testing configuration")
})




test_that("run_covr_modes unloads package when loaded", {
  pkg_source_path <- "mock/path"
  
  # Mock get_pkg_desc to return a known package
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to simulate successful unload
  mock_unloadNamespace <- function(pkg) {
    message(paste0("Mock unloadNamespace called for ", pkg))
  }
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mock_loadedNamespaces <- function() {
    c("mockpkg", "stats", "utils")
  }
  
  # ---- Critical: return a COMPLETE test_pkg_data ----
  mock_check_pkg_tests_and_snaps <- function(...) {
    list(
      has_testthat = FALSE,
      has_snaps = FALSE,
      has_testit = FALSE,
      has_tests_base = FALSE,
      has_BioG_test = FALSE,         # <- required to avoid length-0 condition
      bioc_unit_tests_dir = NA_character_,
      n_golden_tests = 0L,
      n_test_files = 0L
    )
  }
  
  # Stub dependencies
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(run_covr_modes, "loadedNamespaces", mock_loadedNamespaces)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_equal(result$total_cov, 0)
  expect_match(result$res_cov$errors, "No recognised standard or non-standard testing configuration")
})




test_that("run_covr_modes handles error when unloading package", {
  pkg_source_path <- "mock/path"
  
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
  # Stub get_pkg_name to return a mock package name
  mockery::stub(run_covr_modes, "get_pkg_name", function(path) "mockpkg")
  
  # Stub get_pkg_desc to return mock package description
  mockery::stub(run_covr_modes, "get_pkg_desc", function(path, fields) {
    list(Package = "mockpkg", Version = "1.0.0")
  })
  
  # Stub get_test_metadata to return mock metadata
  mockery::stub(run_covr_modes, "get_test_metadata", function() {
    list(datetime = "2026-01-07 12:00:00",
         executor = "test-user",
         info = list(sys = list(sysname = "Linux",
                                version = "5.15",
                                release = "Ubuntu",
                                machine = "x86_64")))
  })
  
  # Stub check_pkg_tests_and_snaps to return no tests
  # IMPORTANT: Include has_BioG_test = FALSE to match the new code logic
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(path) {
    list(
      has_testthat = FALSE,
      has_testit = FALSE,
      has_tests_base = FALSE,
      has_BioG_test = FALSE  # <-- ADD THIS LINE
    )
  })
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mockery:: stub(run_covr_modes, "loadedNamespaces", function() c("mockpkg"))
  
  # Stub unloadNamespace to throw an error
  mockery::stub(run_covr_modes, "unloadNamespace", function(pkg) {
    stop("Simulated unload failure")
  })
  
  # Stub cleanup_and_return_null to do nothing
  mockery::stub(run_covr_modes, "cleanup_and_return_null", function(env, preserve_vars) NULL)
  
  # Expect warning messages to be printed
  expect_message(run_covr_modes("some/path"), "Warning: Failed to unload mockpkg package")
  expect_message(run_covr_modes("some/path"), "Details: Simulated unload failure")
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
  
  # Stub find_project_root to return NULL
  mockery::stub(cleanup_and_return_null, "find_project_root", function() NULL)
  
  # Capture message output
  expect_message(
    result <- cleanup_and_return_null("Test message", env = cov_env),
    "Project root not found."
  )
  
  # Check that the function returns invisibly
  expect_null(result)
  
  # Check that the environment was cleaned
  expect_equal(ls(envir = cov_env), character(0))
})


test_that("cleanup_and_return_null continues when project root is found", {
  # Create a dummy environment to simulate cov_env
  cov_env <- new.env()
  assign("x", 1, envir = cov_env)
  
  # Stub find_project_root to return a fake path
  mockery::stub(cleanup_and_return_null, "find_project_root", function() "/fake/path")
  
  # Capture message output
  expect_message(
    result <- cleanup_and_return_null("Test message", env = cov_env),
    "Test message"
  )
  
  
  # Check that the environment was cleaned
  expect_equal(ls(envir = cov_env), character(0))
})


test_that("cleanup_and_return_null exits early when root_dir is NULL", {
  # Create a dummy environment to simulate cov_env
  cov_env <- new.env()
  assign("x", 1, envir = cov_env)
  
  # Stub find_project_root to return NULL
  mockery::stub(cleanup_and_return_null, "find_project_root", function() NULL)
  
  # Capture the message and result
  expect_message(
    result <- cleanup_and_return_null("Test message", env = cov_env),
    "Project root not found."
  )
  
  # Check that the environment was cleaned
  expect_equal(ls(envir = cov_env), character(0))
  
  # Check that the function returns invisibly
  expect_null(result)
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


test_that("fails if DESCRIPTION file is missing", {
  env <- new.env()
  mockery::stub(load_dependencies_into_env, "requireNamespace", TRUE)
  mockery::stub(load_dependencies_into_env, "file.exists", FALSE)
  expect_error(load_dependencies_into_env("fakepkg", env), "DESCRIPTION file not found")
})

test_that("installs remotes if not available", {
  env <- new.env()
  mockery::stub(load_dependencies_into_env, "requireNamespace", FALSE)
  mockery::stub(load_dependencies_into_env, "install.packages", NULL)
  mockery::stub(load_dependencies_into_env, "library", NULL)
  mockery::stub(load_dependencies_into_env, "file.exists", FALSE)
  expect_error(load_dependencies_into_env("fakepkg", env), "DESCRIPTION file not found")
})

test_that("parses and installs dependencies", {
  env <- new.env()
  desc <- matrix(c("pkgA (>= 1.0.0), pkgB", "pkgC"), nrow = 1, dimnames = list(NULL, c("Imports", "Suggests")))
  
  mockery::stub(load_dependencies_into_env, "requireNamespace", TRUE)
  mockery::stub(load_dependencies_into_env, "file.exists", TRUE)
  mockery::stub(load_dependencies_into_env, "read.dcf", desc)
  mockery::stub(load_dependencies_into_env, "installed.packages", matrix(c("pkgB", "1.0.0"), ncol = 2, dimnames = list(NULL, c("Package", "Version"))))
  mockery::stub(load_dependencies_into_env, "packageVersion", function(pkg) "0.9.0")
  mockery::stub(load_dependencies_into_env, "remotes::install_version", NULL)
  mockery::stub(load_dependencies_into_env, "install.packages", NULL)
  mockery::stub(load_dependencies_into_env, "library", NULL)
  expect_equal(load_dependencies_into_env("fakepkg", env), invisible(TRUE))
})


test_that("ignores base packages", {
  env <- new.env()
  desc <- matrix(c("stats, utils", ""), nrow = 1, dimnames = list(NULL, c("Imports", "Suggests")))
  
  mockery::stub(load_dependencies_into_env, "requireNamespace", TRUE)
  mockery::stub(load_dependencies_into_env, "file.exists", TRUE)
  mockery::stub(load_dependencies_into_env, "read.dcf", desc)
  mockery::stub(load_dependencies_into_env, "installed.packages", matrix(c("stats", "utils"), ncol = 2))
  mockery::stub(load_dependencies_into_env, "library", NULL)
  
  expect_equal(load_dependencies_into_env("fakepkg", env), invisible(TRUE))
})

test_that("handles install/load errors gracefully", {
  env <- new.env()
  desc <- matrix(c("pkgX", ""), nrow = 1, dimnames = list(NULL, c("Imports", "Suggests")))
  
  # Create a mock remotes namespace with install_version
  mock_remotes <- new.env()
  mock_remotes$install_version <- function(...) stop("install error")
  assign("remotes", mock_remotes, envir = .GlobalEnv)
  
  # Stub required functions
  mockery::stub(load_dependencies_into_env, "requireNamespace", TRUE)
  mockery::stub(load_dependencies_into_env, "file.exists", TRUE)
  mockery::stub(load_dependencies_into_env, "read.dcf", desc)
  mockery::stub(load_dependencies_into_env, "installed.packages", matrix(character(0), ncol = 2))
  
  mockery::stub(load_dependencies_into_env, "install.packages", function(pkg) stop("install error"))
  
  # Stub library() to allow loading remotes but fail for others
  mockery::stub(load_dependencies_into_env, "library", function(pkg, character.only = FALSE) {
    pkg_name <- if (character.only) pkg else deparse(substitute(pkg))
    if (pkg_name == "remotes") return(invisible(NULL))
    stop("load error")
  })
  
  
  expect_message(load_dependencies_into_env("fakepkg", env), "Failed to install package: pkgX")
  expect_message(load_dependencies_into_env("fakepkg", env), "Failed to load package: pkgX")
  
  # Clean up mock
  rm(remotes, envir = .GlobalEnv)
})

test_that("setup_covr_env completes successfully", {
  mock_env <- new.env()
  
  mockery::stub(setup_covr_env, "list.files", c("helper_one.R"))
  mockery::stub(setup_covr_env, "sys.source", function(file, envir) assign("helper_loaded", TRUE, envir))
  
  
  # --- Special helper stubs ---
  # Ensure file.path returns a deterministic path for the special helper (optional)
  mockery::stub(setup_covr_env, "file.path", function(test_path, fname) {
    # Return a clean, predictable path the function will attempt to write to
    paste0(test_path, "/", fname)
  })
  
  # Avoid filesystem writes by stubbing writeLines
  mockery::stub(setup_covr_env, "writeLines", function(lines, con) {
    # Simulate success; you can capture 'con' if you want to assert it later
    invisible(NULL)
  })
  
  
  mockery::stub(setup_covr_env, "pkgload::load_all", NULL)
  mockery::stub(setup_covr_env, "asNamespace", function(pkg) {
    e <- new.env()
    assign("fun", function() NULL, envir = e)
    e
  })
  mockery::stub(setup_covr_env, "find_project_root", "/mock/root")
  mockery::stub(setup_covr_env, "load_package_datasets", function(pkg_name, env) c("data1", "data2"))
  mockery::stub(setup_covr_env, "load_dependencies_into_env", NULL)
  mockery::stub(setup_covr_env, "safe_unload_package", NULL)
  
  result <- setup_covr_env("mock/path", "mockpkg")
  
  expect_type(result, "list")
  expect_true("cov_env" %in% names(result))
  expect_equal(result$datasets_loaded, c("data1", "data2"))
  expect_equal(result$root_dir, "/mock/root")
  expect_true("helper_one.R" %in% result$helpers_loaded)
})





