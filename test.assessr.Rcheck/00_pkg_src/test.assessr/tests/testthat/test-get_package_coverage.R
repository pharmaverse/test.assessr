test_that("get_package_coverage works with mocked file.choose", {
  skip_on_cran()
  
  
  # Temporarily replace the cov_env binding for this test only
  testthat::local_mocked_bindings(
    .package = "test.assessr",  # your package name
    cov_env  = new.env(parent = baseenv())
  )
  
  
  # --- Mocks ---
  # 1) Mock file chooser to a known test tarball
  mock_file_choose <- mockery::mock(
    system.file("test-data", "test.package.0001_0.1.0.tar.gz", package = "test.assessr")
  )
  
  # 2) Mock run_covr_modes to avoid calling run_coverage() and other internals
  mock_run_covr_modes <- mockery::mock({
    pkg_name <- "test.package.0001"
    pkg_ver  <- "0.1.0"
    list(
      # ---- metadata fields (10) ----
      pkg_name = pkg_name,
      pkg_ver  = pkg_ver,
      date_time = "2025-01-01 12:00:00",  # any string is fine for the test
      executor  = "tester",
      sysname   = "Windows",
      version   = "10.0",
      release   = "build",
      machine   = "x86_64",
      r_version = "4.3.2",
      test_framework_type = "standard testing framework",
      # ---- coverage payload (2) ----
      total_cov = 1,  # 100% as a fraction, per your run_coverage()
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage  = c(func1 = 100),
          totalcoverage = 100
        ),
        errors = NA,
        notes  = NA
      )
    )
  })
  
  # --- Stub the symbols used inside get_package_coverage() ---
  mockery::stub(get_package_coverage, "file.choose",     mock_file_choose)
  mockery::stub(get_package_coverage, "file.exists",     function(path) TRUE)
  mockery::stub(get_package_coverage, "set_up_pkg",      function(pkg_source_path) {
    list(pkg_source_path = pkg_source_path)
  })
  mockery::stub(get_package_coverage, "install_package_local", function(pkg_source_path) TRUE)
  mockery::stub(get_package_coverage, "run_covr_modes",  mock_run_covr_modes)
  mockery::stub(get_package_coverage, "cleanup_and_return_null", function(...) invisible(NULL))
  
  # --- Execute ---
  tpc <- get_package_coverage()
  
  # --- Assertions (as in your original test) ---
  testthat::expect_identical(length(tpc), 12L)
  testthat::expect_true(checkmate::check_class(tpc, "list"))
  
  # Optional: verify mocks were actually invoked
  mockery::expect_called(mock_file_choose, 1)
  mockery::expect_called(mock_run_covr_modes, 1)
})



test_that("get_package_coverage warns and returns NULL if chosen file does not exist", {
  skip_on_cran()
  
  # Mock file.choose to return a fake path
  fake_path <- "some/non/existing/path.tar.gz"
  mock_file_choose <- function() fake_path
  
  # Stub file.choose and file.exists
  mockery::stub(get_package_coverage, "file.choose", mock_file_choose)
  mockery::stub(get_package_coverage, "file.exists", function(path) FALSE)
  
  # Capture the warning
  expect_warning(
    result <- get_package_coverage(),
    regexp = paste("The specified path", fake_path, "does not exist. Returning NULL.")
  )
  expect_null(result)
})

test_that("get_package_coverage outputs message and returns NULL when package installation fails", {
  skip_on_cran()
  
  # Use a valid path so file.exists returns TRUE
  fake_path <- "some/existing/path.tar.gz"
  mock_file_choose <- function() fake_path
  
  # Stub file.choose to return the fake path
  mockery::stub(get_package_coverage, "file.choose", mock_file_choose)
  # Stub file.exists to return TRUE for our fake path
  mockery::stub(get_package_coverage, "file.exists", function(path) TRUE)
  # Stub set_up_pkg to return a minimal valid setup result
  mockery::stub(get_package_coverage, "set_up_pkg", function(pkg_source_path) {
    list(
      build_vignettes = FALSE,
      package_installed = FALSE,
      pkg_source_path = fake_path,
      rcmdcheck_args = NULL
    )
  })
  # Stub install_package_local to return FALSE (simulate installation failure)
  mockery::stub(get_package_coverage, "install_package_local", function(pkg_source_path) FALSE)
  
  # Capture the message and check result is NULL (since get_package_coverage never sets it in the else block)
  expect_message(
    result <- get_package_coverage(),
    regexp = "Package installation failed."
  )
  expect_null(result)
})

###########################






test_that("on.exit restores NULL when old covr.record_tests was NULL (IF branch)", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  
  options_calls <- list()
  
  # Work on a local copy so stubs bind to it, then call that copy
  fn <- test.assessr::get_package_coverage
  
  ## --- Stub heavy internals as they appear in the body ---
  mockery::stub(fn, "set_up_pkg", function(path, ...) {
    list(package_installed = TRUE, pkg_source_path = path)
  })
  mockery::stub(fn, "test.assessr::set_up_pkg", function(path, ...) {
    list(package_installed = TRUE, pkg_source_path = path)
  })
  mockery::stub(fn, "install_package_local", function(...) TRUE)
  mockery::stub(fn, "test.assessr::install_package_local", function(...) TRUE)
  mockery::stub(fn, "run_covr_modes", function(...) "dummy-cov")
  mockery::stub(fn, "test.assessr::run_covr_modes", function(...) "dummy-cov")
  mockery::stub(fn, "cleanup_and_return_null", function(...) invisible(NULL))
  mockery::stub(fn, "test.assessr::cleanup_and_return_null", function(...) invisible(NULL))
  
  ## --- Control what the function *sees* for old value ---
  stub_getOption <- function(x, default = NULL) {
    if (identical(x, "covr.record_tests")) return(NULL)  # force IF branch
    if (identical(x, "repos")) return(c(CRAN = "https://example-cran", RSPM = "https://example-rspm"))
    base::getOption(x, default)
  }
  mockery::stub(fn, "getOption", stub_getOption)
  
  ## --- Capture options() arguments reliably (preserve NULL, evaluate others) ---
  stub_options <- function(...) {
    dots_expr <- as.list(substitute(list(...)))[-1]
    eval_env <- parent.frame()
    args <- lapply(dots_expr, function(expr) if (identical(expr, quote(NULL))) NULL else eval(expr, eval_env))
    names(args) <- names(dots_expr)
    options_calls <<- append(options_calls, list(args))
    invisible(NULL)
  }
  mockery::stub(fn, "options", stub_options)
  
  ## --- Make on.exit run immediately to avoid harness interference ---
  mockery::stub(fn, "on.exit", function(expr, add = FALSE) {
    eval.parent(substitute(expr), n = 1L)
    invisible(NULL)
  })
  
  # Call the stubbed copy
  res <- fn(path = tempdir())
  expect_identical(res, "dummy-cov")
  
  # Filter only options() calls that touched covr.record_tests
  covr_calls <- Filter(function(x) "covr.record_tests" %in% names(x), options_calls)
  
  # We expect:
  #   - set to TRUE in the body
  #   - restore to NULL via (immediately executed) on.exit
  expect_gte(length(covr_calls), 2L)
  expect_identical(covr_calls[[1]][["covr.record_tests"]], TRUE)
  expect_null(covr_calls[[length(covr_calls)]][["covr.record_tests"]])
})


test_that("on.exit restores previous non-NULL covr.record_tests (ELSE branch)", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  
  options_calls <- list()
  old_value <- FALSE
  
  fn <- test.assessr::get_package_coverage
  
  ## --- Stub heavy internals as they appear in the body ---
  mockery::stub(fn, "set_up_pkg", function(path, ...) {
    list(package_installed = TRUE, pkg_source_path = path)
  })
  mockery::stub(fn, "test.assessr::set_up_pkg", function(path, ...) {
    list(package_installed = TRUE, pkg_source_path = path)
  })
  mockery::stub(fn, "install_package_local", function(...) TRUE)
  mockery::stub(fn, "test.assessr::install_package_local", function(...) TRUE)
  mockery::stub(fn, "run_covr_modes", function(...) "dummy-cov")
  mockery::stub(fn, "test.assessr::run_covr_modes", function(...) "dummy-cov")
  mockery::stub(fn, "cleanup_and_return_null", function(...) invisible(NULL))
  mockery::stub(fn, "test.assessr::cleanup_and_return_null", function(...) invisible(NULL))
  
  ## --- Control what the function *sees* as old value ---
  stub_getOption <- function(x, default = NULL) {
    if (identical(x, "covr.record_tests")) return(old_value)  # force ELSE branch
    if (identical(x, "repos")) return(c(CRAN = "https://example-cran", RSPM = "https://example-rspm"))
    base::getOption(x, default)
  }
  mockery::stub(fn, "getOption", stub_getOption)
  
  ## --- Capture options() arguments reliably (preserve NULL, evaluate others) ---
  stub_options <- function(...) {
    dots_expr <- as.list(substitute(list(...)))[-1]
    eval_env <- parent.frame()
    args <- lapply(dots_expr, function(expr) if (identical(expr, quote(NULL))) NULL else eval(expr, eval_env))
    names(args) <- names(dots_expr)
    options_calls <<- append(options_calls, list(args))
    invisible(NULL)
  }
  mockery::stub(fn, "options", stub_options)
  
  ## --- Make on.exit run immediately ---
  mockery::stub(fn, "on.exit", function(expr, add = FALSE) {
    eval.parent(substitute(expr), n = 1L)
    invisible(NULL)
  })
  
  res <- fn(path = tempdir())
  expect_identical(res, "dummy-cov")
  
  covr_calls <- Filter(function(x) "covr.record_tests" %in% names(x), options_calls)
  expect_gte(length(covr_calls), 2L)
  expect_identical(covr_calls[[1]][["covr.record_tests"]], TRUE)          # set in body
  expect_identical(covr_calls[[length(covr_calls)]][["covr.record_tests"]], old_value)  # restored
})

test_that("get_package_coverage skips installation when package_installed is TRUE", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  
  fn <- test.assessr::get_package_coverage
  pkg_source_path <- tempdir()
  expected <- list(total_cov = 0.8, res_cov = list(name = "dummy", coverage = list(totalcoverage = 80)))
  
  mock_run_covr_modes <- mockery::mock(expected)
  mock_set_up_pkg <- mockery::mock(list(pkg_source_path = pkg_source_path))
  mock_install_local <- mockery::mock(stop("install_package_local should not be called"))
  mock_file_exists <- mockery::mock(TRUE)
  
  mockery::stub(fn, "run_covr_modes", mock_run_covr_modes)
  mockery::stub(fn, "set_up_pkg", mock_set_up_pkg)
  mockery::stub(fn, "install_package_local", mock_install_local)
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  result <- fn(path = pkg_source_path, package_installed = TRUE)
  
  expect_identical(result, expected)
  mockery::expect_called(mock_file_exists, 1)
  mockery::expect_called(mock_run_covr_modes, 1)
  mockery::expect_called(mock_set_up_pkg, 0)
  mockery::expect_called(mock_install_local, 0)
  mockery::expect_args(mock_run_covr_modes, 1, pkg_source_path)
})

test_that("get_package_coverage installs package when package_installed is NULL", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  
  fn <- test.assessr::get_package_coverage
  pkg_source_path <- tempdir()
  expected <- list(total_cov = 0.8, res_cov = list(name = "dummy", coverage = list(totalcoverage = 80)))
  
  mock_file_exists <- mockery::mock(TRUE)
  mock_set_up_pkg <- mockery::mock(list(pkg_source_path = pkg_source_path))
  mock_install_local <- mockery::mock(TRUE)
  mock_run_covr_modes <- mockery::mock(expected)
  
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "set_up_pkg", mock_set_up_pkg)
  mockery::stub(fn, "install_package_local", mock_install_local)
  mockery::stub(fn, "run_covr_modes", mock_run_covr_modes)
  mockery::stub(fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  result <- fn(path = pkg_source_path, package_installed = NULL)
  
  expect_identical(result, expected)
  mockery::expect_called(mock_file_exists, 1)
  mockery::expect_called(mock_set_up_pkg, 1)
  mockery::expect_called(mock_install_local, 1)
  mockery::expect_called(mock_run_covr_modes, 1)
})

test_that("get_package_coverage installs package when package_installed is FALSE", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  
  fn <- test.assessr::get_package_coverage
  pkg_source_path <- tempdir()
  expected <- list(total_cov = 0.8, res_cov = list(name = "dummy", coverage = list(totalcoverage = 80)))
  
  mock_file_exists <- mockery::mock(TRUE)
  mock_set_up_pkg <- mockery::mock(list(pkg_source_path = pkg_source_path))
  mock_install_local <- mockery::mock(TRUE)
  mock_run_covr_modes <- mockery::mock(expected)
  
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "set_up_pkg", mock_set_up_pkg)
  mockery::stub(fn, "install_package_local", mock_install_local)
  mockery::stub(fn, "run_covr_modes", mock_run_covr_modes)
  mockery::stub(fn, "cleanup_and_return_null", function(...) invisible(NULL))
  
  result <- fn(path = pkg_source_path, package_installed = FALSE)
  
  expect_identical(result, expected)
  mockery::expect_called(mock_file_exists, 1)
  mockery::expect_called(mock_set_up_pkg, 1)
  mockery::expect_called(mock_install_local, 1)
  mockery::expect_called(mock_run_covr_modes, 1)
})

test_that("get_package_coverage warns and returns NULL when setup pkg_source_path is missing", {
  skip_on_cran()
  skip_if_not_installed("mockery")
  
  fn <- test.assessr::get_package_coverage
  fake_path <- "some/existing/path.tar.gz"
  
  mock_file_exists <- mockery::mock(TRUE)
  mock_set_up_pkg <- mockery::mock(list(pkg_source_path = NULL))
  mock_install_local <- mockery::mock(stop("install_package_local should not be called"))
  mock_run_covr_modes <- mockery::mock(stop("run_covr_modes should not be called"))
  
  mockery::stub(fn, "file.exists", mock_file_exists)
  mockery::stub(fn, "set_up_pkg", mock_set_up_pkg)
  mockery::stub(fn, "install_package_local", mock_install_local)
  mockery::stub(fn, "run_covr_modes", mock_run_covr_modes)
  
  expect_warning(
    result <- fn(path = fake_path, package_installed = FALSE),
    regexp = "`pkg_source_path` is missing after setup. Returning NULL."
  )
  
  expect_null(result)
  mockery::expect_called(mock_file_exists, 1)
  mockery::expect_called(mock_set_up_pkg, 1)
  mockery::expect_called(mock_install_local, 0)
  mockery::expect_called(mock_run_covr_modes, 0)
})
