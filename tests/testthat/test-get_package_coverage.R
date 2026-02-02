test_that("get_package_coverage works with mocked file.choose", {
  skip_on_cran()
  
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
  mockery::stub(get_package_coverage, "run_covr_modes",  mock_run_covr_modes)
  
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
  # Stub set_up_pkg to return a minimal valid install_list
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
