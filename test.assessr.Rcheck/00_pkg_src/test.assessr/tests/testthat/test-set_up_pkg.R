test_that("set up package for tar file with default check type", {
  
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  withr::local_options(list(repos = r))
  skip_if_repo_unavailable()
  
  # Copy test package to a temp file
  dp_orig <- system.file("test-data", 
                         "test.package.0001_0.1.0.tar.gz", 
                         package = "test.assessr")
  dp <- tempfile(fileext = ".tar.gz")
  file.copy(dp_orig, dp)
  
  # Defer cleanup of copied tarball
  withr::defer(unlink(dp), envir = parent.frame())
  
  # set up package
  install_list <- set_up_pkg(dp)
  
  expect_identical(length(install_list), 2L)
  
  expect_true(checkmate::check_list(install_list, 
                                    any.missing = FALSE)
  )
  
  expect_true(checkmate::check_list(install_list, 
                                    types = c("logical",
                                              "character",
                                              "list")
                                    )
  )
})

test_that("set_up_pkg handles missing pkg_source_path correctly (length 0)", {
  # Use a dummy dp (doesn't matter, as we stub the relevant function)
  dp <- "dummy/path/to/package.tar.gz"
  
  # stub the internal function that returns pkg_source_path to always return length 0
  mockery::stub(set_up_pkg, "unpack_tarball", character(0))
 
  # Run function
  install_list <- set_up_pkg(dp)
  
  # Expectation: install_list should match the state in the code block:
  # c(package_installed, results, pkg_source_path, out_dir, build_vignettes)
  expect_identical(length(install_list), 2L)
  
  expect_false(install_list[[1]])
  expect_identical(install_list[[2]], "")

})

test_that("set_up_pkg sets package_installed TRUE when unpacked path exists on disk", {
  dp <- "dummy/path/to/package.tar.gz"
  fake_path <- "/tmp/fake_pkg_dir"
  # unpack_tarball returns a non-empty path -> enters else branch (lines 47-51)
  mockery::stub(set_up_pkg, "unpack_tarball", fake_path)
  # fs::file_exists returns TRUE -> package_installed <- TRUE (lines 48-50)
  mockery::stub(set_up_pkg, "fs::file_exists", function(x) TRUE)
  install_list <- set_up_pkg(dp)
  expect_identical(length(install_list), 2L)
  expect_true(install_list$package_installed)
  expect_identical(install_list$pkg_source_path, fake_path)
})

test_that("set_up_pkg returns package_installed FALSE when unpacked path does not exist on disk", {
  dp <- "dummy/path/to/package.tar.gz"
  fake_path <- "/tmp/nonexistent_pkg_dir"
  # unpack_tarball returns a non-empty path -> enters else branch
  mockery::stub(set_up_pkg, "unpack_tarball", fake_path)
  # fs::file_exists returns FALSE -> package_installed <- FALSE
  mockery::stub(set_up_pkg, "fs::file_exists", function(x) FALSE)
  install_list <- set_up_pkg(dp)
  expect_identical(length(install_list), 2L)
  expect_false(install_list$package_installed)
  expect_identical(install_list$pkg_source_path, fake_path)
})

