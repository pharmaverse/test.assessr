test_that("set up package for tar file with default check type", {
  
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  options(repos = r)
  
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




