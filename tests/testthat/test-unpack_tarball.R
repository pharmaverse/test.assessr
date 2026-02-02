test_that("Unpacking a tar file works correctly", {
  
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  options(repos = r)
  
  # Copy test package to a temp file
  pkg_orig <- system.file("test-data", "here-1.0.1.tar.gz", 
                         package = "test.assessr")
  pkg <- tempfile(fileext = ".tar.gz")
  file.copy(pkg_orig, pkg)
  
  # Defer cleanup of copied tarball
  withr::defer(unlink(pkg), envir = parent.frame())
  
  extract_files <-  
    suppressWarnings(unpack_tarball(pkg))
  
  expect_true(checkmate::checkVector(extract_files))
  
  expect_true(checkmate::check_class(extract_files, "fs_path"))
  
  expect_true(checkmate::check_string(extract_files))
  
  expect_true(checkmate::check_directory_exists(extract_files))
})

test_that("Unpacking an empty tar file works correctly", {
  
  # Copy test package to a temp file
  pkg_orig <- system.file("test-data", "empty.tar.gz", 
                          package = "test.assessr")
  pkg <- tempfile(fileext = ".tar.gz")
  file.copy(pkg_orig, pkg)
  
  # Defer cleanup of copied tarball
  withr::defer(unlink(pkg), envir = parent.frame())
  
  extract_files <-  
    suppressWarnings(unpack_tarball(pkg))
  
  expect_true(checkmate::checkVector(extract_files))
  
  expect_true(checkmate::check_class(extract_files, "fs_path"))
  
  expect_identical(checkmate::check_string(extract_files), 
                   "Must have length 1"
  )
  
  expect_identical(checkmate::check_directory_exists(extract_files), 
                   "No directory provided"
  )
})  
