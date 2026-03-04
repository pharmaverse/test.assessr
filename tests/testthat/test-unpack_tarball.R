
test_that("Unpacking a tar file works correctly (mocked tar)", {
  # Create a fake tarball path (no real tar content needed)
  fake_tar <- tempfile(fileext = ".tar.gz")
  file.create(fake_tar)
  
  # Ensure the fake tarball is cleaned up
  withr::defer(unlink(fake_tar), envir = parent.frame())
  
  # Mock utils::untar to simulate extraction side effects
  # NOTE: unpack_tarball() calls utils::untar(..., exdir=..., tar="internal")
  testthat::local_mocked_bindings(
    .package = "utils",
    untar = function(tarfile, exdir, tar = "internal", ...) {
      # Simulate extraction: create a single top-level directory
      out_dir <- file.path(exdir, "test.package.0001")
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      # Minimal structure so fs::dir_ls(exdir) returns exactly this dir
      writeLines(
        c("Package: test.package.0001", "Version: 0.1.0"),
        file.path(out_dir, "DESCRIPTION")
      )
      invisible(0)
    }
  )
  
  # Run the function under test against the fake tarball
  extract_files <- suppressWarnings(unpack_tarball(fake_tar))
  
  # Defer cleanup of whatever directory/path is returned
  withr::defer(unlink(extract_files, recursive = TRUE), envir = parent.frame())
  
  # --- Keep your current assertions unchanged ---
  expect_true(checkmate::checkVector(extract_files))
  expect_true(checkmate::check_class(extract_files, "fs_path"))
  expect_true(checkmate::check_string(extract_files))
  expect_true(checkmate::check_directory_exists(extract_files))
})



test_that("Unpacking an empty tar file works correctly", {
  # Create a fake tarball path (no real tar content needed)
  fake_tar <- tempfile(fileext = ".tar.gz")
  file.create(fake_tar)
  
  # Defer cleanup of the fake tarball
  withr::defer(unlink(fake_tar), envir = parent.frame())
  
  # Mock: make untar() create an empty extraction directory
  mock_untar <- function(tarfile, exdir, tar = "internal", ...) {
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    invisible(0)
  }
  
  # Stub utils::untar inside unpack_tarball()
  mockery::stub(unpack_tarball, "utils::untar", mock_untar)
  
  # Call the function under test
  extract_files <- suppressWarnings(unpack_tarball(fake_tar))
  
  # --- Keep your current assertions unchanged ---
  expect_true(checkmate::checkVector(extract_files))
  
  expect_true(checkmate::check_class(extract_files, "fs_path"))
  
  expect_identical(checkmate::check_string(extract_files), 
                   "Must have length 1"
  )
  
  expect_identical(checkmate::check_directory_exists(extract_files), 
                   "No directory provided"
  )
})

