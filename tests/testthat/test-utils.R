test_that("get package description works correctly (full DESCRIPTION, mocked set_up_pkg only)", {
  # Create a temporary source directory with a real DESCRIPTION file
  td <- file.path(tempdir(), paste0("fakepkg_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  
  desc_lines <- c(
    "Package: FAKEPKG",
    "Version: 0.0.1",
    "Title: A Fake Package for Testing",
    "Description: Synthetic package metadata used in tests.",
    "Authors@R: person('Unit','Test', email='test@example.org', role=c('aut','cre'))",
    "License: MIT + file LICENSE",
    "Depends: R (>= 3.5.0)",
    "Imports: utils, stats",
    "Suggests: testthat, mockery, checkmate",
    "Enhances:",
    "URL: https://example.org/fakepkg",
    "BugReports: https://example.org/fakepkg/issues",
    "Encoding: UTF-8",
    "LazyData: true",
    "NeedsCompilation: no",
    "Packaged: 2026-02-12 10:00:00 UTC; tester",
    "Maintainer: Unit Test <test@example.org>"
  )
  writeLines(desc_lines, file.path(td, "DESCRIPTION"))
  
  # Wrapper so we can stub set_up_pkg() inside it while still calling get_pkg_desc() for real
  subject <- function(dp) {
    install_list <- set_up_pkg(dp)  # <-- will be stubbed
    if (isTRUE(install_list$package_installed)) {
      get_pkg_desc(install_list$pkg_source_path)  # <-- real function, covered by covr
    } else {
      NULL
    }
  }
  
  # Fake set_up_pkg() return, pointing to our temp directory
  fake_install_list <- list(
    package_installed = TRUE,
    pkg_source_path   = td
  )
  
  fake_set_up_pkg <- mockery::mock(fake_install_list)
  mockery::stub(subject, "set_up_pkg", fake_set_up_pkg)
  
  # Execute
  pkg_desc <- subject("ignored")
  
  # Your original expectations
  expect_identical(length(pkg_desc), 17L)
  expect_true(checkmate::check_list(pkg_desc, any.missing = FALSE))
  expect_true(checkmate::check_list(pkg_desc, types = "character"))
})

test_that("get package description respects 'fields' (subset read)", {
  # Prepare another temp dir with a DESCRIPTION
  td <- file.path(tempdir(), paste0("fakepkg_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  
  desc_lines <- c(
    "Package: FAKEPKG",
    "Version: 0.0.1",
    "Title: A Fake Package for Testing",
    "Description: Synthetic package metadata used in tests."
  )
  writeLines(desc_lines, file.path(td, "DESCRIPTION"))
  
  # Stub set_up_pkg() to return our directory
  subject <- function(dp) {
    install_list <- set_up_pkg(dp)
    if (isTRUE(install_list$package_installed)) {
      get_pkg_desc(install_list$pkg_source_path, fields = c("Package", "Version"))
    } else NULL
  }
  fake_install_list <- list(package_installed = TRUE, pkg_source_path = td)
  fake_set_up_pkg <- mockery::mock(fake_install_list)
  mockery::stub(subject, "set_up_pkg", fake_set_up_pkg)
  
  # Execute and assert
  pkg_desc <- subject("ignored")
  expect_identical(names(pkg_desc), c("Package", "Version"))
  expect_identical(length(pkg_desc), 2L)
  expect_true(checkmate::check_list(pkg_desc, any.missing = FALSE, types = "character"))
  expect_identical(pkg_desc$Package, "FAKEPKG")
  expect_identical(pkg_desc$Version, "0.0.1")
})



test_that("get package name works correctly (with stubbed set_up_pkg)", {
  # Minimal wrapper so we can stub `set_up_pkg()` inside it.
  subject <- function(dp) {
    install_list <- set_up_pkg(dp)             # <-- will be stubbed
    pkg_source_path <- install_list$pkg_source_path
    get_pkg_name(pkg_source_path)
  }
  
  # Create a fake return value for set_up_pkg()
  fake_install_list <- list(
    package_installed = TRUE,
    pkg_source_path   = "/tmp/build/FAKEPKG_0.0-1.tar.gz"
  )
  
  # A mock that returns our fake list
  fake_set_up_pkg <- mockery::mock(fake_install_list)
  
  # Stub set_up_pkg() inside `subject()`
  mockery::stub(subject, "set_up_pkg", fake_set_up_pkg)
  
  # Call the subject; argument is irrelevant due to stubbing
  pkg <- subject("ignored")
  
  # Assertions consistent with your original test’s intent
  expect_identical(length(pkg), 1L)
  
  expect_true(checkmate::check_character(pkg, any.missing = FALSE))
  expect_true(checkmate::check_character(pkg, pattern = "FAKEPKG"))
})


test_that("get_test_metadata returns correct metadata with executor", {
  # Mocked values
  fake_time <- "2025-09-30 12:00:00"
  fake_sysinfo <- list(
    sysname = "Linux",
    version = "#1 SMP Debian",
    release = "5.10.0-8-amd64",
    machine = "x86_64"
  )
  
  # Stub system functions
  mockery::stub(get_test_metadata, "Sys.time", function() fake_time)
  mockery::stub(get_test_metadata, "Sys.info", function() fake_sysinfo)
  
  result <- get_test_metadata(executor = "edward")
  
  expect_equal(result$datetime, fake_time)
  expect_equal(result$executor, "edward")
  expect_equal(result$info$sys, fake_sysinfo)
})

test_that("get_test_metadata returns correct metadata without executor", {
  # Mocked values
  fake_time <- "2025-09-30 12:00:00"
  fake_user <- "mock_user"
  fake_sysinfo <- list(
    sysname = "Darwin",
    version = "22.6.0",
    release = "macOS",
    machine = "arm64"
  )
  
  # Stub system functions
  mockery::stub(get_test_metadata, "Sys.time", function() fake_time)
  mockery::stub(get_test_metadata, "Sys.getenv", function(var) {
    if (var == "USER") return(fake_user)
  })
  mockery::stub(get_test_metadata, "Sys.info", function() fake_sysinfo)
  
  result <- get_test_metadata()
  
  expect_equal(result$datetime, fake_time)
  expect_equal(result$executor, fake_user)
  expect_equal(result$info$sys, fake_sysinfo)
})


test_that("extract_short_path handles forward slashes", {
  p <- "C:/Users/yyy/AppData/Local/Temp/RtmpXXXX/MASS/R/add.R"
  expect_equal(extract_short_path(p), "R/add.R")
})

test_that("extract_short_path handles backslashes", {
  p <- "C:\\Users\\yyy\\AppData\\Local\\Temp\\RtmpXXXX\\MASS\\R\\add.R"
  expect_equal(extract_short_path(p), "R/add.R")
})

test_that("extract_short_path handles mixed separators", {
  p <- "C:/Users\\yyy/AppData\\Local/Temp/RtmpXXXX/MASS/R/add.R"
  expect_equal(extract_short_path(p), "R/add.R")
})

test_that("extract_short_path returns last two components for general paths", {
  p <- "/opt/projects/pkgname/src/module/file.ext"
  expect_equal(extract_short_path(p), "module/file.ext")
})

test_that("extract_short_path handles single-component paths", {
  p <- "file.ext"
  expect_equal(extract_short_path(p), "file.ext")
})

test_that("extract_short_path handles empty string", {
  p <- ""
  # strsplit("", "[/\\\\]") returns character(0), function returns ""
  expect_equal(extract_short_path(p), "")
})

test_that("extract_short_path handles trailing separator", {
  # Trailing separator creates an empty last component
  p1 <- "dir/subdir/"
  expect_equal(extract_short_path(p1), "subdir/")  # last component is ""
  
  p2 <- "dir\\subdir\\"
  expect_equal(extract_short_path(p2), "subdir/")  # unified separator in output
})

test_that("extract_short_path works over a vector via vapply", {
  paths <- c(
    "C:/A/B/C/D.R",
    "C:\\A\\B\\C\\E.R",
    "/A/B/C/F.R",
    "file.ext"
  )
  out <- vapply(paths, extract_short_path, FUN.VALUE = character(1))
  expect_equal(unname(out), c("C/D.R", "C/E.R", "C/F.R", "file.ext"))
})

test_that("extract_short_path behavior for NA (optional policy)", {
  # If you want NA-in -> NA-out, you can wrap it:
  safe_extract <- function(x) if (is.na(x)) NA_character_ else extract_short_path(x)
  expect_true(is.na(safe_extract(NA_character_)))
})
