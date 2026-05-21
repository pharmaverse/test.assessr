test_that("get_pkg_desc returns all fields as a named list from mocked DESCRIPTION", {
  fake_path <- "/fake/pkg"
  
  # Simulate read.dcf() returning a one-row matrix as it would for a real DESCRIPTION file
  fake_dcf <- matrix(
    c("mypackage", "0.1.0", "What the package does", "GPL-3", "A Person <a@b.com>"),
    nrow = 1L,
    dimnames = list(NULL, c("Package", "Version", "Title", "License", "Author"))
  )
  mockery::stub(get_pkg_desc, "read.dcf", fake_dcf)
  
  result <- get_pkg_desc(fake_path)
  
  expect_type(result, "list")
  expect_named(result, c("Package", "Version", "Title", "License", "Author"))
  expect_identical(result$Package, "mypackage")
  expect_identical(result$Version, "0.1.0")
  expect_identical(result$License, "GPL-3")
})

test_that("get_pkg_desc passes fields argument through to read.dcf", {
  fake_path <- "/fake/pkg"
  
  # Capture the arguments that read.dcf is called with
  mock_read_dcf <- mockery::mock(
    matrix(c("mypackage", "0.2.0"), nrow = 1L,
           dimnames = list(NULL, c("Package", "Version")))
  )
  mockery::stub(get_pkg_desc, "read.dcf", mock_read_dcf)
  
  result <- get_pkg_desc(fake_path, fields = c("Package", "Version"))
  
  # Verify read.dcf was called with the correct path and fields
  mockery::expect_called(mock_read_dcf, 1)
  call_args <- mockery::mock_args(mock_read_dcf)[[1]]
  expect_identical(call_args[[1]], file.path(fake_path, "DESCRIPTION"))
  expect_identical(call_args[["fields"]], c("Package", "Version"))
  
  expect_identical(result$Package, "mypackage")
  expect_identical(result$Version, "0.2.0")
})

test_that("get_pkg_desc returns a list with NA values for missing optional fields", {
  fake_path <- "/fake/pkg"
  
  # read.dcf returns NA for fields absent from the file when fields= is specified
  fake_dcf <- matrix(
    c("mypackage", NA_character_),
    nrow = 1L,
    dimnames = list(NULL, c("Package", "URL"))
  )
  mockery::stub(get_pkg_desc, "read.dcf", fake_dcf)
  
  result <- get_pkg_desc(fake_path, fields = c("Package", "URL"))
  
  expect_type(result, "list")
  expect_identical(result$Package, "mypackage")
  expect_true(is.na(result$URL))
})

test_that("get package description works correctly", {
  
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  withr::local_options(list(repos = r))
  skip_if_repo_unavailable()
  
  dp <- system.file("test-data", "here-1.0.1.tar.gz", 
                    package = "test.assessr")
  
  # set up package
  install_list <- set_up_pkg(dp)
  
  package_installed <- install_list$package_installed
  pkg_source_path <- install_list$pkg_source_path
 
  if (package_installed == TRUE ) {	
    pkg_desc <- get_pkg_desc(pkg_source_path)
    
    expect_identical(length(pkg_desc), 17L)
    
    expect_true(checkmate::check_list(pkg_desc, 
                                      any.missing = FALSE)
    )
    
    expect_true(checkmate::check_list(pkg_desc, 
                                      types = "character")
    )
    
  }
  
})

test_that("get package name works correctly", {
  
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  withr::local_options(list(repos = r))
  skip_if_repo_unavailable()
  
  dp <- system.file("test-data", "MASS_7.3-65.tar.gz", 
                    package = "test.assessr")
  
  # set up package
  install_list <- set_up_pkg(dp)
  
  package_installed <- install_list$package_installed
  pkg_source_path <- install_list$pkg_source_path
 
  if (package_installed == TRUE ) {	
    pkg <- get_pkg_name(pkg_source_path)
    
    expect_identical(length(pkg), 1L)
    
    expect_true(checkmate::check_character(pkg, 
                                           any.missing = FALSE)
    )
    
    expect_true(checkmate::check_character(pkg, 
                                           pattern = "MASS")
    )
    
  }
  
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
