input_string <- "/home/user/R/x86_64-pc-linux-gnu-library/4.1/package.metric/test-data/here_0.1.0.tar.gz"

input_string <- "/home/user/R/x86_64-pc-linux-gnu-library/4.1/package.metric/test-data/here-0.1.0.tar.gz"


input_string <- "TxDb.Dmelanogaster.UCSC.dm3.ensGene_3.2.2.tar.gz"

input_string <- "here-1.0.1.tar.gz"

input_string <- "here_1.0.1.tar.gz"

test_that("finds package name in file path", {
  
  pkg_source_path <- "/home/user/R/x86_64-pc-linux-gnu-library/4.1/test.assessr/test-data/test.package.0001_0.1.0.tar.gz"
  
  pkg_disp <- test.assessr::get_pkg_name(pkg_source_path)
  
  testthat::expect_true(!is.na(pkg_disp))
  
  testthat::expect_identical(length(pkg_disp), 1L)
  
  testthat::expect_identical(pkg_disp, "test.package.0001")
})

test_that("finds package name in file path", {
  
  pkg_source_path <- "/home/user/R/x86_64-pc-linux-gnu-library/4.1/test.assessr/test-data/here_0.1.0.tar.gz"
  
  pkg_disp <- test.assessr::get_pkg_name(pkg_source_path)
  
  testthat::expect_true(!is.na(pkg_disp))
  
  testthat::expect_identical(length(pkg_disp), 1L)
  
  testthat::expect_identical(pkg_disp, "here")
})

test_that("finds package name in file path", {
  
  pkg_source_path <- "/home/user/R/x86_64-pc-linux-gnu-library/4.1/test.assessr/test-data/here-0.1.0.tar.gz"
  
  pkg_disp <- test.assessr::get_pkg_name(pkg_source_path)
  
  testthat::expect_true(!is.na(pkg_disp))
  
  testthat::expect_identical(length(pkg_disp), 1L)
  
  testthat::expect_identical(pkg_disp, "here")
})


test_that("finds package name in tar file string", {
  pkg <- "TxDb.Dmelanogaster.UCSC.dm3.ensGene_3.2.2.tar.gz"
  
  pkg_disp <- test.assessr::get_pkg_name(pkg)
  
  testthat::expect_true(!is.na(pkg_disp))
  
  testthat::expect_identical(length(pkg_disp), 1L)
  
  testthat::expect_identical(pkg_disp, "TxDb.Dmelanogaster.UCSC.dm3.ensGene")
  
})

test_that("finds package name in tar file string", {
  pkg <- "here_0.1.0.tar.gz"
  
  pkg_disp <- test.assessr::get_pkg_name(pkg)
  
  testthat::expect_true(!is.na(pkg_disp))
  
  testthat::expect_identical(length(pkg_disp), 1L)
  
  testthat::expect_identical(pkg_disp, "here")
  
})

test_that("finds package name in tar file string", {
  pkg <- "here-0.1.0.tar.gz"
  
  pkg_disp <- test.assessr::get_pkg_name(pkg)
  
  testthat::expect_true(!is.na(pkg_disp))
  
  testthat::expect_identical(length(pkg_disp), 1L)
  
  testthat::expect_identical(pkg_disp, "here")
  
})

test_that("handles Windows backslash paths", {
  sep <- intToUtf8(92)
  pkg <- paste(c("C:", "Users", "me", "Rtmp", "test.package.0016"), collapse = sep)
  
  testthat::expect_identical(get_pkg_name(pkg), "test.package.0016")
})

test_that("handles Windows tarball path with backslashes", {
  sep <- intToUtf8(92)
  pkg <- paste(c("C:", "data", "test.package.0016_0.1.0.tar.gz"), collapse = sep)
  
  testthat::expect_identical(get_pkg_name(pkg), "test.package.0016")
})

test_that("handles mixed separators and trailing separators", {
  sep <- intToUtf8(92)
  
  testthat::expect_identical(get_pkg_name(paste0("C:/a", sep, "b", sep, "pkg_1.0.tar.gz")), "pkg")
  testthat::expect_identical(get_pkg_name("/home/me/test.package.0016/"), "test.package.0016")
  testthat::expect_identical(get_pkg_name(paste0("C:", sep, "a", sep, "mypkg", sep)), "mypkg")
})

test_that("Linux and macOS style paths are unchanged", {
  testthat::expect_identical(get_pkg_name("/home/user/R/test.package.0001_0.1.0.tar.gz"), "test.package.0001")
  testthat::expect_identical(get_pkg_name("/Users/me/R/stringr-1.5.1.tar.gz"), "stringr")
  testthat::expect_identical(get_pkg_name("/tmp/RtmpAbc/test.package.0016"), "test.package.0016")
})

test_that("result is unnamed (stringr >= 1.6.0 regression guard)", {
  out <- get_pkg_name("data.table_1.18.6.1.tar.gz")
  expect_null(names(out))
  expect_identical(out, "data.table")
})

