make_pls_like_pkg <- function(root) {
  dir.create(file.path(root, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "tests", "RUnit", "common"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "tests", "RUnit", "library"), recursive = TRUE, showWarnings = FALSE)
  
  writeLines(c(
    'Package: plslike',
    'Version: 0.1.0',
    'Title: pls-like fixture',
    'Description: Synthetic fixture for issue-26 tests.',
    'License: GPL-2'
  ), file.path(root, "DESCRIPTION"))
  
  writeLines('export(fit)', file.path(root, "NAMESPACE"))
  
  writeLines(c(
    'fit <- function(x) x'
  ), file.path(root, "R", "fit.R"))
  
  writeLines(c(
    'library(RUnit)',
    'testdirs <- c("RUnit/common", "RUnit/library")',
    'suite <- RUnit::defineTestSuite("plslike", testdirs)',
    'result <- RUnit::runTestSuite(suite)',
    'if (RUnit::getErrors(result)$nFail > 0 || RUnit::getErrors(result)$nErr > 0) stop("tests failed")'
  ), file.path(root, "tests", "test_library.R"))
  
  writeLines(c(
    'test.fit <- function() {',
    '  RUnit::checkEquals(fit(1), 1)',
    '}'
  ), file.path(root, "tests", "RUnit", "common", "runit.a.R"))
  
  writeLines(c(
    'test.fit_lib <- function() {',
    '  RUnit::checkEquals(fit(2), 2)',
    '}'
  ), file.path(root, "tests", "RUnit", "library", "runit.a.R"))
  
  writeLines('test_check("plslike")', file.path(root, "tests", "testthat.R"))
  
  writeLines(c(
    'test_that("fit works", {',
    '  expect_equal(fit(1), 1)',
    '})'
  ), file.path(root, "tests", "testthat", "test-x.R"))
  
  invisible(root)
}
