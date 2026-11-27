# Cleanr-style layout: launcher in tests/runit.R, test files in inst/runit_tests/
make_cleanr_like_pkg <- function(root) {
  dir.create(file.path(root, "R"),                   recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "inst", "runit_tests"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "tests"),               recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "tests", "testthat"),   recursive = TRUE, showWarnings = FALSE)
  
  writeLines(c(
    'Package: cleanrlike',
    'Version: 0.1.0',
    'Title: cleanr-like fixture',
    'Description: Synthetic fixture for launcher-based RUnit detection.',
    'License: MIT'
  ), file.path(root, "DESCRIPTION"))
  
  writeLines('export(greet)', file.path(root, "NAMESPACE"))
  writeLines('greet <- function(x) paste("hello", x)', file.path(root, "R", "greet.R"))
  
  # Launcher: uses defineTestSuite/runTestSuite pointing at inst/runit_tests
  writeLines(c(
    'unit_dir <- file.path("inst", "runit_tests")',
    'suite <- RUnit::defineTestSuite("cleanrlike", dirs = unit_dir)',
    'result <- RUnit::runTestSuite(suite)',
    'if (RUnit::getErrors(result)$nFail > 0) stop("RUnit failed.")'
  ), file.path(root, "tests", "runit.R"))
  
  # RUnit test file inside inst/runit_tests/
  writeLines(c(
    'test_greet <- function() {',
    '  RUnit::checkEquals(greet("world"), "hello world")',
    '}'
  ), file.path(root, "inst", "runit_tests", "runit.greet.R"))
  
  # testthat side
  writeLines('test_check("cleanrlike")', file.path(root, "tests", "testthat.R"))
  writeLines(c(
    'test_that("greet works", {',
    '  expect_equal(greet("x"), "hello x")',
    '})'
  ), file.path(root, "tests", "testthat", "test-greet.R"))
  
  invisible(root)
}

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
