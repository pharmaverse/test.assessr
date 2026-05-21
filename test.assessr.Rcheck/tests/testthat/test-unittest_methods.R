test_that("check_pkg_tests_and_snaps works as expected", {
  # Create a temp directory to act as our package
  pkg_dir <- tempfile("dummyPkg")
  dir.create(pkg_dir)
  
  # Create tests/testthat/_snaps structure
  testthat_dir <- file.path(pkg_dir, "tests", "testthat")
  snaps_dir <- file.path(testthat_dir, "_snaps")
  dir.create(snaps_dir, recursive = TRUE)
  
  # Create some dummy golden snapshot files
  file.create(file.path(snaps_dir, "snapshot1.md"))
  file.create(file.path(snaps_dir, "snapshot2.md"))
  
  # Call the function
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  # Check the results
  expect_true(result$has_testthat)
  expect_false(result$has_testit)
  expect_true(result$has_snaps)
  expect_equal(result$n_golden_tests, 2)
  
  # Clean up (optional)
  unlink(pkg_dir, recursive = TRUE)
})

test_that("works when both testthat and _snaps exist with files", {
  pkg_dir <- tempfile("dummyPkg1")
  dir.create(pkg_dir)
  
  testthat_dir <- file.path(pkg_dir, "tests", "testthat")
  snaps_dir <- file.path(testthat_dir, "_snaps")
  dir.create(snaps_dir, recursive = TRUE)
  
  file.create(file.path(snaps_dir, "snapshot1.md"))
  file.create(file.path(snaps_dir, "snapshot2.md"))
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_testthat)
  expect_false(result$has_testit)  
  expect_true(result$has_snaps)
  expect_equal(result$n_golden_tests, 2)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("works when both tinytest and _snaps exist with files", {
  pkg_dir <- tempfile("dummyPkg1")
  dir.create(pkg_dir)
  
  # tinytest (canonical): inst/tinytest with at least one test file
  tiny_dir <- file.path(pkg_dir, "inst", "tinytest")
  dir.create(tiny_dir, recursive = TRUE)
  writeLines("expect_true(TRUE)", file.path(tiny_dir, "test_basic.R"))
  
  # testthat snapshots live in tests/testthat/_snaps
  snaps_dir <- file.path(pkg_dir, "tests", "testthat", "_snaps")
  dir.create(snaps_dir, recursive = TRUE)
  file.create(file.path(snaps_dir, "snapshot1.md"))
  file.create(file.path(snaps_dir, "snapshot2.md"))
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_tinytest)
  expect_equal(result$n_golden_tests, 2)     # presence of .md alone shouldn't bump this
  expect_true(result$has_snaps)              # optional but consistent with the test title
  expect_gt(result$n_tinytest_files, 0)      # optional: verifies tinytest detection
  
  unlink(pkg_dir, recursive = TRUE)
})



test_that("works when neither testthat nor _snaps exist", {
  pkg_dir <- tempfile("dummyPkg3")
  dir.create(pkg_dir)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_false(result$has_testthat)
  expect_false(result$has_testit)  
  expect_false(result$has_snaps)
  expect_equal(result$n_golden_tests, 0)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("works when _snaps exists but is empty", {
  pkg_dir <- tempfile("dummyPkg4")
  dir.create(pkg_dir)
  
  testthat_dir <- file.path(pkg_dir, "tests", "testthat")
  snaps_dir <- file.path(testthat_dir, "_snaps")
  dir.create(snaps_dir, recursive = TRUE)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_testthat)
  expect_false(result$has_testit)  
  expect_true(result$has_snaps)
  expect_equal(result$n_golden_tests, 0)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("counts nested snapshot files", {
  pkg_dir <- tempfile("dummyPkg5")
  dir.create(pkg_dir)
  
  testthat_dir <- file.path(pkg_dir, "tests", "testthat")
  snaps_dir <- file.path(testthat_dir, "_snaps")
  nested_dir <- file.path(snaps_dir, "nested")
  dir.create(nested_dir, recursive = TRUE)
  
  file.create(file.path(snaps_dir, "snapshot1.md"))
  file.create(file.path(nested_dir, "snapshot2.md"))
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_testthat)
  expect_false(result$has_testit)  
  expect_true(result$has_snaps)
  expect_equal(result$n_golden_tests, 2)
  
  unlink(pkg_dir, recursive = TRUE)
})


# testit

test_that("detects testit framework presence", {
  pkg_dir <- tempfile("dummyPkg_testit")
  dir.create(pkg_dir)
  
  testit_dir <- file.path(pkg_dir, "tests", "testit")
  dir.create(testit_dir, recursive = TRUE)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_false(result$has_testthat)
  expect_true(result$has_testit)  
  unlink(pkg_dir, recursive = TRUE)
})



test_that("normal case: combines line coverage and breadth proxy correctly", {
  percent_cov <- 80  # 80% line coverage
  functions_no_tests_df <- data.frame(fn = c("a", "b", "c"))
  tests_passing <- c("test-a.R", "test-b.R")
  tests_skipped <- c("test-slow.R", "test-experimental.R")
  
  res <- compute_total_coverage(
    percent_cov = percent_cov,
    functions_no_tests_df = functions_no_tests_df,
    tests_passing = tests_passing,
    tests_skipped = tests_skipped
  )
  
  # Counts
  expect_equal(res$n_no_function_tests, 3L)
  expect_equal(res$n_skipped_files, 2L)
  expect_equal(res$n_testfiles, 2L)
  
  # Derived
  n_untested <- 3 + 2
  n_tested <- 2
  total_functions <- n_tested + n_untested
  expected_total_cov <- 80
  
  expect_equal(res$total_cov, expected_total_cov)
  expect_equal(res$percent_cov_round, round(expected_total_cov, 2))
})

test_that("NULL inputs handled: no rows, no lengths; n_tested falls back to 1", {
  res <- compute_total_coverage(
    percent_cov = 50,
    functions_no_tests_df = NULL,
    tests_passing = NULL,
    tests_skipped = NULL
  )
  
  expect_equal(res$n_no_function_tests, 0L)
  expect_equal(res$n_skipped_files, 0L)
  expect_equal(res$n_testfiles, 0L)
  
  # n_tested = 1 (fallback), total_functions = 1
  expected_total_cov <- 50
  expect_equal(res$total_cov, expected_total_cov)
  expect_equal(res$percent_cov_round, round(expected_total_cov, 2))
})

test_that("zero passing tests: fallback n_tested = 1 avoids divide-by-zero", {
  res <- compute_total_coverage(
    percent_cov = 75,
    functions_no_tests_df = data.frame(fn = character()),
    tests_passing = character(),  # empty
    tests_skipped = c("test-x.R")
  )
  
  expect_equal(res$n_testfiles, 0L)
  expect_equal(res$n_no_function_tests, 0L)
  expect_equal(res$n_skipped_files, 1L)
  
  n_untested <- 0 + 1
  n_tested <- 1L       # fallback
  total_functions <- n_tested + n_untested
  expected_total_cov <- 75
  
  expect_equal(res$total_cov, expected_total_cov)
})

test_that("percent_cov is clamped to [0, 100]", {
  # Above 100
  res_hi <- compute_total_coverage(
    percent_cov = 130,
    functions_no_tests_df = data.frame(fn = "a"),
    tests_passing = c("test-a.R", "test-b.R"),
    tests_skipped = NULL
  )
  
  n_untested <- 1
  n_tested <- 2
  total_functions <- n_tested + n_untested
  expected_total_cov_hi <- 100 
  expect_equal(res_hi$total_cov, expected_total_cov_hi)
  
  # Below 0
  res_lo <- compute_total_coverage(
    percent_cov = -20,
    functions_no_tests_df = NULL,
    tests_passing = c("t1.R"),
    tests_skipped = c("t_skip.R")
  )
  expect_equal(res_lo$total_cov, 0)
  expect_equal(res_lo$percent_cov_round, 0)
})

# --- Using mockery::stub to control internal dependencies ---

test_that("stub: simulate large number of functions_without_tests via nrow()", {
  fn <- compute_total_coverage
  mock_nrow <- function(x) 100L
  
  dummy_df <- data.frame(x = 1)
  
  mockery::stub(fn, "nrow", mock_nrow)
  
  res <- fn(
    percent_cov = 60,
    functions_no_tests_df = dummy_df,
    tests_passing = c("test-1.R", "test-2.R"),
    tests_skipped = character()
  )
  
  expect_equal(res$n_no_function_tests, 100L)
  
  n_untested <- 100
  n_tested <- 2
  total_functions <- n_tested + n_untested
  expected_total_cov <- 60 
  expect_equal(res$total_cov, expected_total_cov)
})

test_that("stub: simulate many skipped and passing files via length()", {
  fn <- compute_total_coverage
  
  mock_length <- function(x) {
    if (identical(x, "PASSING")) return(10L)
    if (identical(x, "SKIPPED")) return(7L)
    return(base::length(x))
  }
  
  mockery::stub(fn, "length", mock_length)
  
  res <- fn(
    percent_cov = 90,
    functions_no_tests_df = NULL,
    tests_passing = "PASSING",
    tests_skipped = "SKIPPED"
  )
  
  expect_equal(res$n_testfiles, 10L)
  expect_equal(res$n_skipped_files, 7L)
  n_untested <- 0 + 7
  n_tested <- 10
  total_functions <- n_tested + n_untested
  expected_total_cov <- 90
  expect_equal(res$total_cov, expected_total_cov)
})

test_that("stub: when tests_passing would be 0, fallback n_tested=1 is used", {
  fn <- compute_total_coverage
  
  mock_length_zero <- function(x) {
    if (identical(x, "PASSING")) return(0L)
    if (identical(x, "SKIPPED")) return(5L)
    return(base::length(x))
  }
  
  mockery::stub(fn, "length", mock_length_zero)
  
  res <- fn(
    percent_cov = 40,
    functions_no_tests_df = NULL,
    tests_passing = "PASSING",
    tests_skipped = "SKIPPED"
  )
  
  expect_equal(res$n_testfiles, 0L)
  expect_equal(res$n_skipped_files, 5L)
  
  n_untested <- 5
  n_tested <- 1L
  total_functions <- n_tested + n_untested
  expected_total_cov <- 40
  expect_equal(res$total_cov, expected_total_cov)
})

test_that("stub: rounding behavior can be validated separately", {
  fn <- compute_total_coverage
  
  mock_nrow <- function(x) 3L
  mock_length <- function(x) {
    if (identical(x, "PASSING")) return(2L)
    if (identical(x, "SKIPPED")) return(1L)
    return(base::length(x))
  }
  mockery::stub(fn, "nrow", mock_nrow)
  mockery::stub(fn, "length", mock_length)
  
  res <- fn(
    percent_cov = 83.3333,
    functions_no_tests_df = data.frame(x = 1),
    tests_passing = "PASSING",
    tests_skipped = "SKIPPED"
  )
  
  expected <- 83.3333 
  expect_equal(res$total_cov, expected)
  expect_equal(res$percent_cov_round, round(expected, 2))  # 0.28  expect_equal(res$percent_cov_round, round(expected, 2))  # 0.28
})  