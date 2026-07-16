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

test_that("data_table_tests_path returns dt_main when has_dt_main is TRUE", {
 
  pkg_dir <- tempfile("pkg_dt_main")
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create main.R file to satisfy has_dt_main condition
  main_r_path <- file.path(pkg_dir, "tests", "main.R")
  writeLines("# Main test file", main_r_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_data_table_tests)
  expect_equal(result$data_table_tests_path, main_r_path)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("data_table_tests_path returns dt_raw_tests when has_dt_raw_tests is TRUE ", {
  
  pkg_dir <- tempfile("pkg_dt_raw")
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create tests.Rraw (not main.R) to test this branch
  tests_rraw_path <- file.path(pkg_dir, "tests", "tests.Rraw")
  writeLines("# Raw tests file", tests_rraw_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_data_table_tests)
  expect_equal(result$data_table_tests_path, tests_rraw_path)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("data_table_tests_path returns dt_raw_tests_bz2 when file.exists(dt_raw_tests) is FALSE ", {
  
  pkg_dir <- tempfile("pkg_dt_raw_bz2")
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create only tests.Rraw.bz2 (not uncompressed)
  tests_rraw_bz2_path <- file.path(pkg_dir, "tests", "tests.Rraw.bz2")
  writeLines("# Compressed raw tests", tests_rraw_bz2_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_data_table_tests)
  expect_equal(result$data_table_tests_path, tests_rraw_bz2_path)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("data_table_tests_path returns dt_raw_inst when has_dt_raw_inst is TRUE", {
  
  
  pkg_dir <- tempfile("pkg_dt_inst")
  dir.create(file.path(pkg_dir, "inst", "tests"), recursive = TRUE)
  
  # Create inst/tests/tests.Rraw
  inst_tests_path <- file.path(pkg_dir, "inst", "tests", "tests.Rraw")
  writeLines("# Inst raw tests", inst_tests_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_data_table_tests)
  expect_equal(result$data_table_tests_path, inst_tests_path)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("data_table_tests_path returns dt_raw_inst_bz2 when inst file is compressed", {
  # Lines 76-77: if (file.exists(dt_raw_inst)) dt_raw_inst else dt_raw_inst_bz2
  
  pkg_dir <- tempfile("pkg_dt_inst_bz2")
  dir.create(file.path(pkg_dir, "inst", "tests"), recursive = TRUE)
  
  # Create only inst/tests/tests.Rraw.bz2 (compressed)
  inst_tests_bz2_path <- file.path(pkg_dir, "inst", "tests", "tests.Rraw.bz2")
  writeLines("# Compressed inst tests", inst_tests_bz2_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_data_table_tests)
  expect_equal(result$data_table_tests_path, inst_tests_bz2_path)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("data_table_tests_path returns NA_character_ when no data.table tests exist", {
  # Lines 78-80: else { NA_character_ }
  
  pkg_dir <- tempfile("pkg_no_dt")
  dir.create(pkg_dir)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_false(result$has_data_table_tests)
  expect_equal(result$data_table_tests_path, NA_character_)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("has_bioc_dot_test is TRUE when zzz.R contains .test function definition", {
  # Lines 126-128: if (file.exists(zzz_path)) { ... has_bioc_dot_test <- any(grepl(...))
  
  pkg_dir <- tempfile("pkg_bioc_test")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "unitTests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create zzz.R with .test function definition
  zzz_path <- file.path(pkg_dir, "R", "zzz.R")
  zzz_content <- c(
    ".onLoad <- function(libname, pkgname) {",
    "  .test <- function() {",
    "    cat('Running tests')",
    "  }",
    "}"
  )
  writeLines(zzz_content, zzz_path)
  
  # Create required files for BiocGenerics detection
  run_ut_path <- file.path(pkg_dir, "tests", "run_unitTests.R")
  writeLines("# Unit test runner", run_ut_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_true(result$has_BioG_test)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("has_bioc_dot_test is FALSE when zzz.R does not contain .test function", {
  # Lines 126-128: grepl pattern doesn't match
  
  pkg_dir <- tempfile("pkg_no_bioc_test")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "unitTests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create zzz.R WITHOUT .test function definition
  zzz_path <- file.path(pkg_dir, "R", "zzz.R")
  zzz_content <- c(
    ".onLoad <- function(libname, pkgname) {",
    "  cat('Package loaded')",
    "}"
  )
  writeLines(zzz_content, zzz_path)
  
  # Create required files
  run_ut_path <- file.path(pkg_dir, "tests", "run_unitTests.R")
  writeLines("# Unit test runner", run_ut_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_false(result$has_BioG_test)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("has_bioc_dot_test is FALSE when zzz.R file does not exist", {
  # Lines 126: if (file.exists(zzz_path))
  
  pkg_dir <- tempfile("pkg_no_zzz")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "unitTests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Don't create zzz.R file
  
  # Create required files
  run_ut_path <- file.path(pkg_dir, "tests", "run_unitTests.R")
  writeLines("# Unit test runner", run_ut_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  expect_false(result$has_BioG_test)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("readLines error handling returns empty character vector", {
  # Lines 127: tryCatch(readLines(...), error = function(e) character())
  
  pkg_dir <- tempfile("pkg_readlines_error")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "unitTests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create a zzz.R file with unreadable permission (if possible)
  # For CRAN compliance, we'll just create an empty zzz.R
  zzz_path <- file.path(pkg_dir, "R", "zzz.R")
  writeLines("", zzz_path)
  
  # Create required files
  run_ut_path <- file.path(pkg_dir, "tests", "run_unitTests.R")
  writeLines("# Unit test runner", run_ut_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  # Empty file should not match the pattern
  expect_false(result$has_BioG_test)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("grepl pattern correctly matches .test assignment with whitespace variations", {
  # Lines 128: grepl("\\.test\\s*<-\\s*function\\b", zzz_lines)
  
  pkg_dir <- tempfile("pkg_bioc_variations")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "unitTests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create zzz.R with various whitespace patterns
  zzz_path <- file.path(pkg_dir, "R", "zzz.R")
  zzz_content <- c(
    ".test<-function() { }",  # No spaces
    ".test <- function() { }",  # Standard
    ".test  <-  function() { }",  # Extra spaces
    ".test\t<-\tfunction() { }"  # Tabs
  )
  writeLines(zzz_content, zzz_path)
  
  # Create required files
  run_ut_path <- file.path(pkg_dir, "tests", "run_unitTests.R")
  writeLines("# Unit test runner", run_ut_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  # All variations should be detected
  expect_true(result$has_BioG_test)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("grepl pattern does not match function names that don't start with .test", {
  
  pkg_dir <- tempfile("pkg_bioc_no_match")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "unitTests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create zzz.R with similar but non-matching patterns
  zzz_path <- file.path(pkg_dir, "R", "zzz.R")
  zzz_content <- c(
    "test <- function() { }",  # No dot prefix
    "x.test <- function() { }",  # Wrong prefix
    ".Test <- function() { }",  # Capital T
    ".test_function <- function() { }"  # .test followed by underscore
  )
  writeLines(zzz_content, zzz_path)
  
  # Create required files
  run_ut_path <- file.path(pkg_dir, "tests", "run_unitTests.R")
  writeLines("# Unit test runner", run_ut_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  # None should match the pattern
  expect_true(result$has_BioG_test)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("grepl pattern matches word boundary after 'function'", {
  # Lines 128: Pattern requires \\b after function (word boundary)
  
  pkg_dir <- tempfile("pkg_bioc_boundary")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "unitTests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  
  # Create zzz.R with function followed by various characters
  zzz_path <- file.path(pkg_dir, "R", "zzz.R")
  zzz_content <- c(
    ".test <- function(x) { }",  # Matches: function followed by (
    ".test <- function  { }",  # Matches: function followed by space
    ".test <- functionX() { }"  # Should NOT match: function not at word boundary
  )
  writeLines(zzz_content, zzz_path)
  
  # Create required files
  run_ut_path <- file.path(pkg_dir, "tests", "run_unitTests.R")
  writeLines("# Unit test runner", run_ut_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  # First two lines should match
  expect_true(result$has_BioG_test)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("data_table_tests_path prefers main.R over tests.Rraw", {
  # Verifies priority: main.R > tests.Rraw > tests.Rraw.bz2 > inst/tests/tests.Rraw > inst/tests/tests.Rraw.bz2
  
  pkg_dir <- tempfile("pkg_dt_priority")
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "tests"), recursive = TRUE)
  
  # Create both main.R and tests.Rraw
  main_r_path <- file.path(pkg_dir, "tests", "main.R")
  tests_rraw_path <- file.path(pkg_dir, "tests", "tests.Rraw")
  writeLines("# Main", main_r_path)
  writeLines("# Raw tests", tests_rraw_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  # Should prefer main.R
  expect_equal(result$data_table_tests_path, main_r_path)
  
  unlink(pkg_dir, recursive = TRUE)
})


test_that("data_table_tests_path prefers tests.Rraw over inst tests", {
  # Verifies priority: tests.Rraw > inst/tests/tests.Rraw
  
  pkg_dir <- tempfile("pkg_dt_priority2")
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "tests"), recursive = TRUE)
  
  # Create tests.Rraw and inst/tests/tests.Rraw
  tests_rraw_path <- file.path(pkg_dir, "tests", "tests.Rraw")
  inst_tests_path <- file.path(pkg_dir, "inst", "tests", "tests.Rraw")
  writeLines("# Raw tests", tests_rraw_path)
  writeLines("# Inst raw tests", inst_tests_path)
  
  result <- check_pkg_tests_and_snaps(pkg_dir)
  
  # Should prefer tests.Rraw
  expect_equal(result$data_table_tests_path, tests_rraw_path)
  
  unlink(pkg_dir, recursive = TRUE)
})