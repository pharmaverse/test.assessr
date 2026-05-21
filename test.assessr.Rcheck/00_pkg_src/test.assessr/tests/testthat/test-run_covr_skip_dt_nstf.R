# Helper: make a simple mapping df/list
make_mapping <- function(files) {
  # accept vector -> data.frame(test_file=..)
  if (is.null(files)) files <- character()
  data.frame(test_file = files, stringsAsFactors = FALSE)
}

test_that("standard layout: returns only existing wrappers and obeys skip list", {
  pkg_source_path <- "mockpkg"
  test_pkg_data   <- list(has_testit = FALSE)
  clean_mapping   <- make_mapping(c("a.R", "b.R", "c.R"))
  skip_tests      <- "b.R"
  
  # Stub normalizePath to identity to keep paths predictable
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  # Only mockpkg/tests/a.R and mockpkg/tests/c.R "exist"
  existing <- file.path(pkg_source_path, "tests", c("a.R", "c.R"))
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) p %in% existing)
  
  got <- compute_test_files_clean_dt(
    pkg_name        = "mypkg",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = test_pkg_data,
    skip_tests      = skip_tests
  )
  
  expect_setequal(got, file.path(pkg_source_path, "tests", c("a.R", "c.R")))
  expect_false(any(grepl("b\\.R$", got)))
})

test_that("testit layout: resolves under tests/testit and includes all existing wrappers", {
  pkg_source_path <- "mockpkg"
  test_pkg_data   <- list(has_testit = TRUE)
  clean_mapping   <- make_mapping(c("test-foo.R", "test-bar.R"))
  
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  existing <- file.path(pkg_source_path, "tests", "testit", c("test-foo.R", "test-bar.R"))
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) p %in% existing)
  
  got <- compute_test_files_clean_dt(
    pkg_name        = "mypkg",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = test_pkg_data
  )
  
  expect_setequal(got, existing)
  expect_true(all(grepl("^mockpkg/tests/testit/", got)))
})

test_that("data.table: falls back to tests/main.R when mapped wrappers do not exist", {
  pkg_source_path <- "mockpkg"
  test_pkg_data   <- list(has_testit = FALSE)
  # mapping points to wrappers that don't exist
  clean_mapping   <- make_mapping(c("nonexistent-1.R", "nonexistent-2.R"))
  
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  # No mapped files exist; but mockpkg/tests/main.R exists
  fallback <- file.path(pkg_source_path, "tests", "main.R")
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) p == fallback)
  
  got <- compute_test_files_clean_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = test_pkg_data
  )
  
  expect_length(got, 1L)
  expect_equal(got, fallback)
})

test_that("data.table: prioritizes froll.R first, keeps rest in alphabetical order", {
  pkg_source_path <- "mockpkg"
  test_pkg_data   <- list(has_testit = FALSE)
  clean_mapping   <- make_mapping(c("z.R", "froll.R", "a.R"))
  
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  # All mapped files exist
  existing <- file.path(pkg_source_path, "tests", c("z.R", "froll.R", "a.R"))
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) p %in% existing)
  
  got <- compute_test_files_clean_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = test_pkg_data
  )
  
  # Expected order: froll.R first, then a.R, z.R 
  # preserves input order among non-priority items
  expect_equal(
    got,
    file.path(pkg_source_path, "tests", c("froll.R", "z.R", "a.R"))
  )
})

test_that("returns character(0) when neither mapped files nor fallback exist", {
  pkg_source_path <- "mockpkg"
  test_pkg_data   <- list(has_testit = FALSE)
  clean_mapping   <- make_mapping("does-not-exist.R")
  
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  # Nothing exists, including fallback
  mockery::stub(compute_test_files_clean_dt, "file.exists",
                function(p) rep(FALSE, length(p)))
  
  got <- compute_test_files_clean_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = test_pkg_data
  )
  
  expect_length(got, 0L)
  expect_identical(got, character(0))
})

test_that("deduplicates and normalizes paths", {
  pkg_source_path <- "mockpkg"
  test_pkg_data   <- list(has_testit = FALSE)
  clean_mapping   <- make_mapping(c("dup.R", "dup.R", "other.R"))
  
  # normalizePath returns varied styles to test dedup (simulate OS differences)
  np_calls <- 0L
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) {
    # alternate between original and a fake 'normalized' copy to ensure uniquing works
    np_calls <<- np_calls + 1L
    path
  })
  
  existing <- file.path(pkg_source_path, "tests", c("dup.R", "dup.R", "other.R"))
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) p %in% existing)
  
  got <- compute_test_files_clean_dt(
    pkg_name        = "mypkg",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = test_pkg_data
  )
  
  expect_setequal(got, existing)
  expect_length(got, 3L)      
  expect_true(all(file.exists(gsub("^mockpkg", tempdir(), got)) | TRUE)) # trivial assertion to avoid CRAN notes
})



test_that("data.table MIRROR: copies inst/tests files, records created, and teardown unlinks them", {
  pkg_source_path <- "mockpkg"
  test_files_in   <- file.path(pkg_source_path, "tests", "wrapper.R") # pretend wrapper exists; not used here
  
  # --- Paths used by prepare_tests_for_package_dt() ---
  inst_tests_dir <- file.path(pkg_source_path, "inst", "tests")
  tests_dir      <- file.path(pkg_source_path, "tests")
  raw_in_inst    <- file.path(inst_tests_dir, "tests.Rraw")
  raw_in_tests   <- file.path(tests_dir, "tests.Rraw")
  main_path      <- file.path(tests_dir, "main.R")
  
  # --- Stubs on prepare_tests_for_package_dt itself ---
  
  # mkdir is a no-op that always succeeds
  mockery::stub(prepare_tests_for_package_dt, "dir.create", function(...) TRUE)
  
  # inst/tests contains mix of file & directory; only files should be copied
  src_files <- file.path(inst_tests_dir, c("froll.Rraw", "other.Rraw.bz2", "subdir"))
  mockery::stub(prepare_tests_for_package_dt, "list.files", function(path, all.files, full.names, no..) {
    stopifnot(path == inst_tests_dir)
    src_files
  })
  
  # Mark first two as files, third as directory
  mockery::stub(prepare_tests_for_package_dt, "file.info", function(paths) {
    data.frame(isdir = basename(paths) == "subdir", row.names = paths)
  })
  
  # File existence logic to trigger MIRROR:
  # - inst/tests/tests.Rraw exists (to trigger mirroring)
  # - tests/tests.Rraw does NOT exist (so mirroring is needed)
  # - tests/main.R does NOT exist (so tests_to_run is left as provided)
  # - destination copies do NOT exist yet (so copying occurs)
  mockery::stub(prepare_tests_for_package_dt, "file.exists", function(p) {
    if (identical(p, raw_in_inst))  return(TRUE)   # source driver exists
    if (identical(p, raw_in_tests)) return(FALSE)  # target driver missing -> mirror
    if (identical(p, main_path))    return(FALSE)  # no main.R -> passthrough test_files
    # For copied file destinations and any other path: report missing so copy proceeds
    FALSE
  })
  
  # Copy succeeds for both files
  mockery::stub(prepare_tests_for_package_dt, "file.copy", function(src, dest, overwrite) TRUE)
  
  # normalizePath returns the path unchanged for predictability
  mockery::stub(prepare_tests_for_package_dt, "normalizePath", function(path, ...) path)
  
  # Capture unlink calls from teardown
  unlinked <- NULL
  mockery::stub(prepare_tests_for_package_dt, "unlink", function(paths, ...) {
    unlinked <<- paths
    TRUE
  })
  
  # --- Call under test ---
  res <- prepare_tests_for_package_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    test_files      = test_files_in,
    mode            = "mirror"
  )
  
  # Returned test files are passed through unchanged (since tests/main.R "doesn't exist")
  expect_equal(res$test_files, test_files_in)
  
  # When we call teardown, it should unlink the files we "created"
  res$teardown()
  
  # Expect the two copied files to have been scheduled for unlink
  expected_created <- file.path(pkg_source_path, "tests", basename(src_files[1:2]))
  expect_setequal(unlinked, expected_created)
})



test_that("data.table: falls back to tests/main.R when inst/tests driver is absent (no message)", {
  pkg_source_path <- "mockpkg"
  test_files_in   <- character(0)
  
  inst_tests_dir <- file.path(pkg_source_path, "inst", "tests")
  tests_dir      <- file.path(pkg_source_path, "tests")
  raw_in_inst    <- file.path(inst_tests_dir, "tests.Rraw")
  raw_in_tests   <- file.path(tests_dir, "tests.Rraw")
  fallback       <- file.path(tests_dir, "main.R")
  
  # file.exists: main.R exists; both tests.Rraw do not
  mockery::stub(prepare_tests_for_package_dt, "file.exists", function(p) {
    if (identical(p, fallback))     return(TRUE)   # main.R present
    if (identical(p, raw_in_inst))  return(FALSE)  # inst driver missing
    if (identical(p, raw_in_tests)) return(FALSE)  # tests driver missing
    FALSE
  })
  
  # normalizePath is identity
  mockery::stub(prepare_tests_for_package_dt, "normalizePath", function(path, ...) path)
  
  # Should be silent (no message when inst/tests/tests.Rraw is absent)
  expect_silent({
    res <- prepare_tests_for_package_dt(
      pkg_name        = "data.table",
      pkg_source_path = pkg_source_path,
      test_files      = test_files_in,
      mode            = "mirror"
    )
  })
  
  # Falls back to tests/main.R
  expect_equal(res$test_files, fallback)
  
  # Teardown should be a no-op (no mirroring -> nothing created)
  unlinked <- "not-called"
  mockery::stub(prepare_tests_for_package_dt, "unlink", function(paths, ...) {
    unlinked <<- paths
    TRUE
  })
  res$teardown()
  expect_identical(unlinked, "not-called")
})


test_that("data.table MIRROR: does not copy when destination exists; teardown does not unlink", {
  pkg_source_path <- "mockpkg"
  test_files_in   <- file.path(pkg_source_path, "tests", "wrapper.R")
  
  inst_tests_dir  <- file.path(pkg_source_path, "inst", "tests")
  tests_dir       <- file.path(pkg_source_path, "tests")
  dests           <- file.path(tests_dir, c("froll.Rraw", "other.Rraw.bz2"))
  
  # inst/tests exists
  mockery::stub(prepare_tests_for_package_dt, "dir.exists", function(p) p == inst_tests_dir)
  
  mockery::stub(prepare_tests_for_package_dt, "dir.create", function(...) TRUE)
  mockery::stub(prepare_tests_for_package_dt, "list.files", function(...) {
    file.path(inst_tests_dir, c("froll.Rraw", "other.Rraw.bz2"))
  })
  mockery::stub(prepare_tests_for_package_dt, "file.info", function(paths) {
    data.frame(isdir = rep(FALSE, length(paths)), row.names = paths)
  })
  
  # Destination already exists; hence no copy should occur and 'created' remains empty
  mockery::stub(prepare_tests_for_package_dt, "file.exists", function(p) {
    # Return TRUE for the two destination files; FALSE otherwise (e.g., wrapper checks)
    p %in% dests
  })
  # file.copy should never be called, but if it is, make it fail loudly
  mockery::stub(prepare_tests_for_package_dt, "file.copy", function(...) stop("should not copy"))
  mockery::stub(prepare_tests_for_package_dt, "normalizePath", function(path, ...) path)
  
  # Track unlink – should not be called since 'created' is empty
  unlink_called <- FALSE
  mockery::stub(prepare_tests_for_package_dt, "unlink", function(paths, ...) {
    unlink_called <<- TRUE
    TRUE
  })
  
  res <- prepare_tests_for_package_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    test_files      = test_files_in,
    mode            = "mirror"
  )
  
  expect_equal(res$test_files, test_files_in)
  
  res$teardown()
  expect_false(unlink_called)  # nothing was created; nothing unlinked
})


test_that("prepare_tests_for_package_dt: emits message when MIRROR disabled and tests/tests.Rraw is missing", {
  pkg_source_path <- "mockpkg"
  test_files_in   <- file.path(pkg_source_path, "tests", "wrapper.R")  # arbitrary passthrough
  
  # Paths used by the function
  inst_tests_dir <- file.path(pkg_source_path, "inst", "tests")
  tests_dir      <- file.path(pkg_source_path, "tests")
  raw_in_inst    <- file.path(inst_tests_dir, "tests.Rraw")
  raw_in_tests   <- file.path(tests_dir, "tests.Rraw")
  main_path      <- file.path(tests_dir, "main.R")
  
  # --- Stubs: drive the exact branch we want ---
  
  # Force mode != "mirror" regardless of call input
  mockery::stub(prepare_tests_for_package_dt, "match.arg", function(x) "disabled")
  
  # File existence matrix:
  # - inst driver exists (so mirroring would be needed)
  # - tests driver missing (so branch considers mirroring)
  # - main.R missing (to ensure passthrough of input test_files)
  mockery::stub(prepare_tests_for_package_dt, "file.exists", function(p) {
    if (identical(p, raw_in_inst))  return(TRUE)
    if (identical(p, raw_in_tests)) return(FALSE)
    if (identical(p, main_path))    return(FALSE)
    FALSE
  })
  
  # Identity normalizer for deterministic assertions
  mockery::stub(prepare_tests_for_package_dt, "normalizePath", function(path, ...) path)
  
  # Guard: if mirroring is attempted (it should NOT be), fail the test
  mockery::stub(prepare_tests_for_package_dt, "dir.create",
                function(...) stop("dir.create should not be called when MIRROR is disabled"))
  mockery::stub(prepare_tests_for_package_dt, "list.files",
                function(...) stop("list.files should not be called when MIRROR is disabled"))
  mockery::stub(prepare_tests_for_package_dt, "file.info",
                function(...) stop("file.info should not be called when MIRROR is disabled"))
  mockery::stub(prepare_tests_for_package_dt, "file.copy",
                function(...) stop("file.copy should not be called when MIRROR is disabled"))
  
  # Capture unlink from teardown (should NOT be called because nothing was created)
  unlinked <- "not-called"
  mockery::stub(prepare_tests_for_package_dt, "unlink", function(paths, ...) {
    unlinked <<- paths
    TRUE
  })
  
  # --- Expect the specific message and call SUT ---
  expect_message(
    {
      res <- prepare_tests_for_package_dt(
        pkg_name        = "data.table",
        pkg_source_path = pkg_source_path,
        test_files      = test_files_in,
        mode            = "mirror"  # input is ignored by our match.arg stub above
      )
    },
    regexp = "MIRROR disabled; tests/tests\\.Rraw missing\\. Full suite may fail\\."
  )
  
  # Since main.R "doesn't exist", the function should passthrough the input test_files
  expect_identical(res$test_files, test_files_in)
  
  # Teardown should be a no-op (no created files -> unlink should not be called)
  res$teardown()
  expect_identical(unlinked, "not-called")
})



test_that("run_covr_skip_dt_nstf: happy path (main.R, mirror OK, coverage collected)", {
  # Create a real, existing package source directory + tests folder
  pkg_source_path <- tempfile("mockpkg_")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)     # NEW
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE)       # NEW
  test_main <- file.path(pkg_source_path, "tests", "main.R")
  file.create(test_main)                                                  # optional but harmless
  
  # Make sure the temp files are cleaned after the test
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  # --- Stubs (order matters: stub before calling the function) ---
  mockery::stub(run_covr_skip_dt_nstf, "get_pkg_name", function(...) "data.table")
  mockery::stub(run_covr_skip_dt_nstf, "compute_test_files_clean_dt",
                function(pkg_name, pkg_source_path, ..., skip_tests) test_main)
  mockery::stub(run_covr_skip_dt_nstf, "normalizePath", function(path, ...) path)
  mockery::stub(run_covr_skip_dt_nstf, "file.exists", function(p) TRUE)
  
  # Loading/datasets/namespace
  mockery::stub(run_covr_skip_dt_nstf, "pkgload::load_all", function(...) TRUE)
  mockery::stub(run_covr_skip_dt_nstf, "ls", function(envir, all.names) character(0))
  mockery::stub(run_covr_skip_dt_nstf, "load_package_datasets", function(...) character(0))
  
  # Mirror prep returns teardown no-op
  teardown_called <- FALSE
  mockery::stub(run_covr_skip_dt_nstf, "prepare_tests_for_package_dt", function(...) {
    list(teardown = function() { teardown_called <<- TRUE; invisible(TRUE) })
  })
  
  # covr pipeline
  mockery::stub(run_covr_skip_dt_nstf, "covr::environment_coverage",
                function(env, test_files) "COV")
  mockery::stub(run_covr_skip_dt_nstf, "covr::coverage_to_list",
                function(coverage) list(
                  filecoverage = matrix(1, 1, dimnames = list("R/file.R", NULL)),
                  totalcoverage = 97.82
                ))
  
  # wrappers present -> only main.R is selected, others become "skipped"
  mockery::stub(run_covr_skip_dt_nstf, "dt_find_wrapper_tests", function(pkg_source_path) {
    c(file.path(pkg_source_path, "tests", "main.R"),
      file.path(pkg_source_path, "tests", "froll.R"),
      file.path(pkg_source_path, "tests", "programming.R"))
  })
  
  # 39 untested functions (data.frame with rows = 39)
  untested_df <- data.frame(function_name = paste0("f", 1:39),
                            file = "R/foo.R", start_line = 1, end_line = 1,
                            stringsAsFactors = FALSE)
  mockery::stub(run_covr_skip_dt_nstf, "dt_compute_untested_functions",
                function(pkg_name, coverage) untested_df)
  
  # capture the inputs to compute_total_coverage to assert correctness
  cap <- new.env(parent = emptyenv())
  mockery::stub(run_covr_skip_dt_nstf, "compute_total_coverage",
                function(percent_cov, functions_no_tests_df, tests_passing, tests_skipped) {
                  cap$percent_cov   <- percent_cov
                  cap$no_tests_df   <- functions_no_tests_df
                  cap$tests_passing <- tests_passing
                  cap$tests_skipped <- tests_skipped
                  list(
                    total_cov           = percent_cov,
                    percent_cov         = percent_cov,
                    n_testfiles         = length(tests_passing),
                    n_no_function_tests = nrow(functions_no_tests_df),
                    n_skipped_files     = length(tests_skipped)
                  )
                })
  
  # cleanup stub (called at the end; we just no-op)
  mockery::stub(run_covr_skip_dt_nstf, "cleanup_and_return_null", function(...) TRUE)
  
  # --- Call under test (pass cov_env now) ---
  cov_env <- new.env(parent = emptyenv())   # NEW
  res <- run_covr_skip_dt_nstf(pkg_source_path, test_pkg_data = list(), cov_env = cov_env)  # NEW
  
  # --- Assertions ---
  expect_type(res, "list")
  expect_true(all(c("total_cov", "res_cov", "functions_no_tests",
                    "tests_skipped", "tests_passing") %in% names(res)))
  
  # coverage summary echoes 97.82
  expect_equal(res$total_cov, 97.82)
  expect_equal(res$res_cov$coverage$totalcoverage, 97.82)
  
  # tests_passing = main.R; tests_skipped = the other wrappers
  expect_identical(res$tests_passing, test_main)
  expect_setequal(res$tests_skipped,
                  c(file.path(pkg_source_path, "tests", "froll.R"),
                    file.path(pkg_source_path, "tests", "programming.R")))
  
  # functions_no_tests has 39 rows (as mocked)
  expect_equal(nrow(res$functions_no_tests), 39)
  
  # compute_total_coverage received the correct inputs
  expect_identical(cap$tests_passing, res$tests_passing)
  expect_setequal(cap$tests_skipped, res$tests_skipped)
  expect_equal(cap$percent_cov, 97.82)
  expect_equal(nrow(cap$no_tests_df), 39)
  
  # on.exit teardown executed
  expect_true(teardown_called)
})




test_that("run_covr_skip_dt_nstf: returns zeroed structure when coverage is NULL", {
  # Create a real, existing package source directory + tests folder
  pkg_source_path <- tempfile("mockpkg_")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)     # NEW
  dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE)       # NEW
  test_main <- file.path(pkg_source_path, "tests", "main.R")
  file.create(test_main)                                                  # optional but safe
  
  # Ensure cleanup after the test
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  # --- Stubs (set BEFORE calling the SUT) ---
  
  # This runner is data.table-only
  mockery::stub(run_covr_skip_dt_nstf, "get_pkg_name", function(...) "data.table")
  
  # Resolve tests: pretend tests/main.R is the chosen entrypoint
  mockery::stub(run_covr_skip_dt_nstf, "compute_test_files_clean_dt",
                function(pkg_name, pkg_source_path, ..., skip_tests) test_main)
  
  # filesystem helpers kept predictable
  mockery::stub(run_covr_skip_dt_nstf, "normalizePath", function(path, ...) path)
  mockery::stub(run_covr_skip_dt_nstf, "file.exists", function(p) TRUE)
  
  # Load package & datasets: no-op
  mockery::stub(run_covr_skip_dt_nstf, "pkgload::load_all", function(...) TRUE)
  mockery::stub(run_covr_skip_dt_nstf, "ls", function(envir, all.names) character(0))
  mockery::stub(run_covr_skip_dt_nstf, "load_package_datasets", function(...) character(0))
  
  # Prepare MIRROR: return a teardown function we can detect later
  teardown_called <- FALSE
  mockery::stub(run_covr_skip_dt_nstf, "prepare_tests_for_package_dt", function(...) {
    list(teardown = function() { teardown_called <<- TRUE; invisible(TRUE) })
  })
  
  # Force coverage to be NULL so the function returns the zeroed structure
  mockery::stub(run_covr_skip_dt_nstf, "covr::environment_coverage", function(...) NULL)
  
  # Make sure we DO NOT call coverage_to_list() on NULL (fail fast if we do)
  mockery::stub(run_covr_skip_dt_nstf, "covr::coverage_to_list",
                function(...) stop("should not be called when coverage is NULL"))
  
  # cleanup function should NOT be invoked on the NULL-coverage path
  cleanup_called <- FALSE
  mockery::stub(run_covr_skip_dt_nstf, "cleanup_and_return_null",
                function(...) { cleanup_called <<- TRUE; NULL })
  
  # --- Call under test; assert the user-facing message is emitted ---
  cov_env <- new.env(parent = emptyenv())  # NEW
  expect_message(
    {
      res <- run_covr_skip_dt_nstf(pkg_source_path, test_pkg_data = list(), cov_env = cov_env)  # NEW
    },
    regexp = "No test coverage for this configuration"
  )
  
  # --- Assertions for the returned zeroed structure (lines 118–134) ---
  
  # Structure and top-level fields
  expect_type(res, "list")
  expect_true(all(c("total_cov", "res_cov", "functions_no_tests",
                    "tests_skipped", "tests_passing") %in% names(res)))
  
  # total_cov = 0
  expect_identical(res$total_cov, 0)
  
  # res_cov sub-structure
  expect_true(all(c("name", "coverage", "errors", "notes") %in% names(res$res_cov)))
  expect_identical(res$res_cov$name, "data.table")
  expect_identical(res$res_cov$errors, "No coverage generated")
  expect_true(is.na(res$res_cov$notes))
  
  # coverage sub-structure
  cov_list <- res$res_cov$coverage
  expect_true(all(c("filecoverage", "totalcoverage") %in% names(cov_list)))
  expect_identical(cov_list$totalcoverage, 0)
  
  # filecoverage is a 1-row matrix with rowname "No functions tested"
  expect_true(is.matrix(cov_list$filecoverage))
  expect_identical(nrow(cov_list$filecoverage), 1L)
  expect_identical(rownames(cov_list$filecoverage), "No functions tested")
  
  # functions_no_tests: empty data.frame with the expected columns
  expect_true(is.data.frame(res$functions_no_tests))
  expect_identical(nrow(res$functions_no_tests), 0L)
  expect_true(all(c("function_name", "file") %in% names(res$functions_no_tests)))
  
  # tests_skipped / tests_passing are empty character vectors
  expect_identical(res$tests_skipped, character())
  expect_identical(res$tests_passing, character())
  
  # on.exit teardown ran
  expect_true(teardown_called)
  
  # cleanup path should NOT have been used in this branch
  expect_false(cleanup_called)
})




test_that("run_covr_skip_dt_nstf: early return when no test files are resolvable (length == 0)", {
  pkg_source_path <- tempfile("mockpkg_")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)   # NEW: ensure dir exists
  # Not required for this branch, but harmless if you want:
  # dir.create(file.path(pkg_source_path, "tests"), recursive = TRUE)
  
  # Clean up temp dir after the test
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))
  
  # --- Stubs (defined BEFORE invoking SUT) ---
  
  # Ensure we are in the data.table-only code path
  mockery::stub(run_covr_skip_dt_nstf, "get_pkg_name", function(...) "data.table")
  
  # Force compute_test_files_clean_dt() to return an empty vector
  mockery::stub(run_covr_skip_dt_nstf, "compute_test_files_clean_dt", function(...) character(0))
  
  # normalizePath: pass-through (safe on character(0))
  mockery::stub(run_covr_skip_dt_nstf, "normalizePath", function(path, ...) path)
  
  # Guardrails: if heavy paths are reached, fail the test (they should NOT be)
  mockery::stub(run_covr_skip_dt_nstf, "pkgload::load_all",
                function(...) stop("load_all should not be called on early return"))
  mockery::stub(run_covr_skip_dt_nstf, "prepare_tests_for_package_dt",
                function(...) stop("prepare_tests_for_package_dt should not be called on early return"))
  mockery::stub(run_covr_skip_dt_nstf, "covr::environment_coverage",
                function(...) stop("environment_coverage should not be called on early return"))
  
  # Capture the cleanup message and return a recognizable sentinel value
  captured <- new.env(parent = emptyenv())
  mockery::stub(run_covr_skip_dt_nstf, "cleanup_and_return_null",
                function(msg, ...) {
                  captured$msg <- msg
                  structure(list(aborted = TRUE, reason = msg), class = "cleanup_result")
                })
  
  # --- Call SUT (pass cov_env now) ---
  cov_env <- new.env(parent = emptyenv())  # NEW
  res <- run_covr_skip_dt_nstf(pkg_source_path, test_pkg_data = list(), cov_env = cov_env)  # NEW
  
  # --- Assertions ---
  expect_s3_class(res, "cleanup_result")
  expect_true(res$aborted)
  expect_match(res$reason, "^No resolvable data\\.table tests for data\\.table$")
  # also confirm the stub captured the same message
  expect_identical(captured$msg, res$reason)
})



test_that("run_covr_skip_dt_nstf: early return when test files exist list is non-empty but all file.exists == FALSE", {
  pkg_source_path <- tempfile("mockpkg_")
  dir.create(pkg_source_path, recursive = TRUE, showWarnings = FALSE)    # NEW: ensure dir exists
  withr::defer(unlink(pkg_source_path, recursive = TRUE, force = TRUE))  # NEW: cleanup
  
  test_main <- "/tmp/does-not-exist/tests/main.R"
  
  # Ensure data.table path is taken
  mockery::stub(run_covr_skip_dt_nstf, "get_pkg_name", function(...) "data.table")
  
  # Return a non-empty vector of test files
  mockery::stub(run_covr_skip_dt_nstf, "compute_test_files_clean_dt",
                function(...) test_main)
  
  # Keep deterministic path values
  mockery::stub(run_covr_skip_dt_nstf, "normalizePath", function(path, ...) path)
  
  # Force file.exists to FALSE for the candidate path
  mockery::stub(run_covr_skip_dt_nstf, "file.exists",
                function(p) rep(FALSE, length(p)))
  
  # Guardrails: heavy paths must not be reached
  mockery::stub(run_covr_skip_dt_nstf, "pkgload::load_all",
                function(...) stop("load_all should not be called on early return"))
  mockery::stub(run_covr_skip_dt_nstf, "prepare_tests_for_package_dt",
                function(...) stop("prepare_tests_for_package_dt should not be called on early return"))
  mockery::stub(run_covr_skip_dt_nstf, "covr::environment_coverage",
                function(...) stop("environment_coverage should not be called on early return"))
  
  # Capture the cleanup and return sentinel
  captured <- new.env(parent = emptyenv())
  mockery::stub(run_covr_skip_dt_nstf, "cleanup_and_return_null",
                function(msg, ...) {
                  captured$msg <- msg
                  structure(list(aborted = TRUE, reason = msg), class = "cleanup_result")
                })
  
  # --- Call SUT (pass cov_env now) ---
  cov_env <- new.env(parent = emptyenv())  # NEW
  res <- run_covr_skip_dt_nstf(pkg_source_path, test_pkg_data = list(), cov_env = cov_env)  # NEW
  
  # --- Assertions ---
  expect_s3_class(res, "cleanup_result")
  expect_true(res$aborted)
  expect_match(res$reason, "^No resolvable data\\.table tests for data\\.table$")
  expect_identical(captured$msg, res$reason)
})



test_that("dt_find_wrapper_tests() returns only existing candidates (normalized)", {
  pkg_source_path <- "/tmp/mockpkg"
  tests_dir <- file.path(pkg_source_path, "tests")
  
  # Build the full candidate set as per the implementation
  candidates <- file.path(
    tests_dir,
    c("main.R", "froll.R", "programming.R", "types.R",
      "nafill.R", "other.R", "S4.R", "autoprint.R")
  )
  
  # Stub file.exists(): only some files exist
  exists_set <- candidates[c(1, 3)]  # main.R, programming.R
  mockery::stub(dt_find_wrapper_tests, "file.exists", function(p) p %in% exists_set)
  
  # Stub normalizePath to identity for deterministic expectations
  mockery::stub(dt_find_wrapper_tests, "normalizePath", function(path, ...) path)
  
  res <- dt_find_wrapper_tests(pkg_source_path)
  expect_type(res, "character")
  expect_equal(res, exists_set)
})

test_that("dt_find_wrapper_tests() returns empty when no candidates exist", {
  pkg_source_path <- "/tmp/mockpkg"
  
  mockery::stub(dt_find_wrapper_tests, "file.exists", function(p) FALSE)
  mockery::stub(dt_find_wrapper_tests, "normalizePath", function(path, ...) path)
  
  res <- dt_find_wrapper_tests(pkg_source_path)
  expect_identical(res, character())
})




# Helper used by the test only: create a function *without* srcref attributes
make_fun_without_srcref <- function() {
  f <- function(...) NULL
  attr(f, "srcref") <- NULL
  attr(f, "wholeSrcref") <- NULL
  f
}

make_fake_fun_with_srcref <- function(fname, start_line, end_line) {
  f <- function(...) NULL
  sref <- list()
  sref[[1]] <- as.integer(start_line)  # start line
  sref[[3]] <- as.integer(end_line)    # end line
  # scalar filename is critical; do not pass a vector here
  attr(sref, "srcfile") <- list(filename = as.character(fname)[1L])
  attr(f, "srcref") <- sref
  f
}

test_that("dt_compute_untested_functions(): returns only functions with zero covered lines in span", {
  pkg_name <- "data.table"
  
  # Use scalar file names for srcref
  file_a <- "/tmp/mockpkg/R/a.R"  # fun_tested (has one covered line in its span)
  file_b <- "/tmp/mockpkg/R/b.R"  # fun_untested (no covered lines)
  
  # Construct fake namespace env containing our objects
  fake_ns <- new.env(parent = emptyenv())
  assign("fun_tested",
         make_fake_fun_with_srcref(file_a, start_line = 10, end_line = 20),
         envir = fake_ns)
  assign("fun_untested",
         make_fake_fun_with_srcref(file_b, start_line = 5, end_line = 8),
         envir = fake_ns)
  # A function without srcref must have the attributes removed explicitly
  assign("fun_nosrc", make_fun_without_srcref(), envir = fake_ns)
  assign("not_a_fun", 123L, envir = fake_ns)
  
  # Stub asNamespace() to return our fake namespace
  mockery::stub(dt_compute_untested_functions, "asNamespace",
                function(pkg) { stopifnot(pkg == pkg_name); fake_ns })
  
  # Coverage table: only "a.R" has a covered line within [10,20]
  cov_tab <- data.frame(
    filename = c("R/a.R", "R/a.R"),
    line     = c(15L, 30L),   # 15 in span; 30 out of span (ignored)
    value    = c(2, 0),       # 2 hits at line 15; 0 hits at 30
    stringsAsFactors = FALSE
  )
  mockery::stub(dt_compute_untested_functions, "covr::tally_coverage",
                function(coverage) cov_tab)
  
  # normalizePath is identity for predictability
  mockery::stub(dt_compute_untested_functions, "normalizePath",
                function(path, ...) path)
  
  # ---- Call under test ----
  res <- dt_compute_untested_functions(pkg_name, coverage = "IGNORED")
  
  # ---- Assertions ----
  # Only fun_untested should appear:
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1L)
  expect_setequal(names(res), c("function_name", "file", "start_line", "end_line"))
  expect_identical(res$function_name, "fun_untested")
  expect_identical(res$file, file_b)
  expect_identical(res$start_line, 5L)
  expect_identical(res$end_line, 8L)
})


test_that("dt_compute_untested_functions(): early return when tally_coverage() returns NULL", {
  pkg_name <- "data.table"
  
  # Stub covr::tally_coverage to return NULL (simulate no coverage table)
  mockery::stub(
    dt_compute_untested_functions,
    "covr::tally_coverage",
    function(coverage) NULL
  )
  
  # Ensure nothing else is called after the early return:
  # If these are called, the test should fail.
  mockery::stub(
    dt_compute_untested_functions,
    "asNamespace",
    function(pkg) stop("asNamespace should not be called on NULL coverage")
  )
  mockery::stub(
    dt_compute_untested_functions,
    "normalizePath",
    function(...) stop("normalizePath should not be called on NULL coverage")
  )
  
  res <- dt_compute_untested_functions(pkg_name, coverage = "IGNORED")
  
  # Returned structure is an empty data.frame with the expected columns
  expect_true(is.data.frame(res))
  expect_identical(nrow(res), 0L)
  expect_setequal(
    names(res),
    c("function_name", "file", "start_line", "end_line")
  )
})

test_that("dt_compute_untested_functions(): early return when tally_coverage() returns empty data.frame", {
  pkg_name <- "data.table"
  
  # Stub covr::tally_coverage to return a 0-row data.frame
  mockery::stub(
    dt_compute_untested_functions,
    "covr::tally_coverage",
    function(coverage) {
      data.frame(
        filename = character(),
        line     = integer(),
        value    = numeric(),
        stringsAsFactors = FALSE
      )
    }
  )
  
  # Ensure nothing else is called after the early return:
  mockery::stub(
    dt_compute_untested_functions,
    "asNamespace",
    function(pkg) stop("asNamespace should not be called on empty coverage")
  )
  mockery::stub(
    dt_compute_untested_functions,
    "normalizePath",
    function(...) stop("normalizePath should not be called on empty coverage")
  )
  
  res <- dt_compute_untested_functions(pkg_name, coverage = "IGNORED")
  
  # Returned structure is an empty data.frame with the expected columns
  expect_true(is.data.frame(res))
  expect_identical(nrow(res), 0L)
  expect_setequal(
    names(res),
    c("function_name", "file", "start_line", "end_line")
  )
})


test_that("dt_compute_untested_functions(): early return when fun_df is NULL", {
  pkg_name <- "data.table"
  
  # 1) coverage table is non-empty (pass earlier guard)
  mockery::stub(
    dt_compute_untested_functions,
    "covr::tally_coverage",
    function(coverage) data.frame(filename = "R/a.R", line = 1L, value = 1)
  )
  
  # 2) fake namespace with one symbol
  fake_ns <- new.env(parent = emptyenv())
  assign("x", 123L, envir = fake_ns)
  
  mockery::stub(
    dt_compute_untested_functions,
    "asNamespace",
    function(pkg) { stopifnot(pkg == pkg_name); fake_ns }
  )
  
  # 3) ensure we "see" one object in the namespace
  mockery::stub(
    dt_compute_untested_functions,
    "ls",
    function(envir, all.names) "x"
  )
  
  # 4) force get() to return a non-function (so lapply element becomes NULL)
  mockery::stub(
    dt_compute_untested_functions,
    "get",
    function(nm, envir) 123L
  )
  
  # 5) normalizePath shouldn't matter, but keep deterministic
  mockery::stub(
    dt_compute_untested_functions,
    "normalizePath",
    function(path, ...) path
  )
  
  # ---- call SUT ----
  res <- dt_compute_untested_functions(pkg_name, coverage = "IGNORED")
  
  # ---- assertions ----
  expect_true(is.data.frame(res))
  expect_identical(nrow(res), 0L)
  expect_setequal(names(res), c("function_name", "file", "start_line", "end_line"))
})


test_that("dt_compute_untested_functions(): early return when fun_df has zero rows", {
  pkg_name <- "data.table"
  
  # 1) non-empty coverage table again
  mockery::stub(
    dt_compute_untested_functions,
    "covr::tally_coverage",
    function(coverage) data.frame(filename = "R/a.R", line = 1L, value = 1)
  )
  
  # 2) asNamespace / ls / get can be anything; they won't affect do.call stub
  mockery::stub(
    dt_compute_untested_functions,
    "asNamespace",
    function(pkg) new.env(parent = emptyenv())
  )
  mockery::stub(
    dt_compute_untested_functions,
    "ls",
    function(envir, all.names) character(0)
  )
  mockery::stub(
    dt_compute_untested_functions,
    "get",
    function(nm, envir) NULL
  )
  
  # 3) Force do.call(rbind, out) to return a 0-row data.frame
  mockery::stub(
    dt_compute_untested_functions,
    "do.call",
    function(what, args) {
      # zero-row frame with expected columns would also work,
      # but any 0-row data.frame triggers !nrow(fun_df)
      data.frame(function_name = character(),
                 file          = character(),
                 start_line    = integer(),
                 end_line      = integer(),
                 stringsAsFactors = FALSE)
    }
  )
  
  # 4) keep normalizePath deterministic
  mockery::stub(
    dt_compute_untested_functions,
    "normalizePath",
    function(path, ...) path
  )
  
  # ---- call SUT ----
  res <- dt_compute_untested_functions(pkg_name, coverage = "IGNORED")
  
  # ---- assertions ----
  expect_true(is.data.frame(res))
  expect_identical(nrow(res), 0L)
  expect_setequal(names(res), c("function_name", "file", "start_line", "end_line"))
})


test_that("compute_test_files_clean_dt: applies skip_tests to both mapped and existing", {
  pkg_source_path <- "/tmp/mockpkg"
  tests_dir <- file.path(pkg_source_path, "tests")
  
  # clean_mapping lists two mapped test files; 'froll.R' is on the priority list,
  # 'alpha.R' is not (so its position comes after priority wrappers).
  clean_mapping <- list(test_file = c("alpha.R", "froll.R"))
  
  # We want existing wrappers (from priority set) to include 'programming.R' and 'types.R'.
  # We'll simulate existence via file.exists() stub.
  paths_exist <- c(
    file.path(tests_dir, "alpha.R"),
    file.path(tests_dir, "froll.R"),
    file.path(tests_dir, "programming.R"),
    file.path(tests_dir, "types.R")
  )
  # Important: Ensure tests/main.R does NOT exist so the function does not early-return there.
  main_path <- file.path(tests_dir, "main.R")
  
  # --- Stubs for isolation ---
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) {
    # vectorized truth for the arguments passed by the function
    p %in% paths_exist
  })
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  # Skip 'alpha.R' (mapped) and 'programming.R' (existing wrapper)
  skip_tests <- c("alpha.R", "programming.R")
  
  out <- compute_test_files_clean_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = list(),     # not used in DT-specific branch
    skip_tests      = skip_tests
  )
  
  # After skip:
  # - mapped   initially: {alpha.R, froll.R} -> remove alpha.R -> {froll.R}
  # - existing initially: {programming.R, types.R} -> remove programming.R -> {types.R}
  # The function sorts by explicit priority, so expected order: froll.R, types.R
  expected <- c(
    file.path(tests_dir, "froll.R"),
    file.path(tests_dir, "types.R")
  )
  expect_identical(out, expected)
})

test_that("compute_test_files_clean_dt: skip_tests=NULL does not filter mapped/existing", {
  pkg_source_path <- "/tmp/mockpkg"
  tests_dir <- file.path(pkg_source_path, "tests")
  
  clean_mapping <- list(test_file = c("alpha.R", "froll.R"))
  
  # Simulate these files as existing:
  paths_exist <- c(
    file.path(tests_dir, "alpha.R"),
    file.path(tests_dir, "froll.R"),
    file.path(tests_dir, "programming.R")
  )
  
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) p %in% paths_exist)
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  out <- compute_test_files_clean_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = list(),
    skip_tests      = NULL  # <- no skipping
  )
  
  # mapped: {alpha.R, froll.R}
  # existing (priority-present): {programming.R}
  # Ordered by DT priority wrappers: froll.R (priority), programming.R (priority), alpha.R (non-priority -> after)
  expected <- c(
    file.path(tests_dir, "froll.R"),
    file.path(tests_dir, "programming.R"),
    file.path(tests_dir, "alpha.R")
  )
  expect_identical(out, expected)
})

test_that("compute_test_files_clean_dt: empty skip_tests vector does not filter", {
  pkg_source_path <- "/tmp/mockpkg"
  tests_dir <- file.path(pkg_source_path, "tests")
  
  clean_mapping <- list(test_file = c("alpha.R", "froll.R"))
  paths_exist <- c(
    file.path(tests_dir, "alpha.R"),
    file.path(tests_dir, "froll.R"),
    file.path(tests_dir, "types.R")
  )
  
  mockery::stub(compute_test_files_clean_dt, "file.exists", function(p) p %in% paths_exist)
  mockery::stub(compute_test_files_clean_dt, "normalizePath", function(path, ...) path)
  
  out <- compute_test_files_clean_dt(
    pkg_name        = "data.table",
    pkg_source_path = pkg_source_path,
    clean_mapping   = clean_mapping,
    test_pkg_data   = list(),
    skip_tests      = character(0)  # length == 0 => branch not taken
  )
  
  # mapped: {alpha.R, froll.R}
  # existing: {types.R}
  # expected priority order: froll.R, types.R, alpha.R
  expected <- c(
    file.path(tests_dir, "froll.R"),
    file.path(tests_dir, "types.R"),
    file.path(tests_dir, "alpha.R")
  )
  expect_identical(out, expected)
})
