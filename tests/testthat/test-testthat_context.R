# Tests for the internal helpers in R/testthat_context.R

# ---- get_dev_namespaces ------------------------------------------------------

test_that("get_dev_namespaces returns sorted namespaces flagged by pkgload", {
  mockery::stub(get_dev_namespaces, "loadedNamespaces", c("b", "a", "c"))
  mockery::stub(
    get_dev_namespaces,
    "pkgload::dev_meta",
    function(x) if (x %in% c("a", "b")) list(path = x) else NULL
  )
  
  expect_identical(get_dev_namespaces(), c("a", "b"))
})

test_that("get_dev_namespaces returns character(0) when none are flagged", {
  mockery::stub(get_dev_namespaces, "loadedNamespaces", c("a", "b"))
  mockery::stub(get_dev_namespaces, "pkgload::dev_meta", function(x) NULL)
  
  expect_identical(get_dev_namespaces(), character(0))
})

# ---- get_testthat_edition ----------------------------------------------------

test_that("get_testthat_edition reads Config/testthat/edition", {
  pkg_dir <- withr::local_tempdir()
  writeLines(
    c("Package: fx", "Version: 0.0.1", "Config/testthat/edition: 3"),
    file.path(pkg_dir, "DESCRIPTION")
  )
  
  expect_identical(get_testthat_edition(pkg_dir), 3L)
})

test_that("get_testthat_edition defaults to 2L when the field is absent", {
  pkg_dir <- withr::local_tempdir()
  writeLines(c("Package: fx", "Version: 0.0.1"), file.path(pkg_dir, "DESCRIPTION"))
  
  expect_identical(get_testthat_edition(pkg_dir), 2L)
})

test_that("get_testthat_edition defaults to 2L when DESCRIPTION is missing", {
  pkg_dir <- withr::local_tempdir()
  
  expect_identical(get_testthat_edition(pkg_dir), 2L)
})

test_that("get_testthat_edition defaults to 2L for a non-numeric edition", {
  pkg_dir <- withr::local_tempdir()
  writeLines(
    c("Package: fx", "Config/testthat/edition: abc"),
    file.path(pkg_dir, "DESCRIPTION")
  )
  
  expect_identical(get_testthat_edition(pkg_dir), 2L)
})

test_that("get_testthat_edition defaults to 2L when DESCRIPTION cannot be read", {
  pkg_dir <- withr::local_tempdir()
  writeLines("Package: fx", file.path(pkg_dir, "DESCRIPTION"))
  mockery::stub(get_testthat_edition, "read.dcf", function(...) stop("unreadable"))
  
  expect_identical(get_testthat_edition(pkg_dir), 2L)
})

test_that("get_testthat_edition validates its input", {
  expect_error(get_testthat_edition(NA_character_), "pkg_source_path")
  expect_error(get_testthat_edition(c("a", "b")), "pkg_source_path")
})

# ---- prepare_tests_dir -------------------------------------------------------

make_tests_tree <- function(root) {
  dir.create(file.path(root, "data"), recursive = TRUE)
  dir.create(file.path(root, "_snaps"), recursive = TRUE)
  saveRDS(1, file.path(root, "data", "val.rds"))
  writeLines("snap", file.path(root, "_snaps", "test-a.md"))
  writeLines("hx <- function() 1", file.path(root, "helper-a.R"))
  writeLines("setup_obj <- 1", file.path(root, "setup.R"))
  writeLines("test_that('a', expect_true(TRUE))", file.path(root, "test-a.R"))
  writeLines("test_that('b', expect_true(TRUE))", file.path(root, "test-b.r"))
  return(root)
}

test_that("prepare_tests_dir copies the whole tree including data and snaps", {
  src <- make_tests_tree(withr::local_tempdir())
  dest <- file.path(withr::local_tempdir(), "copy")
  
  out <- prepare_tests_dir(src, dest)
  
  expect_identical(out, dest)
  expect_true(file.exists(file.path(dest, "data", "val.rds")))
  expect_true(file.exists(file.path(dest, "_snaps", "test-a.md")))
  expect_true(all(file.exists(file.path(dest, c("helper-a.R", "setup.R", "test-a.R", "test-b.r")))))
  # The source tree is untouched
  expect_true(file.exists(file.path(src, "test-b.r")))
})

test_that("prepare_tests_dir keeps only the requested test files", {
  src <- make_tests_tree(withr::local_tempdir())
  dest <- file.path(withr::local_tempdir(), "copy")
  
  out <- prepare_tests_dir(src, dest, test_files = "test-a.R")
  
  expect_identical(out, dest)
  expect_true(file.exists(file.path(dest, "test-a.R")))
  expect_false(file.exists(file.path(dest, "test-b.r")))
  # Helper, setup and data files are kept
  expect_true(file.exists(file.path(dest, "helper-a.R")))
  expect_true(file.exists(file.path(dest, "setup.R")))
  expect_true(file.exists(file.path(dest, "data", "val.rds")))
  # The source tree still has both test files
  expect_true(file.exists(file.path(src, "test-b.r")))
})

test_that("prepare_tests_dir accepts full paths in test_files", {
  src <- make_tests_tree(withr::local_tempdir())
  dest <- file.path(withr::local_tempdir(), "copy")
  
  prepare_tests_dir(src, dest, test_files = file.path(src, "test-b.r"))
  
  expect_false(file.exists(file.path(dest, "test-a.R")))
  expect_true(file.exists(file.path(dest, "test-b.r")))
})

test_that("prepare_tests_dir copes with an empty source directory", {
  src <- withr::local_tempdir()
  dest <- file.path(withr::local_tempdir(), "copy")
  
  out <- prepare_tests_dir(src, dest)
  
  expect_identical(out, dest)
  expect_true(dir.exists(dest))
})

test_that("prepare_tests_dir returns NULL with a message when copying fails", {
  src <- make_tests_tree(withr::local_tempdir())
  dest <- file.path(withr::local_tempdir(), "copy")
  mockery::stub(prepare_tests_dir, "file.copy", function(...) c(TRUE, FALSE))
  
  expect_message(
    out <- prepare_tests_dir(src, dest),
    "Failed to copy some test files"
  )
  expect_null(out)
})

test_that("prepare_tests_dir validates its inputs", {
  src <- make_tests_tree(withr::local_tempdir())
  dest <- file.path(withr::local_tempdir(), "copy")
  
  expect_error(prepare_tests_dir(file.path(src, "missing"), dest), "src_dir")
  expect_error(prepare_tests_dir(src, NA_character_), "dest_dir")
  expect_error(prepare_tests_dir(src, dest, test_files = 1), "test_files")
})

# ---- run_environment_coverage ------------------------------------------------

# Build a fixture package: source file, helper, setup (with testthat-3e teardown),
# relative data file and three test files (pass, fail, pass-after-fail).
make_covr_fixture <- function(root, marker) {
  tests <- file.path(root, "tests", "testthat")
  dir.create(file.path(tests, "data"), recursive = TRUE)
  src_file <- file.path(root, "pkg_code.R")
  writeLines(
    c(
      "pkg_f <- function(x) { x + 1 }",
      "pkg_g <- function(x) { x * 2 }",
      "pkg_h <- function(x) { x - 1 }"
    ),
    src_file
  )
  writeLines(
    c("Package: fxpkg", "Version: 0.0.1", "Config/testthat/edition: 3"),
    file.path(root, "DESCRIPTION")
  )
  saveRDS(41, file.path(tests, "data", "val.rds"))
  writeLines("hx <- function() 1", file.path(tests, "helper-a.R"))
  writeLines(
    c(
      "scratch_file <- tempfile()",
      "writeLines('x', scratch_file)",
      "setup_obj <- 5",
      "withr::defer({",
      "  unlink(scratch_file)",
      "  writeLines('torn down', Sys.getenv('COVR_TEST_MARKER'))",
      "}, teardown_env())"
    ),
    file.path(tests, "setup.R")
  )
  writeLines(
    c(
      "test_that('helper, setup, data and package name are available', {",
      "  expect_equal(hx(), 1)",
      "  expect_equal(setup_obj, 5)",
      "  expect_equal(readRDS('data/val.rds'), 41)",
      "  expect_equal(pkg_f(1), 2)",
      "  expect_equal(Sys.getenv('TESTTHAT_PKG'), 'fxpkg')",
      "})"
    ),
    file.path(tests, "test-1.R")
  )
  writeLines(
    "test_that('deliberately failing', { expect_equal(pkg_g(2), 99) })",
    file.path(tests, "test-2.R")
  )
  writeLines(
    c(
      "test_that('runs after the failure, before teardown', {",
      "  expect_equal(pkg_h(3), 2)",
      "  expect_true(file.exists(scratch_file))",
      "  expect_false(file.exists(Sys.getenv('COVR_TEST_MARKER')))",
      "})"
    ),
    file.path(tests, "test-3.R")
  )
  env <- new.env(parent = globalenv())
  sys.source(src_file, envir = env, keep.source = TRUE)
  return(list(root = root, tests = tests, env = env, marker = marker))
}

skip_if_nested_covr_unsafe <- function() {
  testthat::skip_on_cran()
  # A real covr run would reset the counters of an outer covr measurement
  testthat::skip_if(
    identical(Sys.getenv("R_COVR"), "true"),
    "nested covr run inside a covr measurement"
  )
}

test_that("run_environment_coverage runs helpers, setup, teardown and tolerates failures", {
  skip_if_nested_covr_unsafe()
  marker <- file.path(withr::local_tempdir(), "marker.txt")
  withr::local_envvar(c(COVR_TEST_MARKER = marker, TESTTHAT_PKG = NA))
  fx <- make_covr_fixture(withr::local_tempdir(), marker)
  wd_before <- getwd()
  globals_before <- setdiff(ls(globalenv(), all.names = TRUE), ".Random.seed")
  
  cov <- run_environment_coverage("fxpkg", fx$env, fx$tests, fx$root)
  
  expect_s3_class(cov, "coverage")
  expect_identical(attr(cov, "n_failed"), 1L)
  expect_identical(attr(cov, "n_error"), 0L)
  expect_gt(covr::percent_coverage(cov), 0)
  # Teardown from setup.R ran once, after the last test file
  expect_true(file.exists(marker))
  # Temporary bindings are removed from the traced environment
  expect_false(any(c(".covr_cfg", ".covr_results") %in% ls(fx$env, all.names = TRUE)))
  # No leakage into the session
  expect_identical(getwd(), wd_before)
  expect_identical(Sys.getenv("TESTTHAT_PKG"), "")
  expect_identical(
    setdiff(ls(globalenv(), all.names = TRUE), ".Random.seed"),
    globals_before
  )
})

test_that("run_environment_coverage still returns coverage when every test fails", {
  skip_if_nested_covr_unsafe()
  marker <- file.path(withr::local_tempdir(), "marker.txt")
  withr::local_envvar(c(COVR_TEST_MARKER = marker))
  fx <- make_covr_fixture(withr::local_tempdir(), marker)
  # Keep only the failing test file
  unlink(file.path(fx$tests, c("test-1.R", "test-3.R")))
  
  cov <- run_environment_coverage("fxpkg", fx$env, fx$tests, fx$root)
  
  expect_s3_class(cov, "coverage")
  expect_identical(attr(cov, "n_failed"), 1L)
  expect_identical(attr(cov, "n_error"), 0L)
})

test_that("run_environment_coverage counts errors separately from failures", {
  skip_if_nested_covr_unsafe()
  marker <- file.path(withr::local_tempdir(), "marker.txt")
  withr::local_envvar(c(COVR_TEST_MARKER = marker))
  fx <- make_covr_fixture(withr::local_tempdir(), marker)
  unlink(file.path(fx$tests, c("test-1.R", "test-2.R", "test-3.R")))
  writeLines(
    "test_that('errors', { stop('boom'); expect_true(TRUE) })",
    file.path(fx$tests, "test-err.R")
  )
  
  cov <- run_environment_coverage("fxpkg", fx$env, fx$tests, fx$root)
  
  expect_identical(attr(cov, "n_failed"), 0L)
  expect_identical(attr(cov, "n_error"), 1L)
})

test_that("run_environment_coverage returns NULL without attributes when covr returns NULL", {
  env <- new.env()
  tests_dir <- withr::local_tempdir()
  mockery::stub(run_environment_coverage, "covr::environment_coverage", function(...) NULL)
  
  out <- run_environment_coverage("fxpkg", env, tests_dir, tests_dir)
  
  expect_null(out)
  expect_false(any(c(".covr_cfg", ".covr_results") %in% ls(env, all.names = TRUE)))
})

test_that("run_environment_coverage sets NA counts when no results were recorded", {
  env <- new.env()
  tests_dir <- withr::local_tempdir()
  mockery::stub(
    run_environment_coverage,
    "covr::environment_coverage",
    function(...) structure(list(), class = "coverage")
  )
  
  out <- run_environment_coverage("fxpkg", env, tests_dir, tests_dir)
  
  expect_s3_class(out, "coverage")
  expect_identical(attr(out, "n_failed"), NA_integer_)
  expect_identical(attr(out, "n_error"), NA_integer_)
})

test_that("run_environment_coverage passes parallel and edition settings to covr", {
  withr::local_envvar(c(TESTTHAT_PARALLEL = NA, TESTTHAT_EDITION = NA))
  env <- new.env()
  tests_dir <- withr::local_tempdir()
  writeLines(
    c("Package: fx", "Config/testthat/edition: 3"),
    file.path(tests_dir, "DESCRIPTION")
  )
  seen <- new.env()
  mockery::stub(
    run_environment_coverage,
    "covr::environment_coverage",
    function(env, test_files) {
      seen$parallel <- Sys.getenv("TESTTHAT_PARALLEL")
      seen$edition <- Sys.getenv("TESTTHAT_EDITION")
      seen$wrapper <- readLines(test_files)
      structure(list(), class = "coverage")
    }
  )
  
  run_environment_coverage("fxpkg", env, tests_dir, tests_dir)
  
  expect_identical(seen$parallel, "false")
  expect_identical(seen$edition, "3")
  expect_true(any(grepl("testthat::test_dir", seen$wrapper, fixed = TRUE)))
  expect_true(any(grepl("package = cfg$pkg", seen$wrapper, fixed = TRUE)))
  # Settings are restored afterwards
  expect_identical(Sys.getenv("TESTTHAT_EDITION"), "")
})

test_that("run_environment_coverage cleans up when covr errors", {
  env <- new.env()
  tests_dir <- withr::local_tempdir()
  wrapper_file <- NULL
  mockery::stub(
    run_environment_coverage,
    "covr::environment_coverage",
    function(env, test_files) {
      wrapper_file <<- test_files
      stop("boom")
    }
  )
  
  expect_error(run_environment_coverage("fxpkg", env, tests_dir, tests_dir), "boom")
  
  expect_false(file.exists(wrapper_file))
  expect_false(".covr_cfg" %in% ls(env, all.names = TRUE))
})

test_that("run_environment_coverage validates its inputs", {
  env <- new.env()
  tests_dir <- withr::local_tempdir()
  
  expect_error(run_environment_coverage("", env, tests_dir, tests_dir), "pkg")
  expect_error(run_environment_coverage("fxpkg", list(), tests_dir, tests_dir), "env")
  expect_error(
    run_environment_coverage("fxpkg", env, file.path(tests_dir, "missing"), tests_dir),
    "tests_dir"
  )
  expect_error(run_environment_coverage("fxpkg", env, tests_dir, NA_character_), "pkg_source_path")
})
