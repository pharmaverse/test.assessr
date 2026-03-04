toy_assessment_results <- list(
  pkg_name = "here",
  pkg_ver = "1.0.1",
  date_time = "2026-01-07 15:48:31.338571",
  executor = "u1004798",
  sysname = "Linux",
  version = "#1 SMP PREEMPT_DYNAMIC Tue Sep  9 12:21:26 UTC 2025",
  release = "6.1.150-174.273.amzn2023.x86_64",
  machine = "x86_64",
  r_version = "4.4.1",
  total_cov = 0.987,
  res_cov = list(
    name = "here-1.0.1",
    coverage = list(
      filecoverage = structure(
        c(100, 100, 100, 95.8, 100, 97.5),
        dim = c(6L),
        dimnames = list(c("R/aaa.R", "R/dr_here.R", "R/here.R", "R/i_am.R", "R/utils.R", "R/zzz.R"))
      ),
      totalcoverage = 98.7
    ),
    errors = NA,
    notes = NA
  )
)


test_that("handle_null returns 'N/A' for NULL", {
  expect_equal(handle_null(NULL), "N/A")
})

test_that("handle_null returns original value for non-NULL", {
  expect_equal(handle_null(123), "123")
  expect_equal(handle_null("text"), "text")
})

test_that("convert_number_to_abbreviation handles millions", {
  expect_equal(convert_number_to_abbreviation(1500000), "1.5M")
  expect_equal(convert_number_to_abbreviation(-2500000), "-2.5M")
})

test_that("convert_number_to_abbreviation handles thousands", {
  expect_equal(convert_number_to_abbreviation(1200), "1.2K")
  expect_equal(convert_number_to_abbreviation(-9999), "-10K")
})

test_that("convert_number_to_abbreviation handles small numbers", {
  expect_equal(convert_number_to_abbreviation(999), "999")
  expect_equal(convert_number_to_abbreviation(0), "0")
})

test_that("convert_number_to_abbreviation handles NA and non-numeric", {
  expect_true(is.na(convert_number_to_abbreviation(NA)))
  expect_true(is.na(convert_number_to_abbreviation("text")))
})

test_that("safe_value returns 'N/A' for NULL", {
  expect_equal(safe_value(NULL), "N/A")
})

test_that("safe_value abbreviates numeric values", {
  expect_equal(safe_value(1000), "1K")
  expect_equal(safe_value(1000000), "1M")
})

test_that("safe_value returns character for small numbers", {
  expect_equal(safe_value(42), "42")
})

# Define the toy assessment_results object
toy_assessment_results_1_1 <- list(
  pkg_name = "test.package.0001",
  pkg_version = "1.0.0"
)

# Test the generate_test_pkg_summary function
test_that("generate_risk_summary works correctly", {
  result <- generate_test_pkg_summary(toy_assessment_results_1_1)
  
  expect_equal(result$Metric[1], "Package")
  expect_equal(result$Value[1], "test.package.0001")
  expect_equal(result$Metric[2], "Version")
  expect_equal(result$Value[2], "1.0.0")
})

# Define the toy assessment_results object
toy_assessment_results_1_2 <- list(
  check = 1L,
  covr = 1L,
  date_time = "2025-01-13 17:01:49",
  executor = "test.executor",
  sysname = "Linux",
  release = "5.4.0-42-generic",
  machine = "x86_64",
  check_list = list(res_check = list(rversion = "4.0.2")),
  test_framework_type = "standard testing framework"
)

# Define the expected output
expected_test_details <- list(
  Metric = c(
    'Test Coverage Score', 'Date Time', 'Executor', 
    'OS Name', 'OS Release', 'OS Machine', 
    'R version', 'Testing Framework'
  ),
  Value = c(
    toy_assessment_results_1_2$total_cov,
    toy_assessment_results_1_2$date_time,
    toy_assessment_results_1_2$executor,
    toy_assessment_results_1_2$sysname,
    toy_assessment_results_1_2$release,
    toy_assessment_results_1_2$machine,
    toy_assessment_results_1_2$rversion,
    toy_assessment_results_1_2$test_framework_type
  )
)

# Test the generate_test_details function
test_that("generate_test_details works correctly", {
  result <- generate_test_details(toy_assessment_results_1_2)
  
  expect_equal(result$Metric, expected_test_details$Metric)
})



test_that("generate_coverage_section calls create_file_coverage_df when errors have expected structure", {
  file_names <- c("pkg/R/file1.R", "pkg/R/file2.R")
  file_coverage <- matrix(c(85, 90), ncol = 1,
                          dimnames = list(file_names, NULL))
  
  errors <- list(
    message = "in callr subprocess.",
    srcref = NULL,
    status = 0,
    stdout = "",
    stderr = "",
    parent_trace = NULL,
    call = NULL,
    procsrcref = NULL,
    parent = NULL
  )
  
  notes <- c("Note1", "Note2")
  
  assessment_results <- list(
    res_cov = list(
      coverage = list(totalcoverage = mean(file_coverage), filecoverage = file_coverage),
      errors = errors,
      notes = notes
    )
  )
  
  mockery::stub(generate_coverage_section, "extract_short_path", function(x) basename(x))
  
  captured_args <- NULL
  mockery::stub(generate_coverage_section, "create_file_coverage_df", function(fn, fc, err, nt) {
    captured_args <<- list(file_names = fn, file_coverage = fc, errors = err, notes = nt)
    data.frame(Function = fn, Coverage = fc, Errors = paste0(err$message, "; ", err$status),
               Notes = nt, stringsAsFactors = FALSE, row.names = fn)
  })
  
  result <- generate_coverage_section(assessment_results, pkg_name = "mockpkg")
  
  expect_identical(rownames(result), c("file1.R", "file2.R"))
  expect_true(!is.null(captured_args))
  expect_identical(unname(captured_args$file_names), c("file1.R", "file2.R"))
  expect_equal(as.vector(captured_args$file_coverage), c(85, 90))
})

test_that("generate_coverage_section handles NA errors and notes", {
  file_names <- c("R/file1.R", "R/file2.R")
  file_coverage <- matrix(c(75, 82), ncol = 1,
                          dimnames = list(file_names, NULL))
  
  errors <- NA
  notes <- NA
  
  assessment_results <- list(
    res_cov = list(
      coverage = list(totalcoverage = mean(file_coverage), filecoverage = file_coverage),
      errors = errors,
      notes = notes
    )
  )
  
  mockery::stub(generate_coverage_section, "extract_short_path", function(x) basename(x))
  
  result <- generate_coverage_section(assessment_results, pkg_name = "mockpkg")
  
  expect_identical(result$Function, c("file1.R", "file2.R"))
  expect_equal(result$Coverage, c(75, 82))
  expect_true(all(!is.na(result$Errors)))
  expect_true(all(!is.na(result$Notes)))
})

test_that("generate_test_report errors when directory does not exist", {
  # Skip if rmarkdown not installed because the function checks it first
  testthat::skip_if_not_installed("rmarkdown")
  
  # Construct a path that does not exist
  output_dir <- file.path(tempdir(), paste0("mock_output_dir_", as.integer(Sys.time())))
  testthat::expect_false(fs::dir_exists(output_dir))
  
  # Expect a checkmate assertion error about non-existent directory
  testthat::expect_error(
    generate_test_report(toy_assessment_results, output_dir),
    regexp = "does not exist."
  )
})

test_that("generate_test_report creates an HTML report", {
  skip_on_cran()
  # Create a temporary directory for output
  tmp_dir <- withr::local_tempdir()
  
  # Ensure cleanup of all files in tmp_dir
  withr::defer(unlink(tmp_dir, recursive = TRUE), envir = parent.frame())
  
  # Force rendering in CRAN/win-builder
  withr::local_envvar(NOT_CRAN = "true")
  
  # Run the function
  generate_test_report(toy_assessment_results, tmp_dir)
  
  # Find any .html file in tmp_dir
  html_files <- list.files(tmp_dir, pattern = "\\.html$", full.names = TRUE)
  
  # Check that at least one HTML file exists and is non-empty
  expect_true(length(html_files) > 0, info = "No HTML report was generated")
  expect_true(file.info(html_files[1])$size > 0, info = "HTML report is empty")
  
})

test_that("generate_coverage_section handles unexpected errors structure (character)", {
  file_names <- c("src/a.R", "src/b.R")
  file_coverage <- matrix(c(50, 60), ncol = 1,
                          dimnames = list(file_names, NULL))
  
  errors <- "Some error occurred"
  notes <- c("n1", "n2")
  
  assessment_results <- list(
    res_cov = list(
      coverage = list(totalcoverage = mean(file_coverage), filecoverage = file_coverage),
      errors = errors,
      notes = notes
    )
  )
  
  mockery::stub(generate_coverage_section, "extract_short_path", function(x) basename(x))
  
  result <- generate_coverage_section(assessment_results, pkg_name = "mockpkg")
  
  expect_identical(result$Function, c("a.R", "b.R"))
  expect_equal(result$Coverage, c(50, 60))
  expect_identical(result$Errors, rep("Some error occurred", 2))
  expect_identical(result$Notes, notes)
})

test_that("generate_coverage_section returns NA-row when filecoverage is NULL", {
  assessment_results <- list(
    res_cov = list(
      coverage = list(totalcoverage = NA_real_, filecoverage = NULL),
      errors = list(message = "m", status = 1),
      notes = NA
    )
  )
  
  result <- generate_coverage_section(assessment_results, pkg_name = "mockpkg")
  
  expect_equal(nrow(result), 1)
  expect_true(all(is.na(result$Function)))
  expect_true(all(is.na(result$Coverage)))
  expect_true(all(is.na(result$Errors)))
  expect_true(all(is.na(result$Notes)))
})




test_that("create_file_coverage_df works correctly with toy dataset", {
  # Toy dataset
  file_names <- c("file1.R", "file2.R")
  file_coverage <- c(85.0, 90.0)
  notes <- c("Note1", "Note2")
  errors <- list(
    message = "in callr subprocess.",
    status = 0
  )
  
  # Expected output with matching row names
  expected_output <- data.frame(
    File = file_names,
    Coverage = file_coverage,
    Errors = rep("in callr subprocess.; 0", length(file_names)),
    Notes = notes,
    stringsAsFactors = FALSE,
    row.names = file_names
  )
  
  
  # Run the function
  result <- create_file_coverage_df(file_names, file_coverage, errors, notes)
  
  # Check if the result matches the expected output
  expect_equal(result, expected_output)
})


test_that("convert_vectors_to_df returns named list of data frames for existing elements", {
  # Mocked input list
  x <- list(
    tests_skipped  = c("glmmPQL.R", "loglm.R", "polr.R"),
    tests_passing  = c("/tmp/MASS/tests/fitdistr.R", "/tmp/MASS/tests/hubers.R"),
    functions_no_tests = c("add.R", "area.R")
  )
  
  # Call function
  res <- convert_vectors_to_df(x, c("tests_skipped", "tests_passing"))
  
  # Basic structure checks
  expect_type(res, "list")
  expect_named(res, c("tests_skipped", "tests_passing"))
  
  # Each element is a data.frame with column 'file'
  expect_s3_class(res$tests_skipped, "data.frame")
  expect_s3_class(res$tests_passing, "data.frame")
  expect_equal(names(res$tests_skipped), "file")
  expect_equal(names(res$tests_passing), "file")
  
  # Row content matches source vectors
  expect_equal(res$tests_skipped$file, x$tests_skipped)
  expect_equal(res$tests_passing$file, x$tests_passing)
})


test_that("convert_vectors_to_df filters out non-existent elements", {
  x <- list(
    tests_skipped = c("a.R", "b.R")
  )
  
  res <- convert_vectors_to_df(x, c("tests_skipped", "tests_passing", "functions_no_tests"))
  
  # Only existing element should be present
  expect_named(res, "tests_skipped")
  expect_equal(nrow(res$tests_skipped), 2)
  expect_equal(res$tests_skipped$file, x$tests_skipped)
})

test_that("convert_vectors_to_df returns empty list when no elems exist", {
  x <- list(
    tests_skipped = character(0)
  )
  res <- convert_vectors_to_df(x, c("tests_passing", "functions_no_tests"))
  
  expect_type(res, "list")
  expect_length(res, 0L)
  expect_equal(names(res), character(0))
})

test_that("convert_vectors_to_df handles empty vectors and NA values", {
  x <- list(
    tests_skipped = character(0),
    tests_passing = c(NA_character_, "foo.R", NA_character_)
  )
  
  res <- convert_vectors_to_df(x, c("tests_skipped", "tests_passing"))
  
  expect_named(res, c("tests_skipped", "tests_passing"))
  
  # Empty vector becomes 0-row data frame
  expect_s3_class(res$tests_skipped, "data.frame")
  expect_equal(nrow(res$tests_skipped), 0L)
  expect_equal(names(res$tests_skipped), "file")
  
  # NA values are kept as-is in the 'file' column
  expect_s3_class(res$tests_passing, "data.frame")
  expect_equal(nrow(res$tests_passing), length(x$tests_passing))
  expect_true(any(is.na(res$tests_passing$file)))
  expect_equal(res$tests_passing$file, x$tests_passing)
})

test_that("convert_vectors_to_df preserves element order as in 'elems' after filtering to existing names", {
  x <- list(
    a = c("a1.R", "a2.R"),
    b = c("b1.R"),
    c = c("c1.R", "c2.R", "c3.R")
  )
  
  res <- convert_vectors_to_df(x, c("c", "a", "missing", "b"))
  expect_named(res, c("c", "a", "b"))
  
  # Confirm each data frame content matches
  expect_equal(res$c$file, x$c)
  expect_equal(res$a$file, x$a)
  expect_equal(res$b$file, x$b)
})

test_that("convert_vectors_to_df works with non-character vectors by coercion (data.frame behavior)", {
  x <- list(
    tests_numeric = c(1, 2, 3),
    tests_logical = c(TRUE, FALSE)
  )
  
  res <- convert_vectors_to_df(x, c("tests_numeric", "tests_logical"))
  
  expect_named(res, c("tests_numeric", "tests_logical"))
  expect_s3_class(res$tests_numeric, "data.frame")
  expect_s3_class(res$tests_logical, "data.frame")
  
  # data.frame will coerce non-character to appropriate column types
  expect_true(is.numeric(res$tests_numeric$file))
  expect_true(is.logical(res$tests_logical$file))
  expect_equal(res$tests_numeric$file, x$tests_numeric)
  expect_equal(res$tests_logical$file, x$tests_logical)
})

test_that("convert_vectors_to_df handles elems = character(0)", {
  x <- list(
    tests_skipped = c("a.R")
  )
  res <- convert_vectors_to_df(x, character(0))
  
  expect_type(res, "list")
  expect_length(res, 0L)
  expect_equal(names(res), character(0))
})

test_that("convert_vectors_to_df handles list with additional names without affecting result", {
  x <- list(
    tests_skipped = c("a.R"),
    extra_matrix = matrix(1:4, nrow = 2),
    inner_list = list(foo = "bar")
  )
  
  res <- convert_vectors_to_df(x, c("tests_skipped", "inner_list"))
  
  # Only elements matching elems and existing in x are included
  expect_named(res, c("tests_skipped", "inner_list"))
  expect_s3_class(res$tests_skipped, "data.frame")
  expect_s3_class(res$inner_list, "data.frame")
  
  # For list element, data.frame(file = x[[nm]]) will create a list-column; ensure it exists
  expect_true("foo" %in% names(res$inner_list))
  expect_equal(nrow(res$inner_list), length(x$inner_list))
})


# Minimal res_cov structure that satisfies assert_list(... names="named")
make_res_cov <- function() {
  list(
    coverage = list(
      totalcoverage = 0.42,
      filecoverage  = data.frame(file = "R/foo.R", cov = 0.9, stringsAsFactors = FALSE)
    )
  )
}

make_test_results_stf <- function(pkg = "pkg", ver = "1.0.0") {
  list(
    pkg_name  = pkg,
    pkg_ver   = ver,
    date_time = Sys.time(),
    res_cov   = make_res_cov()
  )
}

make_test_results_nstf <- function(pkg = "pkg", ver = "1.0.0") {
  list(
    pkg_name  = pkg,
    pkg_ver   = ver,
    date_time = Sys.time(),
    res_cov   = make_res_cov(),
    # NSTF payloads
    functions_no_tests = data.frame(`function` = c("fn_a", "fn_b"), stringsAsFactors = FALSE),
    tests_skipped      = c("test-a", "test-b"),
    tests_passing      = c("path/to/test-1.R", "path/to/test-2.R")
  )
}



# A general stub for system.file that returns a temp path for the requested filename

stub_system_file <- function(...) {
  args <- list(...)
  
  # Extract the requested filename (2nd positional arg)
  filename <- if (length(args) >= 2) args[[2]] else "template.Rmd"
  
  # Create a real file in tempdir()
  tmp <- file.path(tempdir(), filename)
  writeLines(
    c(
      "---",
      "title: 'Stub Template'",
      "output: html_document",
      "---",
      "",
      "Dummy content"
    ),
    tmp
  )
  
  return(tmp)
}


# A simple path_abs stub that mimics fs::path_abs(path, start=output_dir)
stub_path_abs <- function(path, start = NULL) {
  if (!is.null(start)) {
    file.path(start, basename(path))
  } else {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
}


# Basic stubs for coverage and other section generators
stub_generate_test_pkg_summary    <- function(test_results) "pkg_summary_stub"
stub_generate_test_details        <- function(test_results) "details_stub"
stub_generate_coverage_section    <- function(test_results, pkg_name) "coverage_stub"

# ---- TESTS ----

test_that("returns NULL with message when rmarkdown is not available", {
  tr <- make_test_results_stf()
  
  # First requireNamespace call should return FALSE -> early exit
  mockery::stub(generate_test_report, "requireNamespace", function(pkg, quietly = TRUE) FALSE)
  
  # Output dir doesn't matter because we should exit first
  expect_message(
    res <- generate_test_report(tr, output_dir = tempdir()),
    "Package 'rmarkdown' is required but not installed\\."
  )
  expect_null(res)
})





test_that("standard testing framework (nstf_none) uses STF template and renders", {
  tr     <- make_test_results_stf()
  outdir <- tempdir()
  
  withr::local_envvar(NOT_CRAN = "true")
  
  # ---- CRITICAL: match how the function calls these symbols ----
  mockery::stub(generate_test_report, "requireNamespace", function(pkg, quietly = TRUE) TRUE)
  mockery::stub(generate_test_report, "dir_exists", function(d) TRUE)
  mockery::stub(generate_test_report, "path_abs", stub_path_abs)
  mockery::stub(generate_test_report, "system.file", stub_system_file)
  mockery::stub(generate_test_report, "interactive", function() FALSE)
  
  # NSTF none → results in STF template
  mockery::stub(
    generate_test_report, 
    "check_elements_simple",
    function(test_results, elements_to_check) list(present = character(0), missing = elements_to_check)
  )
  
  # Generators
  mockery::stub(generate_test_report, "generate_test_pkg_summary", stub_generate_test_pkg_summary)
  mockery::stub(generate_test_report, "generate_test_details",     stub_generate_test_details)
  mockery::stub(generate_test_report, "generate_coverage_section", stub_generate_coverage_section)
  
  # ---- CRITICAL: stub correct render symbol ----
  # generate_test_report calls rmarkdown::render, not render
  mock_render <- mockery::mock("ok")
  mockery::stub(generate_test_report, "rmarkdown::render", mock_render)
  
  # ---- Act ----
  res <- generate_test_report(tr, output_dir = outdir)
  
  # ---- Assert return file path ----
  
  expect_identical(
    normalizePath(dirname(res), winslash = "/", mustWork = FALSE),
    normalizePath(outdir, winslash = "/", mustWork = FALSE)
  )
  
  
  expect_identical(
    basename(res),
    paste0("test_report_", tr$pkg_name, "_", tr$pkg_ver, ".html")
  )
  
  
   expected_outfile <- file.path(outdir, paste0("test_report_", tr$pkg_name, "_", tr$pkg_ver, ".html"))
  
  # ---- Assert render() was called ----
  mockery::expect_called(mock_render, 1)
  
  args_list <- mockery::mock_args(mock_render)
  expect_equal(length(args_list), 1)
  
  args <- args_list[[1]]
  expect_true(is.list(args))
  
  # ---- Assert render() arguments ----
  expect_true(grepl("test_report_template_stf\\.Rmd$", as.character(args$input)))
  expect_true(is.environment(args$envir))
  expect_identical(args$quiet, TRUE)
})




test_that("non-standard testing framework (nstf present) uses NSTF template and processes data frames", {
  tr     <- make_test_results_nstf()
  outdir <- tempdir()
  
  withr::local_envvar(NOT_CRAN = "true")
  
  # --- CRITICAL: stubs must match unqualified names used inside generate_test_report ---
  mockery::stub(generate_test_report, "requireNamespace", function(pkg, quietly = TRUE) TRUE)
  mockery::stub(generate_test_report, "dir_exists", function(d) TRUE)
  mockery::stub(generate_test_report, "path_abs", stub_path_abs)
  mockery::stub(generate_test_report, "system.file", stub_system_file)
  mockery::stub(generate_test_report, "interactive", function() FALSE)
  
  # NSTF present (functions_no_tests, tests_skipped, tests_passing)
  mockery::stub(
    generate_test_report,
    "check_elements_simple",
    function(test_results, elements_to_check) list(
      present = c("functions_no_tests", "tests_skipped", "tests_passing"),
      missing = character(0)
    )
  )
  
  # Generators
  mockery::stub(generate_test_report, "generate_test_pkg_summary", stub_generate_test_pkg_summary)
  mockery::stub(generate_test_report, "generate_test_details",     stub_generate_test_details)
  mockery::stub(generate_test_report, "generate_coverage_section", stub_generate_coverage_section)
  
  # Convert vectors + shorten paths
  mockery::stub(
    generate_test_report,
    "convert_vectors_to_df",
    function(test_results, name)
      list(data.frame(
        file = c("long/path/to/file_one.R", "another/deep/path/file_two.R"),
        x = 1:2,
        stringsAsFactors = FALSE
      ))
  )
  mockery::stub(generate_test_report, "extract_short_path", function(x) basename(x))
  
  # --- CRITICAL: stub correct symbol for render ---
  mock_render <- mockery::mock("ok")
  mockery::stub(generate_test_report, "rmarkdown::render", mock_render)
  
  # --- Act ---
  res <- generate_test_report(tr, output_dir = outdir)
  
  # --- Assert return path ---
  expect_identical(
    normalizePath(dirname(res), winslash = "/", mustWork = FALSE),
    normalizePath(outdir, winslash = "/", mustWork = FALSE)
  )
  
  
  expect_identical(
    basename(res),
    paste0("test_report_", tr$pkg_name, "_", tr$pkg_ver, ".html")
  )
  
  expected_outfile <- file.path(outdir, paste0("test_report_", tr$pkg_name, "_", tr$pkg_ver, ".html"))
  
  
  # --- Assert render() was called ---
  mockery::expect_called(mock_render, 1)
  
  args_list <- mockery::mock_args(mock_render)
  expect_equal(length(args_list), 1)
  
  args <- args_list[[1]]
  expect_true(is.list(args))
  
  # --- Assert NSTF template was used ---
  expect_true(grepl("test_report_template_nstf\\.Rmd$", as.character(args$input)))
  
  # --- Assert render args ---
  expect_true(is.environment(args$envir))
  expect_identical(args$quiet, TRUE)
  
  # --- Assert NSTF environment values (data frames processed properly) ---
  env <- args$envir
  expect_true(is.data.frame(env$functions_no_tests))
  expect_true(is.data.frame(env$tests_skipped_df))
  expect_true(is.data.frame(env$tests_passing_df))
  
  expect_identical(env$tests_passing_df$file, c("file_one.R", "file_two.R"))
})

test_that("errors when output_dir is NULL", {
  tr <- make_test_results_stf()
  
  # Ensure we don't early-return due to missing rmarkdown
  withr::local_envvar(NOT_CRAN = "true")
  mockery::stub(generate_test_report, "requireNamespace", function(pkg, quietly = TRUE) TRUE)
  
  # Now, because output_dir must be a non-empty string, NULL should error
  expect_error(
    generate_test_report(tr, output_dir = NULL),
    regexp = "(non-empty.*string|Assertion on 'output_dir')",
    fixed  = FALSE
  )
})


test_that("errors when required columns are missing", {
  df_bad <- data.frame(a = 1)
  expect_error(create_long_summary_details(df_bad), "Input df must contain columns")
})

test_that("parses file without '#' and returns NA start_line", {
  df <- data.frame(
    file = "path/to/file.R",
    code_script = "fun",
    status = "PASS",
    stringsAsFactors = FALSE
  )
  
  res <- create_long_summary_details(df)
  expect_equal(nrow(res), 1)
  expect_equal(res$test, "path/to/file.R")
  expect_equal(res$`R.function`, "fun")
  expect_true(is.na(res$start_line))
})

test_that("extracts start_line from after '#' up to the first underscore", {
  df <- data.frame(
    file = "dir/file.R#123_abc",
    code_script = "do_stuff",
    status = "PASS",
    stringsAsFactors = FALSE
  )
  
  res <- create_long_summary_details(df)
  expect_equal(nrow(res), 1)
  expect_equal(res$test, "dir/file.R")
  expect_equal(res$start_line, "123")
  expect_equal(res$`R.function`, "do_stuff")
})

test_that("trailing '#' with empty after_hash yields NA start_line", {
  df <- data.frame(
    file = "dir/file.R#",
    code_script = "f",
    status = "PASS",
    stringsAsFactors = FALSE
  )
  
  res <- create_long_summary_details(df)
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$start_line))
})

test_that("filters only rows with status == PASS (case and whitespace tolerant)", {
  df <- data.frame(
    file = c("a.R", "b.R", "c.R", "d.R"),
    code_script = c("f1", "f2", "f3", "f4"),
    status = c(" pass ", "FAIL", "PASS", "pAsS"),
    stringsAsFactors = FALSE
  )
  
  res <- create_long_summary_details(df)
  expect_equal(nrow(res), 3)
  expect_setequal(res$`R.function`, c("f1", "f3", "f4"))
})

test_that("using mockery::stub to override sub allows testing parsing branches", {
  # Replacement inspects the pattern argument to decide what to return,
  # so we can simulate the three different sub() usages inside the function.
  replacement_sub <- function(pattern, replacement, x, ...) {
    if (identical(pattern, "#.*$")) return("patched_before")
    if (identical(pattern, ".*#")) return("patched_after")
    if (identical(pattern, "_.*$")) return("patched_start")
    return(NA_character_)
  }
  
  # Stub `sub` inside create_long_summary_details, then call the function
  mockery::stub(create_long_summary_details, "sub", replacement_sub)
  
  df <- data.frame(
    file = "anything",           # value doesn't matter because sub is stubbed
    code_script = "foo",
    status = "PASS",
    stringsAsFactors = FALSE
  )
  
  res <- create_long_summary_details(df)
  expect_equal(nrow(res), 1)
  expect_equal(res$test, "patched_before")
  expect_equal(res$status, "PASS")
})

test_that("stubbing grepl to FALSE simulates no '#' present and results in NA start_line", {
  # Force grepl to always say there is no '#'
  mockery::stub(create_long_summary_details, "grepl", function(pattern, x, ...) FALSE)
  
  df <- data.frame(
    file = "dir/file.R#1_abc",   # original string contains '#', but grepl is stubbed
    code_script = "foo",
    status = "PASS",
    stringsAsFactors = FALSE
  )
  
  res <- create_long_summary_details(df)
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$start_line))
})

test_that("errors when required columns are missing", {
  df_bad <- data.frame(a = 1)
  expect_error(
    create_test_skip_details(df_bad),
    "Input df must contain columns: file, code_script, status, expectation, line1"
  )
})

test_that("errors when only some required columns are present", {
  df_partial <- data.frame(
    file = "test.R",
    code_script = "func",
    status = "SKIP"
  )
  expect_error(
    create_test_skip_details(df_partial),
    "Input df must contain columns: file, code_script, status, expectation, line1"
  )
})

test_that("processes valid dataframe with all required columns", {
  df <- data.frame(
    file = "test-myscript.R",
    code_script = "my_func",
    status = "SKIP",
    expectation = "function should handle NA",
    line1 = "42",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(nrow(res), 1)
  expect_equal(res$`Rfunction`, "my_func")
  expect_equal(res$test, "test-myscript.R")
  expect_equal(res$status, "SKIP")
  expect_equal(res$expectation, "function should handle NA")
  expect_equal(res$start_line, "42")
})

test_that("extracts portion before '#' from file column", {
  df <- data.frame(
    file = "test-example.R#100_context",
    code_script = "example_func",
    status = "SKIP",
    expectation = "should skip this",
    line1 = "100",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(nrow(res), 1)
  expect_equal(res$test, "test-example.R")
})

test_that("handles file names without '#' separator", {
  df <- data.frame(
    file = "test-nomodifier.R",
    code_script = "func",
    status = "SKIP",
    expectation = "no hash present",
    line1 = "10",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(res$test, "test-nomodifier.R")
})

test_that("preserves digits in test file names", {
  df <- data.frame(
    file = "test-utils2.R#200",
    code_script = "util_func",
    status = "SKIP",
    expectation = "test expectation",
    line1 = "200",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(res$test, "test-utils2.R")
})

test_that("handles multiple rows correctly", {
  df <- data.frame(
    file = c("test-a.R#1", "test-b.R", "test-c.R#50_info"),
    code_script = c("func_a", "func_b", "func_c"),
    status = c("SKIP", "SKIP", "SKIP"),
    expectation = c("expect a", "expect b", "expect c"),
    line1 = c("1", "20", "50"),
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(nrow(res), 3)
  expect_equal(res$test[1], "test-a.R")
  expect_equal(res$test[2], "test-b.R")
  expect_equal(res$test[3], "test-c.R")
  expect_equal(res$`Rfunction`, c("func_a", "func_b", "func_c"))
})

test_that("converts input columns to character type", {
  df <- data.frame(
    file = "test. R",
    code_script = "func",
    status = "SKIP",
    expectation = "expect",
    line1 = 123,  # numeric input
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(res$start_line, "123")
  expect_true(is.character(res$start_line))
})

test_that("returns dataframe with correct column names and order", {
  df <- data.frame(
    file = "test.R",
    code_script = "func",
    status = "SKIP",
    expectation = "expect",
    line1 = "10",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expected_cols <- c("Rfunction", "test", "status", "expectation", "start_line")
  expect_equal(colnames(res), expected_cols)
})

test_that("removes row names (resets to default numeric indexing)", {
  df <- data.frame(
    file = c("test-a.R", "test-b.R"),
    code_script = c("f1", "f2"),
    status = c("SKIP", "SKIP"),
    expectation = c("e1", "e2"),
    line1 = c("1", "2"),
    row.names = c("custom_1", "custom_2"),
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(rownames(res), c("1", "2"))
})

test_that("using mockery::stub to override sub allows testing file parsing", {
  # Stub sub() to return predictable values based on pattern
  replacement_sub <- function(pattern, replacement, x, ...) {
    if (identical(pattern, "#.*$")) return("stubbed_file.R")
    return(x)
  }
  
  mockery::stub(create_test_skip_details, "sub", replacement_sub)
  
  df <- data.frame(
    file = "original_file.R#some_hash",
    code_script = "func",
    status = "SKIP",
    expectation = "expect",
    line1 = "42",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(res$test, "stubbed_file.R")
})

test_that("stubbing sub to identity preserves full file path including '#'", {
  # Stub sub() to act as identity (no replacement)
  replacement_sub <- function(pattern, replacement, x, .) {
    return(x)  # Always return x unchanged
  }
  
  mockery::stub(create_test_skip_details, "sub", replacement_sub)
  
  df <- data.frame(
    file = "test-preserve. R#100_suffix",
    code_script = "func",
    status = "SKIP",
    expectation = "expect",
    line1 = "100",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  # Since sub is stubbed to return x unchanged, test should be full file string
  expect_equal(res$test, "test-preserve. R#100_suffix")
})

test_that("handles empty/whitespace in expectation and status columns", {
  df <- data.frame(
    file = "test. R",
    code_script = "func",
    status = "  ",
    expectation = "",
    line1 = "5",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(nrow(res), 1)
  expect_equal(res$status, "  ")
  expect_equal(res$expectation, "")
})

test_that("handles special characters in expectation column", {
  df <- data.frame(
    file = "test.R",
    code_script = "func",
    status = "SKIP",
    expectation = "function should throw error:  'value < 0' is invalid; see #123",
    line1 = "10",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(
    res$expectation,
    "function should throw error:  'value < 0' is invalid; see #123"
  )
})

test_that("line1 column is directly assigned to start_line without modification", {
  df <- data.frame(
    file = "test.R",
    code_script = "func",
    status = "SKIP",
    expectation = "expect",
    line1 = "999",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(res$start_line, "999")
})

test_that("stringsAsFactors=FALSE is handled correctly for character conversion", {
  df <- data.frame(
    file = "test.R",
    code_script = "func",
    status = "SKIP",
    expectation = "expect",
    line1 = "10",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  # Check that all expected columns are character class
  expect_true(is.character(res$`Rfunction`))
  expect_true(is.character(res$test))
  expect_true(is.character(res$status))
  expect_true(is.character(res$expectation))
  expect_true(is.character(res$start_line))
})

test_that("handles very long file paths and special delimiters", {
  df <- data.frame(
    file = "path/to/deeply/nested/test-complex_name.R#line_456_with_underscores",
    code_script = "complex_function_name",
    status = "SKIP",
    expectation = "should handle complex paths",
    line1 = "456",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  expect_equal(res$test, "path/to/deeply/nested/test-complex_name.R")
  expect_equal(res$`Rfunction`, "complex_function_name")
})

test_that("stubbing as.character allows testing type coercion behavior", {
  # Mock as.character to prefix all values with "MOCK_"
  call_count <- 0
  
  replacement_as_char <- function(x) {
    call_count <<- call_count + 1
    paste0("MOCK_", x)
  }
  
  # Note: We can only stub one instance; here we stub the first as.character call
  # In practice, you might test the overall behavior instead
  mockery::stub(create_test_skip_details, "as.character", replacement_as_char)
  
  df <- data.frame(
    file = "test.R",
    code_script = "func",
    status = "SKIP",
    expectation = "expect",
    line1 = "10",
    stringsAsFactors = FALSE
  )
  
  res <- create_test_skip_details(df)
  
  # After stubbing, all character conversions should be prefixed
  expect_match(res$`Rfunction`, "^MOCK_")
})