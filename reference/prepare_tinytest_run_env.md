# Enable environment-gated tinytest suites (internal)

Some packages gate their entire tinytest suite behind environment
variables, exiting each test file early via
[`tinytest::exit_file()`](https://rdrr.io/pkg/tinytest/man/exit_file.html)
unless a variable is set to a specific value. Rcpp is the canonical
example: every file under `inst/tinytest` begins with a guard such as
`if (Sys.getenv("RunAllRcppTests") != "yes") exit_file(...)`, so a plain
[`tinytest::run_test_dir()`](https://rdrr.io/pkg/tinytest/man/run_test_dir.html)
executes zero tests and reports 0% coverage.

## Usage

``` r
prepare_tinytest_run_env(tiny_dir, max_files = 40L, header_lines = 60L)
```

## Arguments

- tiny_dir:

  Character scalar. Path to the tinytest directory (typically
  `inst/tinytest`).

- max_files:

  Integer scalar. Maximum number of test files to scan. Defaults to
  `40L`.

- header_lines:

  Integer scalar. Number of lines read from the top of each test file
  when searching for gates. Defaults to `60L`.

## Value

A list with components:

- set_vars:

  Named character vector of environment variables that were set
  (variable name to required value); empty when no gates are found.

- teardown:

  A function that restores the previous environment state (unsetting
  variables that were previously unset).

## Details

This helper scans the headers of the discovered test files for gate
patterns of the form `Sys.getenv("VAR") != "VALUE"` and sets the
discovered variables to their required values so the tests run during
coverage. It is generic: any package using this idiom (not only Rcpp)
benefits without hard-coding package names. Packages without such gates
(e.g. digest) produce no matches, so nothing is changed and behaviour is
identical to before.

Both gate idioms are recognised: the primary “exit unless opted in” form
(`Sys.getenv("VAR") != "VALUE"`) and the complementary `== "VALUE"` form
used for secondary “verbose” suites (e.g. `RunVerboseRcppTests`). In
both cases the suite runs only when `VAR == VALUE`, so the variable is
set to that value. Verbose suites exercise packaging helpers in-process
(which covr can trace) but may also run `R CMD build` /
`install.packages` in child processes that can fail;
[`create_tinytest_coverage()`](create_tinytest_coverage.md) therefore
drives test files in isolation so one failure cannot stop the run.

The function is intentionally defensive and never throws: on any problem
it returns a no-op result so the surrounding tinytest coverage path is
unaffected. Callers should register the returned `teardown` via
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) to restore the
previous environment state.

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`prepare_tinytest_dev_includes()`](prepare_tinytest_dev_includes.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
