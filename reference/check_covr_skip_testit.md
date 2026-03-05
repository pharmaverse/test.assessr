# Run testit Tests Individually and Capture Issues

This internal NSTF utility runs \`testit\` test files one-by-one and
reports whether sourcing each file produces a warning or an error. The
function uses dynamic package-root detection by searching upward for a
\`DESCRIPTION\` file to ensure tests run in the correct working
directory.

## Usage

``` r
check_covr_skip_testit(pkg_name, mapping, test_path)
```

## Arguments

- pkg_name:

  Character string. The package name. (Not used internally, retained
  only for compatibility with other NSTF utilities.)

- mapping:

  A data frame containing a column \`test_file\` listing test files to
  be run from the \`test_path\` directory.

- test_path:

  Path to the directory containing the \`testit\` test files.

## Value

A base R \`data.frame\` with two columns:

\* \*\*test_file\*\* — The name of the testit file evaluated \*
\*\*issue_type\*\* — One of: \* \`"warning: \<message\>"\` — A warning
was raised while sourcing the file \* \`"error: \<message\>"\` — An
error occurred while sourcing the file \* \`"No tests skipped"\` —
Returned as a single-row result if no warnings or errors occurred in any
test file

The data frame summarizes which testit files fail to run cleanly,
allowing early detection of broken or skipped tests in NSTF workflows.

## Details

The function does not evaluate testthat-based tests and is intended only
for lightweight or legacy \`testit\` scripts.

## Output Meaning

The returned data frame indicates potential problems with testit files
before coverage or build steps occur. There is no side effect other than
evaluation of test files; all results are reported in the data frame.

## See also

Other nstf_utility: [`check_covr_skip_nstf()`](check_covr_skip_nstf.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
