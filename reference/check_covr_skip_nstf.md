# Check for Skipped or Errored Test Files (non-testthat)

This internal utility runs test files mapped to source files and
identifies any that produce warnings or errors when sourced. This helper
does \*\*not\*\* evaluate tests written using \`testthat\`; it is
intended for legacy or lightweight “non-testthat” test files.

## Usage

``` r
check_covr_skip_nstf(pkg_source_path, mapping)
```

## Arguments

- pkg_source_path:

  Character string. Path to the root directory of the package source.

- mapping:

  A data frame containing at least a column \`test_file\` identifying
  test file names found under \`tests/\`.

## Value

A data frame with the following columns:

\* \*\*test_file\*\* — The name of the test file that produced a
diagnostic \* \*\*issue_type\*\* — One of: \* \`"warning"\` — sourcing
the file triggered a warning \* \`"error"\` — sourcing the file threw an
error \* \`"No tests skipped"\` — returned when no problems are detected

The returned object is always a base R \`data.frame\`. If no warnings or
errors occur in any sourced test file, the function returns a single-row
data frame indicating \`"No tests skipped"\`.

## Details

The function sources each test file in a clean environment and captures
whether sourcing resulted in a warning or an error. Files that produce
no issues are omitted from the output.

## Output Meaning

The output is intended to help developers identify test files that fail
to run cleanly \*\*before\*\* test execution, ensuring they do not
silently skip or break coverage workflows.

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
