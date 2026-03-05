# Compute approximate total coverage combining line coverage and test breadth

Computes an approximate "total coverage" metric by combining: 1) line
coverage percentage (\`percent_cov\`, 0–100), and 2) a conservative
proxy for breadth of tests based on counts of passing test files,
functions without tests, and skipped test files.

The final metric scales line coverage (as a proportion) by the fraction
of tested over total function-space, where: - \`n_testfiles\` = number
of passing test files (if 0, treated as 1 to avoid divide-by-zero), -
\`n_no_function_tests\` = number of functions lacking tests
(\`nrow(functions_no_tests_df)\`), - \`n_skipped_files\` = number of
skipped test files (\`length(tests_skipped)\`), - \`total_functions\` =
\`n_tested + n_untested\` with \`n_tested = max(n_testfiles, 1)\`, -
\`total_cov\` = (percent_cov / 100) \* (n_tested / total_functions).

## Usage

``` r
compute_total_coverage(
  percent_cov,
  functions_no_tests_df,
  tests_passing,
  tests_skipped
)
```

## Arguments

- percent_cov:

  Numeric scalar. Line coverage percentage (0–100).

- functions_no_tests_df:

  A \`data.frame\` (or \`NULL\`) of functions lacking tests.

- tests_passing:

  A character vector (or \`NULL\`) of passing test file paths/names.

- tests_skipped:

  A character vector (or \`NULL\`) of skipped test file paths/names.

## Value

A named list with:

- `total_cov` (numeric): Approximate total coverage (proportion 0–1).

- `percent_cov_round` (numeric): `total_cov` rounded to 2 decimals
  (still 0–1).

- `n_testfiles` (integer): Count of passing test files.

- `n_no_function_tests` (integer): Count of functions lacking tests.

- `n_skipped_files` (integer): Count of skipped test files.

## Details

This function is intentionally conservative: - Passing test \*files\*
are used as a proxy for tested functions. - Skipped test files
contribute to "untested breadth". - If there are zero passing test
files, \`n_tested\` is set to 1 to avoid divide-by-zero and produce a
small non-zero denominator.
