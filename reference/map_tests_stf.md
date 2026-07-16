# Map the Structure of Testthat Tests (Standard Testing Framework)

This function analyses the test directory used by the Standard Testing
Framework (STF), typically \`tests/testthat/\`, and extracts a
structured representation of the unit tests contained in the directory.
It identifies test files, expectations, contexts, test names, and
associated line ranges.

## Usage

``` r
map_tests_stf(path = "tests/testthat")
```

## Arguments

- path:

  Character. Path to the test directory. Defaults to
  \`"tests/testthat"\`.

## Value

A consolidated \`data.frame\` where each row corresponds to an
expectation discovered within the STF test directory. The data frame
typically contains:

- file:

  Name of the test file in which the expectation appears.

- test:

  The combined description and test label assigned to the expectation.

- expectation:

  The expectation type detected (e.g., \`expect_equal\`).

- line1, line2:

  The starting and ending line numbers of the expectation.

- block_line1, block_line2:

  The starting and ending line numbers of the enclosing test block
  (e.g., the whole \`test_that()\` call). Used to skip an entire
  failing/erroring block, including non-expectation setup code.

This structured mapping is used by downstream STF processing functions
to support skip mapping, coverage enhancement, and reporting.

## Details

The function scans all testthat test files matching the pattern
\`test\*.R\`. For each file, it extracts structured metadata describing
the hierarchy of expectations, including:

- file name

- context or description

- test label

- expectation type

- starting and ending line numbers of the

- starting and ending line numbers of the enclosing test block

Files that contain no parseable test structure are omitted. If no valid
test files are found, the function returns \`NULL\`.

## See also

[`list.files`](https://rdrr.io/r/base/list.files.html),
[`lapply`](https://rdrr.io/r/base/lapply.html),
[`rbind`](https://rdrr.io/r/base/cbind.html)

Other stf_utility:
[`create_coverage_skip_stf()`](create_coverage_skip_stf.md),
[`map_test()`](map_test.md), [`nest_test()`](nest_test.md),
[`run_covr_skip_stf()`](run_covr_skip_stf.md)
