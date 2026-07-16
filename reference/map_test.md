# Map the Structure of Expectations in a Testthat File

This function parses a single testthat test file and extracts a
structured representation of the expectations it contains. It identifies
top‑level test blocks (e.g., \`test_that()\`, \`describe()\`,
\`tar_test()\`) and any nested expectation blocks (e.g., \`it()\`). The
result maps expectation location, context, and source line ranges for
downstream analysis.

## Usage

``` r
map_test(path)
```

## Arguments

- path:

  Character. Path to a single test file (typically named \`test-\*.R\`).

## Value

A \`data.frame\` in which each row corresponds to an expectation
identified in the test file. Typical columns include:

- description:

  Context or test block label derived from the test file.

- expectation:

  The expectation type (e.g., \`expect_equal\`).

- line1, line2:

  Numeric start/end lines associated with the expectation.

- block_line1, block_line2:

  Numeric start/end lines of the enclosing test block (e.g., the whole
  \`test_that()\` call).

- test:

  Test label (combined description and nested block label, if
  applicable).

This structured output is used internally by STF test‑mapping functions
(e.g., \`map_tests_stf()\`) for coverage analysis, skip mapping, and
test reporting.

## Details

The function uses \`utils::getParseData()\` to inspect parse information
from the test file and constructs a hierarchical mapping of:

- test block context (e.g., \`describe()\`, \`test_that()\`)

- test labels or sub‑contexts (e.g., \`it()\` blocks)

- expectation type

- source line ranges associated with each expectation

If the file does not contain any parseable test blocks or expectations,
\`NULL\` is returned.

## See also

[`getParseData`](https://rdrr.io/r/utils/getParseData.html)

Other stf_utility:
[`create_coverage_skip_stf()`](create_coverage_skip_stf.md),
[`map_tests_stf()`](map_tests_stf.md), [`nest_test()`](nest_test.md),
[`run_covr_skip_stf()`](run_covr_skip_stf.md)
