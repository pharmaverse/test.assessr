# Ensure Test Results Have Valid Context Labels

This function normalizes the \`context\` field in testthat test results.
Some test result objects may have missing, empty, or \`NA\` context
values. This helper function assigns a meaningful context by using the
associated file name (excluding the \`.R\` extension) whenever the
original context is unavailable.

## Usage

``` r
fix_test_context(test_results)
```

## Arguments

- test_results:

  A list of test result objects produced by testthat execution functions
  (e.g., \`testthat::test_dir()\` or internal STF reporter helpers).
  Each element should contain a \`context\` field.

## Value

A list of test result objects where all entries have a non-empty
\`context\` value. If a test result originally lacked a valid context,
its context is replaced by the corresponding test file name. The
structure of each test result object is otherwise unchanged.

This output is used by downstream functions that rely on context labels
for grouping, mapping, or summarizing test outcomes.
