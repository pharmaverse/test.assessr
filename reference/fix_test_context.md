# Fix Missing Test Contexts

Ensures that each test result has a valid \`context\` field. If missing
or empty, the context is set to the file name (without extension).

## Usage

``` r
fix_test_context(test_results)
```

## Arguments

- test_results:

  A list of test result objects from \`testthat\`.

## Value

A list of test results with updated contexts.
