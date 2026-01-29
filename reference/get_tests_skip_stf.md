# Safely generate skip test mapping from problem tests and test map

This function filters and joins test metadata to identify which tests
should be skipped based on failed or problematic test results.

## Usage

``` r
get_tests_skip_stf(problem_tests, test_map)
```

## Arguments

- problem_tests:

  A data frame of problematic tests, including columns \`file\` and
  \`context\`.

- test_map:

  A data frame representing the test structure, including columns
  \`file\` and \`context\`.

## Value

A data frame containing the merged skip test mapping, or \`NULL\` if
validation or processing fails.
