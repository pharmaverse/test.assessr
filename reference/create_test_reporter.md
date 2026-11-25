# Create Test Reporter

Executes all tests in a given directory using \`testthat::test_dir()\`
and applies context fixes to ensure each test has a valid context.

## Usage

``` r
create_test_reporter(test_path, pkg, cov_env = NULL)
```

## Arguments

- test_path:

  Character string specifying the path to the test directory.

- pkg:

  Character string specifying the package name.

- cov_env:

  Optional environment; receives \`stf_error\` on failure. Not the test
  environment.

## Value

A list of test results with corrected contexts, or \`NULL\` if an error
occurs.
