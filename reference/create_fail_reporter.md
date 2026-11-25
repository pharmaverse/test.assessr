# Create Fail Reporter

Runs all tests in a directory with \`testthat::FailReporter\` to detect
failing tests and report setup or helper errors.

## Usage

``` r
create_fail_reporter(test_path, pkg, cov_env = NULL)
```

## Arguments

- test_path:

  Character string specifying the path to the test directory.

- pkg:

  Character string specifying the package name.

- cov_env:

  Optional environment; not used as the test environment.

## Value

A list with \`failed\` (logical) and \`error\` (character or \`NULL\`).
