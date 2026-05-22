# Load Testthat Helper Files into Environment

This function searches for all files in the \`tests/testthat\` directory
that match the pattern \`helper-\*.R\` and sources them into the
provided environment. This is useful when running coverage with
\`covr::environment_coverage()\` to ensure helper functions are
available during test execution.

## Usage

``` r
load_test_helpers(env, helper_dir = "tests/testthat")
```

## Arguments

- env:

  An environment into which the helper files should be sourced.
  Typically this is the same environment used for coverage, e.g.,
  \`cov_env\`.

- helper_dir:

  Path to the directory containing helper files. Defaults to
  \`"tests/testthat"\`.

## Value

Invisibly returns \`TRUE\` if all files are sourced successfully.
