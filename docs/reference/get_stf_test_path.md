# Determine the appropriate standard test path for a package

This internal utility function inspects the \`test_pkg_data\` structure
to identify which testing framework is used in the package and returns
the corresponding test path.

## Usage

``` r
get_stf_test_path(test_pkg_data, testdir)
```

## Arguments

- test_pkg_data:

  A named list containing logical flags such as \`has_testthat\`,
  \`has_testit\`, \`has_tests_base\`, etc.

- testdir:

  A character string specifying the root directory where test folders
  are located.

## Value

A character string representing the path to the test directory, or
\`NULL\` if no known test framework is found.
