# Check for skipped or errored test files (non-testit)

This internal function attempts to run test files mapped to source files
and identifies any that produce warnings or errors. It does not handle
\`testit\`.

## Usage

``` r
check_covr_skip_nstf(
  pkg_source_path,
  mapping,
  base_dir = file.path(pkg_source_path, "tests")
)
```

## Arguments

- pkg_source_path:

  Path to the root of the package source.

- mapping:

  A data frame with a column \`test_file\` listing test file names.

- base_dir:

  Directory containing the test files to probe. Defaults to
  \`\<pkg\>/tests\`, but may also be \`inst/tinytest\` or
  \`tests/testthat\` depending on package layout.

## Value

A data frame listing test files and any issues encountered.
