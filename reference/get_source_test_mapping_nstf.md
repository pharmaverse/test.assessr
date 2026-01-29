# Map R Source Files to Corresponding Test Files

This internal helper function scans the \`R/\` and \`tests/\`
directories of a package and attempts to match each source file with a
corresponding test file based on filename similarity.

## Usage

``` r
get_source_test_mapping_nstf(pkg_source_path, test_dir)
```

## Arguments

- pkg_source_path:

  Character string. Path to the root of the package source directory.

- test_dir:

  character string. directory where tests are

## Value

A data frame with two columns:

- source_file:

  Name of the source file in the \`R/\` directory.

- test_file:

  Name of the matched test file in the \`tests/\` directory, or `NA` if
  no match is found.
