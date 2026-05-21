# Remove mapping rows that point to run_unitTests.R

Remove mapping rows that point to run_unitTests.R

Remove mapping rows that point to run_unitTests.R

## Usage

``` r
remove_run_unitTests_mapping(mapping, test_pkg_data)

remove_run_unitTests_mapping(mapping, test_pkg_data)
```

## Arguments

- mapping:

  A data.frame with columns \`source_file\` and \`test_file\` returned
  by get_source_test_mapping_nstf().

- test_pkg_data:

  A list or environment containing \`bioc_run_ut_path\`, e.g.,
  "/tmp/.../BiocGenerics/tests/run_unitTests.R".

## Value

The filtered mapping data.frame with run_unitTests.R rows removed.

The filtered mapping data.frame with run_unitTests.R rows removed.
