# Prepare package-specific test environment for data.table (internal)

For data.table, Only "mirror" mode is supported here: copy inst/tests/\*
-\> tests/ and clean up.

## Usage

``` r
prepare_tests_for_package_dt(
  pkg_name,
  pkg_source_path,
  test_files,
  mode = c("mirror")
)
```

## Details

Returns a list with: - test_files: character() of wrapper files to
execute - teardown: function() to revert any changes (delete mirrored
files, unset env)
