# Create an empty covr list for fallback

This internal utility function returns a default coverage list structure
used when test execution or mapping fails. It ensures consistent output
format for downstream processing even when no valid test results are
available.

## Usage

``` r
create_empty_covr_list(pkg_name, error_message, test_pkg_data = NULL)
```

## Arguments

- pkg_name:

  A character string representing the name of the package.

- error_message:

  A character string describing the reason for the fallback.

- test_pkg_data:

  Optional list from
  [`check_pkg_tests_and_snaps()`](check_pkg_tests_and_snaps.md); use
  `NULL` when unavailable (default).

## Value

A named list containing default coverage values, the provided error
message, and placeholder notes.
