# Create an empty covr list for fallback

This internal utility function returns a default coverage list structure
used when test execution or mapping fails. It ensures consistent output
format for downstream processing even when no valid test results are
available.

## Usage

``` r
create_empty_covr_list(pkg_name, error_message)
```

## Arguments

- pkg_name:

  A character string representing the name of the package.

- error_message:

  A character string describing the reason for the fallback.

## Value

A named list containing default coverage values, the provided error
message, and placeholder notes.
