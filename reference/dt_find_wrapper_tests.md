# Locate data.table wrapper test scripts (internal)

Returns absolute, normalized paths to known wrapper test scripts under
`tests/` for a data.table source checkout. This is used to determine
which wrapper files exist locally so that callers can distinguish
between the canonical entrypoint `tests/main.R` and other wrapper
scripts (e.g., `froll.R`, `programming.R`, etc.).

## Usage

``` r
dt_find_wrapper_tests(pkg_source_path)
```

## Arguments

- pkg_source_path:

  character(1). Path to the package source root that contains the
  `tests/` directory.

## Value

character(). A vector of existing files among the known candidates, with
absolute paths normalized via `normalizePath(..., winslash = "/")`. The
vector is empty if none of the candidates exist.

## Notes

The candidate list is intentionally small and stable. If the upstream
repository adds additional wrappers, extend this function’s list.
