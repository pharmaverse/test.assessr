# Normalize test results for single or multi-framework structure

For single-framework results, passes through unchanged. For
multi-framework, sets `multi_framework` and `frameworks` while keeping
`results` as a named list of per-framework blocks (no flattening).

## Usage

``` r
normalize_test_results(test_results)
```

## Arguments

- test_results:

  List from `get_package_coverage` or `run_covr_modes`.

## Value

A list with `multi_framework` (logical), `frameworks` (character, multi
only), and `results` preserved for multi-framework.
