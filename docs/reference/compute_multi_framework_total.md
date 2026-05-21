# Compute aggregate total coverage across multiple frameworks

Compute aggregate total coverage across multiple frameworks

## Usage

``` r
compute_multi_framework_total(per_framework)
```

## Arguments

- per_framework:

  Named list of covr results from each framework.

## Value

Numeric. Mean of per-framework total_cov (0-100), or 0 if none.
