# Generate Coverage Sections (multi-framework)

Returns a named list of coverage data frames, one per framework.

## Usage

``` r
generate_coverage_sections(test_results, pkg_name)
```

## Arguments

- test_results:

  Multi-framework list with `results` (named list of per-framework
  blocks, each with `res_cov`).

- pkg_name:

  \- name of the package

## Value

Named list of data frames (Function, Coverage, Errors, Notes).
