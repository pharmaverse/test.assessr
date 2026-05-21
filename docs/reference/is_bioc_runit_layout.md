# Detect if a package uses the Bioconductor RUnit layout

Heuristics: - inst/unitTests exists, OR - tests/run_unitTests.R exists,
OR - explicit flag in test_pkg_data\$has_BioG_test

## Usage

``` r
is_bioc_runit_layout(pkg_source_path, test_pkg_data = NULL)
```
