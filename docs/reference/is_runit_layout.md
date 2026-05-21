# Detect if a package uses RUnit layout (BioC or RUnit-only)

Returns TRUE when the package has RUnit tests in inst/unitTests (with or
without the BiocGenerics .test() runner). Heuristics: -
test_pkg_data\$has_BioG_test (BioC full layout), OR -
test_pkg_data\$has_RUnit_test (inst/unitTests, no .test required), OR -
inst/unitTests exists, OR - tests/run_unitTests.R exists

## Usage

``` r
is_runit_layout(pkg_source_path, test_pkg_data = NULL)
```

## Arguments

- pkg_source_path:

  Character. Path to package source root.

- test_pkg_data:

  List or NULL. Output from check_pkg_tests_and_snaps().

## Value

Logical. TRUE if the package uses RUnit layout.
