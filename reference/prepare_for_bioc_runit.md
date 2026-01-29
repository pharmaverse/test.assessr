# Prepare a BioC/RUnit package for testing

Detaches common BioC infrastructure packages (if attached) and
load_all() the target package before RUnit tests. Also clears R_TESTS to
avoid interference from the check harness.

## Usage

``` r
prepare_for_bioc_runit(pkg_name, pkg_source_path)
```
