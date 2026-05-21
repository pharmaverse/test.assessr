# Map Bioconductor source files to test files, avoiding false positives. Tightens filename and content-based matching; handles short names like "t".

Map Bioconductor source files to test files, avoiding false positives.
Tightens filename and content-based matching; handles short names like
"t".

## Usage

``` r
get_source_test_mapping_bioc_nstf(pkg_source_path, test_dir)
```

## Arguments

- pkg_source_path:

  Character; path to the package root (expects R/ under it).

- test_dir:

  Character; path to tests directory (e.g., file.path(pkg, "tests"))

## Value

data.frame with columns: source_file, test_file, evidence, has_tests
