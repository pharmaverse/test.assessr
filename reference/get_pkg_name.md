# Get Package Name for Display

Extracts a display-friendly package name from either a file path or a
filename. The function removes directory components (if present) and
then returns the substring up to the first underscore or hyphen. This is
useful for converting paths or tarball names into a clean package
identifier.

## Usage

``` r
get_pkg_name(input_string)
```

## Arguments

- input_string:

  Character string. A package filename or a path containing the filename
  (e.g., `"mypkg_1.0.0.tar.gz"` or `"/path/to/mypkg_1.0.0.tar.gz"`).

## Value

A character scalar containing the cleaned package name.

The returned object is always of class `character` and corresponds to
the portion of the filename before the first underscore or hyphen.

## Output Meaning

The value represents a human‑readable package name extracted from a file
path or filename. It does not validate whether the extracted name
corresponds to an installed or existing package—only that it conforms to
the expected tarball naming convention.

## Examples

``` r
# \donttest{
pkg_source_path <- "/home/user/R/test.package.0001_0.1.0.tar.gz"
pkg_disp_1 <- get_pkg_name(pkg_source_path)
print(pkg_disp_1)
#> [1] "test.package.0001"

pkg <- "TxDb.Dmelanogaster.UCSC.dm3.ensGene_3.2.2.tar.gz"
pkg_disp_2 <- get_pkg_name(pkg)
print(pkg_disp_2)
#> [1] "TxDb.Dmelanogaster.UCSC.dm3.ensGene"
# }
```
