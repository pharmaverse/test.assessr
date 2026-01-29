# Creates information on package installation

Creates information on package installation

## Usage

``` r
set_up_pkg(dp, check_type = "1")
```

## Arguments

- dp:

  data path and name for the package.

- check_type:

  basic R CMD check type - "1" CRAN R CMD check_type - "2"

## Value

list with local package install

## Examples

``` r
if (FALSE) { # \dontrun{
set_up_pkg(path/to/package, "mypackage")
} # }
```
