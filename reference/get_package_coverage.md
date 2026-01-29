# get test package coverage

simplified input to assess package for test coverage

## Usage

``` r
get_package_coverage(path = NULL)
```

## Arguments

- path:

  (optional) path of locally stored package source code

## Value

list containing results - list containing package coverage

## Examples

``` r
if (FALSE) { # \dontrun{
package_coverage <- get_package_coverage()

package_coverage <- get_package_coverage(path/to/package.tar.gz)
} # }
```
