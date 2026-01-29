# Generate HTML Report for Package Test Assessment

Generates an HTML report for the package test assessment results using
rmarkdown.

## Usage

``` r
generate_test_report(test_results, output_dir = NULL)
```

## Arguments

- test_results:

  List containing the results from get_package_coverage function.

- output_dir:

  Character string indicating the directory where the report will be
  saved.

## Value

Path to the generated HTML report.

## Examples

``` r
if (FALSE) { # \dontrun{
test_results <- get_package_coverage()
generate_test_report(test_results, output_dir = "path/to/save/report")
} # }
```
