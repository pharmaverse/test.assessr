# Map the hierarchy structure of tests directory in standard testing framework

Query a test directory for the unit test structure.

## Usage

``` r
map_tests_stf(path = "tests/testthat")
```

## Arguments

- path:

  character, path to tests, Default: 'tests/testthat'

## Value

data.frame

## Details

Return data.frame containing which expectations are in the testthat
directory by file/context/description/test/expectation/linerange

## See also

[`names`](https://rdrr.io/r/base/names.html),
[`lapply`](https://rdrr.io/r/base/lapply.html),
[`rbind`](https://rdrr.io/r/base/cbind.html)

Other utility: [`create_coverage_skip_stf()`](coverage_skip.md),
[`map_test()`](map_test.md)
