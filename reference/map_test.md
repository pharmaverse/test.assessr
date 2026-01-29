# Map the hierarchy structure of a testthat test file

Query a test file to see what expectations are defined in it.

## Usage

``` r
map_test(path)
```

## Arguments

- path:

  character, path to file

## Value

data.frame

## Details

Return data.frame containing which expectations are in the test file by
context/description/test/expectation/linerange

## See also

[`getParseData`](https://rdrr.io/r/utils/getParseData.html)

Other utility: [`create_coverage_skip_stf()`](coverage_skip.md),
[`map_tests_stf()`](map_testthat.md)
