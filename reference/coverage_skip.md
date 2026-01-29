# Run covr with failing tests

Run
[`package_coverage`](http://covr.r-lib.org/reference/package_coverage.md)
ignoring failing tests from
[`test_dir`](https://testthat.r-lib.org/reference/test_dir.html).

## Usage

``` r
create_coverage_skip_stf(
  pkg_source_path,
  pkg,
  test_path = "tests/testthat",
  test_skip,
  test_map,
  preserve_vars = "old"
)
```

## Arguments

- pkg_source_path:

  Character. path to the package

- pkg:

  Character. Name of the package being tested.

- test_path:

  character, path to test directory, Default: 'tests/testthat'

- test_skip:

  data.frame, mapping of failing tests

- test_map:

  data.frame, mapping of all the tests in the package

- preserve_vars:

  Character. preserve \`old\` for CRAN examples

## Details

If test_skip is NULL then a new test_dir will be run internally checking
for failing tests.

## See also

[`test_dir`](https://testthat.r-lib.org/reference/test_dir.html)
[`package_coverage`](http://covr.r-lib.org/reference/package_coverage.md)

Other utility: [`map_test()`](map_test.md),
[`map_tests_stf()`](map_testthat.md)
