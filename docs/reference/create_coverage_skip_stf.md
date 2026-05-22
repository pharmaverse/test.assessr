# Run Coverage While Skipping Failing Tests

This function runs coverage analysis using \`covr\` while temporarily
commenting out known failing tests. It is intended for situations where
specific tests must be skipped (e.g., due to upstream bugs or known
failures) so that coverage can still be computed on remaining tests.

## Usage

``` r
create_coverage_skip_stf(
  pkg_source_path,
  pkg,
  test_path = "tests/testthat",
  test_skip,
  test_map,
  cov_env
)
```

## Arguments

- pkg_source_path:

  Character. Path to the root of the package source.

- pkg:

  Character. Name of the package being tested.

- test_path:

  Character. Path to the test directory. Defaults to
  \`'tests/testthat'\`.

- test_skip:

  Data frame. Mapping of failing tests, typically containing columns
  \`file\`, \`line1\`, and \`line2\`. Each row describes a span of lines
  that should be commented out before running coverage.

- test_map:

  Data frame. Mapping of all tests within the package (used for
  diagnostic messaging and consistency checks).

- cov_env:

  Environment that holds the temporary coverage workspace and shared
  state for the run.

## Value

A coverage object (a list) returned by \`covr::environment_coverage()\`,
filtered to remove problematic entries. If coverage fails, the function
returns \`NULL\`.

The returned structure typically contains:

- filename:

  Name of the file being measured.

- functions:

  Function-level coverage information.

- linecoverage:

  Line-by-line execution counts.

- srcref:

  Source references extracted for coverage.

## Details

The function:

- sets up a temporary coverage environment,

- identifies lines to skip based on \`test_skip\`,

- comments out those test lines,

- runs \`covr::environment_coverage()\`, and

- restores the original test files on exit.

If \`test_skip\` is \`NULL\`, failing tests must be detected elsewhere
before calling this function. This function does not perform test
detection; it only applies the skip mappings supplied in \`test_skip\`.

## See also

[`test_dir`](https://testthat.r-lib.org/reference/test_dir.html),
[`package_coverage`](http://covr.r-lib.org/reference/package_coverage.md),
[`environment_coverage`](http://covr.r-lib.org/reference/environment_coverage.md)

Other stf_utility: [`map_test()`](map_test.md),
[`map_tests_stf()`](map_tests_stf.md), [`nest_test()`](nest_test.md),
[`run_covr_skip_stf()`](run_covr_skip_stf.md)
