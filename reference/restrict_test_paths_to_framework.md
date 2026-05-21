# Restrict a list of test paths to a single framework

Filters a named list of candidate test directories (typically produced
by [`get_nstf_test_path()`](get_nstf_test_path.md)) down to the entry
that corresponds to a requested framework. If no usable mapping can be
made, the input list is returned unchanged.

## Usage

``` r
restrict_test_paths_to_framework(test_path_list, framework)
```

## Arguments

- test_path_list:

  `list`. Named list of candidate test paths. Known names include
  `"testthat"`, `"testit"`, `"tinytest"`, `"base_tests_root"`, and
  optionally `"nonstandard_testit_root"`.

- framework:

  `character(1)` or `NULL`. Framework identifier (e.g., `"testthat"`,
  `"testit"`, `"tests_base"`, `"tinytest"`). If `NULL` or empty (`""`),
  the list is returned unchanged.

## Value

`list`. Either:

- a single-element named list containing only the resolved framework
  path (name is the selected key), or

- the original `test_path_list` if `framework` is `NULL`/empty, maps to
  an unknown key, or the resolved key is absent in `test_path_list`.

## Details

The function maps `framework` to a primary key in `test_path_list` as
follows:

- `"testthat"` → `"testthat"`

- `"testit"` → `"testit"` (falls back to `"nonstandard_testit_root"` if
  `"testit"` is `NULL`)

- `"tests_base"` → `"base_tests_root"`

- `"tinytest"` → `"tinytest"`

For `framework == "testit"`, if `test_path_list[["testit"]]` is `NULL`
and `test_path_list[["nonstandard_testit_root"]]` is present, the latter
is used instead. No directories are created or validated here; the
function only selects from the provided list.

## See also

[`get_nstf_test_path`](get_nstf_test_path.md)

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
