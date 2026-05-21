# Collect all applicable nonstandard test paths for a package (internal)

Detects and returns all relevant testing directories used by an R
package, combining framework-specific and filesystem-based checks.
Supported layouts:

- **testthat**: `tests/testthat/`

- **tinytest**: `inst/tinytest/` (primary location). Some packages also
  include a small runner under `tests/` (e.g., `tests/tinytest.R`) which
  this function does not return as a path, but it will still detect
  `inst/tinytest/`.

- **testit** (standard): `tests/testit/`

- **testit** (nonstandard): both `tests/test-ci/` and `tests/test-cran/`
  present, using `tests/` as the root

- **base R tests**: `*.R` files directly under `tests/`

- **Bioconductor unit tests**: `inst/unitTests/`

## Usage

``` r
get_nstf_test_path(test_pkg_data, testdir)
```

## Arguments

- test_pkg_data:

  `list`. A list of booleans/fields describing detected test frameworks
  and layouts. All fields are optional and default to `FALSE` or `NULL`
  if missing:

  - `has_testthat`: logical; whether `tests/testthat/` is present.

  - `has_tinytest` (or `has_tiny_test`): logical; whether tinytest is
    present. If true, the function will look for `inst/tinytest/` (or
    use `tinytest_dir` if provided).

  - `tinytest_dir`: character; optional explicit path to the tinytest
    directory. May be absolute or relative to the package root; when
    missing, defaults to `inst/tinytest/`.

  - `has_testit`: logical; whether `tests/testit/` or the nonstandard
    `tests/test-ci/ + tests/test-cran/` layout is present.

  - `has_tests_base`: logical; whether `*.R` test files exist directly
    under `tests/`.

  - `has_bioc_unit_tests`: logical; whether `inst/unitTests/` exists
    (Bioconductor layout).

- testdir:

  `character`. Path to the package `tests/` directory. The function also
  derives the package root as `dirname(testdir)` to locate paths such as
  `inst/unitTests/` and `inst/tinytest/`.

## Value

`list`. A named list of character paths to all applicable test
directories found. Possible names include:

- `tinytest` — `inst/tinytest/`

- `testthat` — `tests/testthat/`

- `testit` — `tests/testit/`

- `nonstandard_testit_root` — `tests/` (when both `test-ci` and
  `test-cran` exist)

- `base_tests_root` — `tests/` (when base `*.R` files exist in `tests/`
  root)

- `bioc_unitTests` — `inst/unitTests/`

- `default_tests_root` — `tests/` (returned only if no other paths are
  detected)

## Details

The function returns a named list of paths for all detected testing
setups. It uses both the flags contained in `test_pkg_data` and direct
filesystem checks for robustness. If no paths are detected, the returned
list contains a single entry `default_tests_root = tests/`.

Identical paths arising from multiple detection routes (e.g., `tests/`
playing multiple roles) are de-duplicated while preserving the first
descriptive label. If `tinytest_dir` is provided and relative, it is
resolved against the package root (i.e., `dirname(testdir)`).

## See also

Other nstf_utility:
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`create_base_tests_coverage()`](create_base_tests_coverage.md),
[`create_nstf_covr_list()`](create_nstf_covr_list.md),
[`create_tinytest_coverage()`](create_tinytest_coverage.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`restrict_test_paths_to_framework()`](restrict_test_paths_to_framework.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
