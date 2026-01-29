# Collect all applicable nonstandard test paths for a package (internal)

Detects and returns all relevant testing directories used by an R
package, combining framework-specific and filesystem-based checks.
Supported layouts:

- **testthat**: `tests/testthat/`

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

  `list`. A list of booleans describing detected test frameworks and
  layouts. The following fields are consulted (all optional, defaulting
  to FALSE if missing):

  - `has_testthat`: logical; whether `tests/testthat/` is present.

  - `has_testit`: logical; whether `tests/testit/` or the nonstandard
    `tests/test-ci/ + tests/test-cran/` layout is present.

  - `has_tests_base`: logical; whether `*.R` test files exist directly
    under `tests/`.

  - `has_bioc_unit_tests`: logical; whether `inst/unitTests/` exists
    (Bioconductor layout).

- testdir:

  `character`. Path to the package `tests/` directory. The function will
  also derive the package root as `dirname(testdir)` to locate
  `inst/unitTests/`.

## Value

`list`. A named list of character paths to all applicable test
directories found. Possible names include:

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
list contains a single entry `default_tests_root = <tests/>`.
