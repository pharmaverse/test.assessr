# Collect All Applicable Nonstandard Test Paths for a Package (internal)

This internal NSTF utility detects and returns all relevant test
directories used by an R package. It combines declared test framework
flags in `test_pkg_data` with filesystem-based detection for robustness.

## Usage

``` r
get_nstf_test_path(test_pkg_data, testdir)
```

## Arguments

- test_pkg_data:

  `list`. A list of logical flags describing detected test frameworks.
  Missing fields default to `FALSE`. Expected (optional) fields include:

  - `has_testthat`

  - `has_testit`

  - `has_tests_base`

  - `has_bioc_unit_tests`

- testdir:

  `character`. Path to the package's `tests/` directory. The package
  root is inferred as `dirname(testdir)` for detection of
  Bioconductor-style `inst/unitTests/`.

## Value

`list`. A named list of character vectors giving all detected applicable
test directories. Possible names include:

- `testthat` – path to `tests/testthat/`

- `testit` – path to `tests/testit/`

- `nonstandard_testit_root` – `tests/` when both `test-ci/` and
  `test-cran/` exist

- `base_tests_root` – `tests/` if `*.R` files exist directly

- `bioc_unitTests` – path to `inst/unitTests/`

- `default_tests_root` – fallback `tests/` directory

## Details

Supported directory layouts include:

- **testthat**: `tests/testthat/`

- **testit** (standard): `tests/testit/`

- **testit** (nonstandard): both `tests/test-ci/` and `tests/test-cran/`
  present, using `tests/` as the root

- **base R tests**: `*.R` files directly under `tests/`

- **Bioconductor unit tests**: `inst/unitTests/`

If no test directory of any supported type is detected, the output falls
back to a single entry: `default_tests_root = <tests/>`.

## Output Meaning

The returned list describes all test frameworks detected in a package.
Each element is a filesystem path that the NSTF workflow should consider
when identifying or running tests. Duplicate paths (e.g., multiple roles
pointing to `tests/`) are automatically removed.

## See also

Other nstf_utility: [`check_covr_skip_nstf()`](check_covr_skip_nstf.md),
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`get_source_test_mapping_nstf()`](get_source_test_mapping_nstf.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
