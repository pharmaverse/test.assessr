# Setup coverage environment using an existing cov_env (CRAN-safe)

NOTE: - This function no longer creates \`cov_env\` nor writes helper
files into tests/. - Pass an environment (e.g., from
initialise_covr_env()) via \`cov_env\`. - All writes by downstream
helpers should target cov_env\$work_dir (under tempdir()).

## Usage

``` r
setup_covr_env(
  pkg_source_path,
  pkg_name,
  test_path = "tests/testthat",
  cov_env
)
```

## Arguments

- pkg_source_path:

  Character, package root path.

- pkg_name:

  Character, package name.

- test_path:

  Character, path to tests (default "tests/testthat").

- cov_env:

  Environment created by initialise_covr_env(); must exist.

## Value

list(cov_env, datasets_loaded, root_dir, initial_state, helpers_loaded)
