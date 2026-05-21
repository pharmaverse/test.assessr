# Initialise a temporary coverage environment (framework-aware)

Initialise a temporary coverage environment (framework-aware)

## Usage

``` r
initialise_covr_env(pkg_source_path, pkg, framework = "none", work_dir = NULL)
```

## Arguments

- pkg_source_path:

  Character; package root.

- pkg:

  Character; package name.

- framework:

  Character; e.g. "testthat", "testit", "bioc", "none".

- work_dir:

  Optional explicit temp workspace; if NULL it is composed from inputs.

## Value

environment cov_env with \$work_dir and \$pkg_root set.
