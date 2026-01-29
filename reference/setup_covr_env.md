# Setup Coverage Environment

Internal utility to prepare a coverage environment for a package by
loading helpers, package code, datasets, and dependencies. Also locates
the project root, records the initial environment state, and safely
unloads the package to avoid conflicts.

## Usage

``` r
setup_covr_env(pkg_source_path, pkg_name, test_path = "tests/testthat")
```

## Arguments

- pkg_source_path:

  Path to the source directory of the package.

- pkg_name:

  Name of the package.

- test_path:

  Path to the testthat directory. Defaults to "tests/testthat".

## Value

A list containing:

- cov_env:

  The prepared coverage environment.

- datasets_loaded:

  Character vector of loaded dataset names.

- root_dir:

  Path to the project root.

- initial_state:

  Character vector of object names in the environment before setup.

- helpers_loaded:

  Character vector of helper file paths that were sourced.
