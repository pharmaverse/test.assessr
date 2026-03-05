# Load package objects into a coverage environment

This internal function loads all objects from a package namespace into a
specified environment. It is used by \`create_covr_list_no_skip()\` to
prepare the environment for coverage analysis.

## Usage

``` r
load_package_into_env(pkg_source_path, pkg, cov_env)
```

## Arguments

- pkg_source_path:

  Path to the package source directory.

- pkg:

  Name of the package to load.

- cov_env:

  Environment into which package objects will be loaded.

## Value

Invisibly returns TRUE if successful, otherwise stops with an error.
