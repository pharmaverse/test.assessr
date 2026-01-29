# Load Datasets from a Package

Loads all datasets from a specified installed package into the global
environment. It first attempts to load \`.rda\` files from the package's
\`data/\` directory. If none are found, it falls back to using
\`data()\` to load datasets listed in the package metadata.

## Usage

``` r
load_package_datasets(pkg_name, env = cov_env)
```

## Arguments

- pkg_name:

  A character string specifying the name of the package.

- env:

  A character string specifying the covr env

## Value

A character vector of dataset names that were attempted to be loaded.
Returns \`NULL\` if the package is not installed.

## Details

This function is intended for internal use. It silently attempts to load
each dataset and suppresses errors if loading fails. Datasets are loaded
into the global environment.
