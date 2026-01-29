# Load Imported and Suggested Packages into Environment

This function reads the DESCRIPTION file of a package and loads all
packages listed under \`Imports\` and \`Suggests\` into the specified
environment. This ensures that functions from those packages are
available when running coverage with \`covr::environment_coverage()\`.

## Usage

``` r
load_dependencies_into_env(pkg_source_path, env)
```

## Arguments

- pkg_source_path:

  Path to the source directory of the package.

- env:

  Environment into which the packages should be loaded.

## Value

Invisibly returns TRUE if all packages are loaded successfully.
