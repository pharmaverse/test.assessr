# Safely unload a package to avoid coverage conflicts

This internal function attempts to unload a specified package if it is
currently loaded. It is used to prevent coverage conflicts when
reloading the package in a custom environment.

## Usage

``` r
safe_unload_package(pkg)
```

## Arguments

- pkg:

  Character string. The name of the package to unload.

## Value

NULL (invisibly). Messages are printed to indicate success or failure.
