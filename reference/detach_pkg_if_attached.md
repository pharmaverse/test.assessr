# Detach a package if attached (internal)

Safely detaches a package from the search path and optionally unloads
its namespace.

## Usage

``` r
detach_pkg_if_attached(pkg, unload_namespace = TRUE, quiet = TRUE)
```
