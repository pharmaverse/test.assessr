# Create Information on Local Package Installation

This function unpacks a package tarball using
[`unpack_tarball()`](unpack_tarball.md) and returns information about
whether the unpacked directory exists. It is a lightweight preparatory
step used before attempting a local package installation.

## Usage

``` r
set_up_pkg(dp)
```

## Arguments

- dp:

  Character string. Path to a package tarball or package directory.

## Value

A named list with the following elements:

- `package_installed` — Logical. `TRUE` if the unpacked package
  directory exists, otherwise `FALSE`.

- `pkg_source_path` — Character string giving the unpacked package
  source directory, or `""` if unpacking failed.

The returned object is always a base R `list`. It contains no side
effects besides calling [`unpack_tarball()`](unpack_tarball.md) and
checking filesystem paths.

## Output Meaning

A value of `package_installed = TRUE` indicates that the unpacked
directory exists on disk and can be used for local installation.

`package_installed = FALSE` indicates either:

- [`unpack_tarball()`](unpack_tarball.md) returned an empty result, or

- the unpacked directory does not exist on disk.

## Examples

``` r
if (FALSE) { # \dontrun{
set_up_pkg(path/to/package, "mypackage")
} # }
```
