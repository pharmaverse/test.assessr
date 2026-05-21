# Install a Package from a Local Source Directory

Attempts to install an R package from a local source directory using
[`remotes::install_local()`](https://remotes.r-lib.org/reference/install_local.html).
The function reports on whether installation succeeded, whether the
package was already installed, or whether the provided source path does
not exist.

## Usage

``` r
install_package_local(pkg_source_path)
```

## Arguments

- pkg_source_path:

  Character string. Path to the local package source directory (e.g., an
  unpacked package or extracted tarball path).

## Value

A logical value indicating whether the package is installed after
running the function.

The returned object is always of class `logical`:

- `TRUE` — The package is already installed or was successfully
  installed.

- `FALSE` — Installation failed or the path does not exist.

## Details

The display name of the package is derived from the input path using
[`get_pkg_name()`](get_pkg_name.md).

## Output Meaning

`TRUE` does not necessarily imply that the installation occurred during
this function call—it may also mean the package was already installed.

`FALSE` indicates a failure to install or an invalid path. All
diagnostic messages are printed via
[`message()`](https://rdrr.io/r/base/message.html) for user visibility.

## Examples

``` r
if (FALSE) { # \dontrun{
results <- install_package_local("pkg_source_path")
print(results)
} # }
```
