# Load exported objects from installed packages into an environment

Copy exported objects (typically functions, but also exported constants
or other objects) from one or more installed package namespaces into a
target environment.

## Usage

``` r
load_package_exports_into_env(pkg_names, env, overwrite = FALSE)
```

## Arguments

- pkg_names:

  Character vector of package names whose exported objects should be
  copied into \`env\`.

- env:

  Environment into which exported objects should be assigned.

- overwrite:

  Logical; if \`TRUE\`, overwrite objects already present in \`env\`. If
  \`FALSE\` (default), existing objects are left unchanged.

## Value

A character vector of fully qualified object names that were loaded, in
the form \`"pkg::name"\`.

## Details

This is useful when evaluating code in an isolated environment, such as
one created with \`new.env(parent = baseenv())\`, where package exports
that are normally available on the search path (for example from
\`stats\` or \`utils\`) are not automatically visible.

The function does not attach packages to the search path. Instead, it
copies exported objects directly into \`env\`.

For each package in \`pkg_names\`, this function:

1.  checks whether the package namespace is available,

2.  retrieves the package's exported names,

3.  obtains each exported object with \`getExportedValue()\`,

4.  assigns the object into \`env\`.

Packages that are not installed or cannot be loaded are skipped with a
message.
