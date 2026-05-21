# Load Base R packages into an isolated coverage environment

Populate an isolated environment (typically created with
\`new.env(parent = baseenv())\`) with objects needed to mimic the
standard Base R package surface during coverage or test evaluation.

## Usage

``` r
load_base_r_packages_into_env(env, overwrite = FALSE)
```

## Arguments

- env:

  An environment to populate.

- overwrite:

  Logical; if \`TRUE\`, overwrite objects already present in \`env\`. If
  \`FALSE\` (default), existing objects are left unchanged.

## Value

A named list with two character vectors:

- exports_loaded:

  Exported objects copied from Base R packages other than \`base\` and
  \`datasets\`.

- datasets_loaded:

  Dataset objects loaded from the \`datasets\` package.

## Details

The \`base\` package itself is already available via \`baseenv()\` and
is therefore not copied into \`env\`. The function instead:

\- loads datasets from the \`datasets\` package into \`env\`, and -
copies exported objects from other Base R packages into \`env\` (e.g.
\`stats\`, \`utils\`, \`graphics\`, \`grDevices\`, \`methods\`, etc.).

This is useful when tests are evaluated in a deliberately isolated
environment whose parent is \`baseenv()\`, because objects from packages
such as \`stats\` (e.g. \`rnorm\`) and \`datasets\` (e.g. \`mtcars\`,
\`iris\`) are not automatically visible there.

The Base R package set handled here is:

- \`base\`

- \`compiler\`

- \`datasets\`

- \`graphics\`

- \`grDevices\`

- \`grid\`

- \`methods\`

- \`parallel\`

- \`splines\`

- \`stats\`

- \`stats4\`

- \`tcltk\`

- \`tools\`

- \`utils\`

Because \`base\` is already reachable through \`baseenv()\`, it is
excluded from the explicit copy step.
