# Set up a coverage environment for isolated test execution

Prepare an existing coverage environment for use with coverage workflows
that evaluate tests in an isolated environment, typically one created
with \`new.env(parent = baseenv())\`.

## Usage

``` r
setup_covr_env(
  pkg_source_path,
  pkg_name,
  test_path = "tests/testthat",
  cov_env
)
```

## Arguments

- pkg_source_path:

  Character scalar. Path to the source package root.

- pkg_name:

  Character scalar. Package name.

- test_path:

  Character scalar. Path to the test directory. Defaults to
  \`"tests/testthat"\`.

- cov_env:

  Environment. A pre-existing coverage environment, usually created by
  an initializer such as \`initialise_covr_env()\`.

## Value

A named list with components:

- cov_env:

  The populated coverage environment.

- datasets_loaded:

  Character vector of dataset objects loaded into \`cov_env\` from the
  target package, \`datasets\`, and optionally \`nycflights13\`.

- base_r_datasets_loaded:

  Character vector of datasets loaded from the \`datasets\` package.

- base_r_exports_loaded:

  Character vector of exported objects copied from Base R packages other
  than \`base\` and \`datasets\`.

- root_dir:

  Resolved project root path.

- initial_state:

  Character vector of object names present in \`cov_env\` before setup.

- helpers_loaded:

  Character vector of helper files successfully sourced into
  \`cov_env\`.

## Details

The setup process:

\- loads helper files from \`tests/testthat\`, - loads package namespace
objects into \`cov_env\`, - resolves the project root, - loads package
datasets into \`cov_env\`, - loads Base R datasets and exports into
\`cov_env\`, - optionally loads \`nycflights13\` datasets if
available, - loads dependencies into \`cov_env\`, and - unloads the
package namespace afterward to reduce conflicts.

This function does \*\*not\*\* create \`cov_env\`; it expects an
existing environment to be supplied.

This function is designed for workflows where \`cov_env\` is
deliberately isolated (for example with \`parent = baseenv()\`). In such
cases, objects from packages like \`stats\` and \`datasets\` are not
automatically visible, so they are explicitly loaded into \`cov_env\`.

The package \`nycflights13\` is treated as optional: if it is installed,
its datasets are loaded into \`cov_env\`; otherwise it is silently
skipped.
