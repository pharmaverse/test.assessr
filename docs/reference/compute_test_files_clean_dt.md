# Compute wrapper data.table test files to run for coverage (internal)

Selects and sanitizes the list of \*\*wrapper\*\* test files to execute
during coverage, supporting both the standard \`tests/\` layout and the
\`testit\` layout (\`tests/testit/\`). When \`pkg_name\` is
\`"data.table"\`, the function adds package-aware behavior: it falls
back to \`tests/main.R\` if the mapping yields no existing files and
optionally prioritizes \`froll.R\` so rolling function tests run early.

## Usage

``` r
compute_test_files_clean_dt(
  pkg_name,
  pkg_source_path,
  clean_mapping,
  test_pkg_data,
  skip_tests = NULL
)
```

## Arguments

- pkg_name:

  \`character(1)\`. Package name.

- pkg_source_path:

  \`character(1)\`. Absolute or relative path to the package source
  root.

- clean_mapping:

  A data frame or list containing at least a \`test_file\`
  column/element with \*\*relative\*\* wrapper paths to run. Paths are
  resolved against \`tests/\` or \`tests/testit/\` depending on
  \`test_pkg_data\$has_testit\`.

- test_pkg_data:

  \`list\`. Must include logical flag \`has_testit\` indicating whether
  the package uses a \`tests/testit/\` layout.

- skip_tests:

  \`NULL\` or \`character()\`. Relative wrapper paths to exclude
  (matched within the chosen base directory).

## Value

\`character()\` - \*\*normalized\*\*, de-duplicated vector of wrapper
file paths that \*\*exist\*\* on disk. May be zero length if no
candidates exist and no fallback is available.

## Details

This helper \*\*only resolves file paths\*\*; it does \*\*not\*\* modify
the evaluation environment, copy/mirror test assets, or toggle
data.table's test harness mode. Any package-specific preparation (e.g.,
mirroring \`inst/tests/\` to \`tests/\` or exposing
\`test.data.table()\` in dev mode) must be handled elsewhere (see
\`prepare_tests_for_package()\`).
