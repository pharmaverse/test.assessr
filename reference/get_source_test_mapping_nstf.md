# Map R Source Files to Corresponding Test Files

This internal NSTF utility scans the package's \`R/\` directory and the
specified test directory and attempts to match each source file with a
corresponding test file. Matching is based on filename similarity
between source files and test files (case-insensitive, substring
matching).

## Usage

``` r
get_source_test_mapping_nstf(pkg_source_path, test_dir)
```

## Arguments

- pkg_source_path:

  Character string. Path to the root directory of the package source.

- test_dir:

  Character string. Path to the directory containing test files
  (typically \`"tests/"\`, \`"tests/testthat/"\`, or
  \`"tests/testit/"\`).

## Value

A base R \`data.frame\` containing two columns:

\* \*\*source_file\*\* — Name of the \`.R\` file found in the \`R/\`
directory \* \*\*test_file\*\* — Name of the matched test file from
\`test_dir\`, or \`NA\` if no matching file is found

The mapping shows which source files have corresponding tests and which
do not, enabling automated checks for test completeness in NSTF
pipelines.

## Details

The function is intended for lightweight structural checks in NSTF
workflows and does not interpret test contents.

## Output Meaning

A row with \`test_file = NA\` indicates that no test file appears to
match the given source file. The returned data frame is solely a
structural mapping; no tests are run, and no side effects occur.

## See also

Other nstf_utility: [`check_covr_skip_nstf()`](check_covr_skip_nstf.md),
[`check_covr_skip_testit()`](check_covr_skip_testit.md),
[`get_nstf_test_path()`](get_nstf_test_path.md),
[`run_covr_skip_nstf()`](run_covr_skip_nstf.md)
