# Check for tests/testthat and \_snaps folder and count golden tests

Check for tests/testthat and \_snaps folder and count golden tests

## Usage

``` r
check_pkg_tests_and_snaps(pkg_source_path)
```

## Arguments

- pkg_source_path:

  Path to the root of the package source

## Value

A list with: - \`has_testthat\`: Does tests/testthat exist? -
\`has_testit\`: Does tests/testit exist? - \`has_tests_base\`: Do tests
exist in tests subdirectory? - \`has_BioG_test\`: Do BioGenerics tests
exist in inst/tests subdirectory? - \`has_RUnit_test\`: Do RUnit tests
exist (inst/unitTests; optionally tests/run_unitTests.R)? -
\`bioc_unit_tests_dir\` : location of BioGenerics unit tests -
\`bioc_run_ut_path\`: location of BioGenerics \`run_unitTests.R\` -
\`has_snaps\`: Does \_snaps exist inside tests/testthat? -
\`n_golden_tests\`: Number of snapshot files inside \_snaps -
\`n_test_files\`: Number of test-\*.R files inside tests/testthat -
\`has_tinytest\` : Does tinytest exist? - \`tinytest_dir\` : location of
tinytest unit tests - \`tinytest_runner_paths\` : location of tinytest
runner paths - \`n_tinytest_files\` : Number of test\_\*.R files under
inst/tinytest - \`has_data_table_tests\`: Do data.table tests exist in
directories? - \`data_table_tests_path\` : location of data.table unit
tests
