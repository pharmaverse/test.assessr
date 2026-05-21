# Locate active test paths for Bioconductor RUnit layout

Returns a \*list\* of one or more existing test directories suitable for
normalization and use in subsequent mapping (e.g.,
\`get_source_test_mapping_nstf\`). Designed to work with:

## Usage

``` r
get_runit_test_paths(test_pkg_data, testdir)
```

## Arguments

- test_pkg_data:

  list/env of test metadata; may include: - \`bioc_run_ut_path\`
  (character): path to \`tests/run_unitTests.R\` wrapper, if present

- testdir:

  character: expected RUnit tests directory (typically
  "\<pkg_root\>/inst/unitTests")

## Value

named list of \*\*existing\*\* directories (e.g., \`bioc_unitTests\`,
\`tests_root\`); if nothing is found, returns a single \`fallback\`
entry pointing to \`testdir\`.

## Details

path \<- as.character(pkg_source_path) testdir \<- file.path(path,
"inst/unitTests") test_path_list \<-
find_biocgenerics_test_paths(test_pkg_data, testdir) test_path \<-
normalizePath( unlist(test_path_list, use.names = TRUE), winslash = "/",
mustWork = FALSE )
