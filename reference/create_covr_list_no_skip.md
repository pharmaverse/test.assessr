# create covr_list when no tests are skipped

create covr_list when no tests are skipped

## Usage

``` r
create_covr_list_no_skip(
  test_map,
  test_results_long,
  pkg_source_path,
  pkg,
  preserve_vars = "old"
)
```

## Arguments

- test_map:

  \- R object with mapped tests

- test_results_long:

  \- cvr object with no tests skipped

- pkg_source_path:

  \- path to the package

- pkg:

  \- name of the package

- preserve_vars:

  Character. preserve \`old\` for CRAN examples

## Value

\- summary cvr object
