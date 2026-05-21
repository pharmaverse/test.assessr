# Create a Summary of Coverage Results

This function extracts overall test coverage and file‑level coverage
information from a covr coverage object. It provides a simplified
summary for downstream reporting and scoring.

## Usage

``` r
create_results_summary(cvr)
```

## Arguments

- cvr:

  A covr coverage object, typically produced by
  \`covr::environment_coverage()\` or by higher‑level wrapper functions
  in this package (e.g., skip‑aware coverage for STF or NSTF workflows).

## Value

A data frame containing summary statistics, including:

- total_cov:

  Numeric. Overall percentage of lines covered.

- name:

  Character. File or package name associated with the coverage result.

- coverage:

  List or data structure summarizing file‑level or element‑level
  coverage from the covr object.

- errors:

  Character vector containing any coverage‑related errors (if present).

- notes:

  Additional diagnostic notes returned by coverage processing.

The summary object is used by downstream reporting functions to generate
test coverage summaries and scoring metrics.
