# This internal function takes named character vectors (e.g., \`tests_skipped\`, \`tests_passing\`) from a list and converts them into data frames for easier reporting or processing.

This internal function takes named character vectors (e.g.,
\`tests_skipped\`, \`tests_passing\`) from a list and converts them into
data frames for easier reporting or processing.

## Usage

``` r
convert_vectors_to_df(x, elems)
```

## Arguments

- x:

  A list containing character vectors (e.g., \`test_results\`).

- elems:

  A character vector of element names to convert (must exist in \`x\`).

## Value

A named list of data frames, where each data frame has one column:

- file:

  The file names or paths from the original character vector.
