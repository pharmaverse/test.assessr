# Safely perform a left join with error handling

This helper function attempts to join two data frames using
\`dplyr::left_join()\`. If an error occurs during the join (for example,
due to missing columns or incompatible types), the function issues a
warning and returns \`NULL\` instead of stopping execution.

## Usage

``` r
safe_left_join(x, y, by, relationship = "many-to-many", ...)
```

## Arguments

- x:

  A data frame or tibble. The left-hand side of the join.

- y:

  A data frame or tibble. The right-hand side of the join.

- by:

  A character vector specifying the join keys. Passed to
  \`dplyr::left_join()\`.

- relationship:

  A string specifying the expected relationship between keys:
  \`"one-to-one"\`, \`"one-to-many"\`, \`"many-to-one"\`, or
  \`"many-to-many"\`. Defaults to \`NULL\` (no check).

## Value

A data frame resulting from the left join if successful, otherwise
\`NULL\` when an error occurs.
