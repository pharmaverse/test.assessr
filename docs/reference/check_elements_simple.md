# Check Which Elements Exist in a List

This internal function checks whether specified elements exist in a
given list and returns two vectors: elements that exist and elements
that do not. When `nested = TRUE` and `x` has `results`
(multi-framework), checks inside each `x$results$<framework>`.

## Usage

``` r
check_elements_simple(x, elems, nested = FALSE)
```

## Arguments

- x:

  A list object to check (e.g., \`test_results\`).

- elems:

  A character vector of element names to check.

- nested:

  If TRUE and `x` has `results`, check inside each framework block.
  Returns `present` (flattened unique) and `by_framework` (named list:
  framework -\> present elements).

## Value

A list with:

- present:

  Character vector of elements that exist.

- missing:

  Character vector of elements that do not exist (top-level only).

- by_framework:

  Named list (framework -\> present elements), when nested.
