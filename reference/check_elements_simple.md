# Check Which Elements Exist in a List

This internal function checks whether specified elements exist in a
given list and returns two vectors: elements that exist and elements
that do not.

## Usage

``` r
check_elements_simple(x, elems)
```

## Arguments

- x:

  A list object to check (e.g., \`test_results\`).

- elems:

  A character vector of element names to check.

## Value

A list with:

- present:

  Character vector of elements that exist in the list.

- missing:

  Character vector of elements that do not exist in the list.
