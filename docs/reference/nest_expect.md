# Extract and Structure Expectations from Parsed Test Blocks

This internal helper takes a list of parsed test blocks (as produced by
[`nest_test()`](nest_test.md)) and extracts expectation calls from each
block using [`get_expect()`](get_expect.md). It then assembles all
detected expectations into a structured data frame that associates each
expectation with its corresponding test label.

## Usage

``` r
nest_expect(x)
```

## Arguments

- x:

  A named list of parsed test blocks, typically produced by
  [`nest_test()`](nest_test.md). Each element should contain parse data
  suitable for expectation extraction via
  [`get_expect()`](get_expect.md).

## Value

A `data.frame` in which each row corresponds to an expectation extracted
from the supplied test blocks. Typical columns include:

- expectation:

  Name of the expectation function (e.g., `"expect_equal"`).

- line1:

  Starting line number of the expectation expression.

- line2:

  Ending line number of the expectation expression.

- test:

  Label identifying the test block the expectation belongs to.

Returns `NULL` if no expectations are detected.

## Details

For each block in `x`, the function:

- calls [`get_expect()`](get_expect.md) to extract expectation function
  calls and their line ranges,

- removes blocks with no expectations,

- attaches a `test` label using the block's list name, and

- binds all expectation results into a single consolidated data frame.

If no expectations are found in any block, the function returns `NULL`.
