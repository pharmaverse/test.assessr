# Extract Expectation Calls from Parsed Testthat Code

This internal helper identifies expectation calls (e.g.,
\`expect_equal()\`, \`expect_true()\`, etc.) within parse data extracted
from a testthat test file. The function locates expectation function
calls by matching their token text and returns a structured data frame
containing the expectation names and their corresponding source line
ranges.

## Usage

``` r
get_expect(parsed_data, token_text = "^expect_")
```

## Arguments

- parsed_data:

  data.frame. Parse data produced by
  `utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)`.
  Must include the columns `token`, `text`, `line1`, and `line2`.

- token_text:

  Character. A regular expression used to detect expectation‑style
  function calls. Defaults to `"^expect_"`.

## Value

A `data.frame` where each row corresponds to a detected expectation. The
data frame contains:

- expectation:

  Character. Name of the expectation function (e.g., `"expect_equal"`).

- line1:

  Integer. Starting line number of the expression.

- line2:

  Integer. Ending line number of the expression.

Returns `NULL` if no matching expectations are found.

## Details

The function:

- locates all rows where the token is `"SYMBOL_FUNCTION_CALL"` and the
  text matches `token_text`,

- identifies the expression node corresponding to each expectation,

- extracts line ranges (`line1`, `line2`) describing where the
  expectation appears in the source file, and

- returns a consolidated data frame of results.

If no expectation calls are found, the function returns `NULL`. This
helper is used internally by STF mapping functions (such as
[`map_test()`](map_test.md) and [`map_tests_stf()`](map_tests_stf.md))
when constructing a full hierarchy of test blocks and expectations.
