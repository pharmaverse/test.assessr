# Extract expectation calls from parsed test code

Internal helper function to identify and extract expectation function
calls (e.g., expect_equal) from parsed testthat code.

## Usage

``` r
get_expect(parsed_data, token_text = "^expect_")
```

## Arguments

- parsed_data:

  data.frame, parse data from utils::getParseData

- token_text:

  character, regex pattern to match expectation function names, Default:
  '^expect\_'

## Value

data.frame with expectation names and their line ranges
