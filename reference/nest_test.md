# Nest parsed testthat code into logical test blocks

Internal helper function to group parsed testthat code into blocks based
on context, test_that, describe, or it calls.

## Usage

``` r
nest_test(x, token_text = "^context$")
```

## Arguments

- x:

  data.frame, parse data from utils::getParseData

- token_text:

  character, regex pattern to match test block function names, Default:
  '^context\$'

## Value

named list of data.frames, each representing a nested test block
