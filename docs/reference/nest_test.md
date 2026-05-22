# Group Parsed Testthat Code into Logical Test Blocks (Internal)

This internal helper groups parse data from a testthat test file into
logical test blocks based on calls to block‑defining functions such as
\`context()\`, \`test_that()\`, \`describe()\`, \`it()\`, or
\`tar_test()\`. It identifies top‑level expressions that match a given
function‑name regex and returns a named list of data frames—one per
detected block—trimmed to the relevant code region.

## Usage

``` r
nest_test(x, token_text = "^context$")
```

## Arguments

- x:

  data.frame. Parse data produced by
  `utils::getParseData(parse(file, keep.source = TRUE), includeText = TRUE)`.
  Must include columns such as `token`, `text`, `parent`, and row names
  that correspond to node ids.

- token_text:

  Character. A regular expression used to select the block‑starting
  function names (e.g., `"^context$"`,
  `"^test_that$|^describe$|^tar_test$"`, or `"^it$"`). Defaults to
  `"^context$"`.

## Value

A named list of data frames, where each element represents the parse
rows for a single detected test block. Names encode the block function
and label. If no matching blocks are found, an empty list is returned.
This structure is consumed by higher‑level STF mapping helpers (e.g.,
[`map_test()`](map_test.md), [`map_tests_stf()`](map_tests_stf.md)) to
derive expectations, contexts, and line ranges.

## Details

The function locates top‑level expressions whose function call tokens
(`SYMBOL_FUNCTION_CALL`) match `token_text`. For each matching block,
it:

- assigns a human‑readable name using the function name and its string
  constant argument (when available),

- trims the block to exclude leading parse noise, and

- returns the subset of `x` corresponding to that block.

Block names are prefixed with the function (e.g., `"test_that: ..."`,
`"describe: ..."`, `"it: ..."`, `"tar_test: ..."`) when the function is
recognized.

## See also

[`getParseData`](https://rdrr.io/r/utils/getParseData.html),
[`map_test`](map_test.md), [`map_tests_stf`](map_tests_stf.md)

Other stf_utility:
[`create_coverage_skip_stf()`](create_coverage_skip_stf.md),
[`map_test()`](map_test.md), [`map_tests_stf()`](map_tests_stf.md),
[`run_covr_skip_stf()`](run_covr_skip_stf.md)
