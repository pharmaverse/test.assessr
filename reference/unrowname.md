# Add Identifier Column and Remove Row Names from a Data Frame

This internal helper restructures a data frame by adding an identifier
column (e.g., file name or test label) and removing row names. It is
used by STF parsing functions to attach contextual labels (such as test
block names or file names) to expectation or test‑block structures.

## Usage

``` r
unrowname(identifier, ret, label)
```

## Arguments

- identifier:

  Character. The identifier to insert into the resulting data frame
  (typically the name of the list element being processed).

- ret:

  List of data frames, usually the output of applying a mapping or
  extraction function across multiple parsed test blocks.

- label:

  Character. Name of the column used to store the identifier (e.g.,
  `"test"` or `"file"`).

## Value

A `data.frame` with an identifier column prepended and row names
removed. The remaining columns retain their original structure.

## Details

The function:

- extracts the data frame corresponding to `identifier`,

- appends a new column containing the identifier value,

- removes row names for clean downstream processing, and

- reorders columns so the identifier appears first.

This structure is used by STF mapping helpers such as
[`nest_expect()`](nest_expect.md) and
[`map_tests_stf()`](map_tests_stf.md) to carry forward file or test
labels when constructing unified test‑mapping tables.
