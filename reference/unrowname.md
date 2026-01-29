# Add identifier column and remove row names from a data.frame

Internal helper function to restructure a data.frame by adding an
identifier column and removing row names.

## Usage

``` r
unrowname(identifier, ret, label)
```

## Arguments

- identifier:

  character, the name or label to assign to the new column

- ret:

  list of data.frames, typically the result of applying a function over
  multiple files

- label:

  character, name of the new column to store the identifier

## Value

data.frame with identifier column prepended and row names removed
