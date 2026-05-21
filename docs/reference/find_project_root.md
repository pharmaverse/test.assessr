# Find R project root directory

Searches upward from a starting path to locate the root of an R package
or project.

## Usage

``` r
find_project_root(start = getwd())
```

## Arguments

- start:

  character, starting path (e.g., test directory or script path)

## Value

character path to project root, or NULL if not found
