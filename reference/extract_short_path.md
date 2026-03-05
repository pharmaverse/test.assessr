# Extract the Last Two Path Components from a File Path

This helper function takes any full file path and extracts only the last
two components, such as \`"R/add.R"\`. It supports both forward slashes
(\`/\`) and backslashes (\`\\) to accommodate Windows, macOS, and Linux
paths.

## Usage

``` r
extract_short_path(long_file_name)
```

## Arguments

- long_file_name:

  Character string. A full file path using \`/\` or \`\\ as separators.

## Value

A character scalar containing the last two components of the path joined
with a forward slash—for example \`"R/add.R"\`—or a single component if
only one exists. The returned value is always of class `character`.

## Details

Trailing separators are preserved where meaningful (e.g., \`"R/"\` when
the input ends with a slash). Empty path components are removed except
when required to detect a trailing separator.

## Output Meaning

This function is intended for display and logging purposes, where only
the tail portion of a full file path is meaningful. It does not check
for file existence; it simply processes the string supplied by the user.

## Examples

``` r
extract_short_path("pkg/R/add.R")
#> [1] "R/add.R"
extract_short_path("C:\\\\projects\\\\mypkg\\\\R\\\\helper.R")
#> [1] "R/helper.R"
```
