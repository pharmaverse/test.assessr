# Remove New Globals from Environment Based on Initial Snapshot

This function removes objects from an environment that were added after
a recorded initial state. Optionally filters by pattern.

## Usage

``` r
remove_new_globals(env, initial_state, patterns = NULL, dry_run = FALSE)
```

## Arguments

- env:

  Environment to clean.

- initial_state:

  Character vector of object names before setup.

- patterns:

  Optional regex patterns to filter objects.

- dry_run:

  Logical. If TRUE, only prints what would be removed.

## Value

Invisibly returns a character vector of removed object names.
