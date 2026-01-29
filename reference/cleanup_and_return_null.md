# Clean up global environment and return NULL

This internal utility function is used to clean up the global
environment by removing all objects and optionally displaying a message
before returning \`NULL\`. It is primarily used within
\`run_coverage_base()\` to ensure consistent cleanup in error-handling
scenarios.

## Usage

``` r
cleanup_and_return_null(msg = NULL, env = cov_env)
```

## Arguments

- msg:

  Optional character string. If provided, the message will be printed
  before cleanup.

- env:

  A character string specifying the covr env

## Value

NULL (invisibly)
