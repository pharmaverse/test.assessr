# Run covr in a Subprocess with a Timeout

This internal helper runs coverage evaluation in a separate R subprocess
using \`callr\`. It allows coverage to be computed with a specified
timeout, ensuring that long‑running or hanging test processes do not
block further analysis.

## Usage

``` r
run_covr(path, timeout)
```

## Arguments

- path:

  Character. Path to the package source directory for which coverage
  should be computed.

- timeout:

  Numeric. Maximum time (in seconds) allowed for the coverage
  computation. Defaults to `Inf`, meaning no timeout is applied unless
  explicitly set.

## Value

A coverage object returned by the underlying covr call, or `NULL` if the
subprocess execution fails or times out.

## Details

The function launches a safe R session via
[`callr::r_safe()`](https://callr.r-lib.org/reference/r.html) and
executes the relevant covr routines within that isolated environment.
This is used internally by higher‑level STF/NSTF coverage functions to
ensure reproducible and controlled coverage execution.
