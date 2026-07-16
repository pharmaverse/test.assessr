# test.assessr 2.1.3

## New Features

- fix CRAN issue with `test-run_covr_skip_stf`
- increase test coverage for `test-map_tests_stf`, `test-unittest_methods`
- fix handling with test skip and setup
- update processing for `Rcpp` package

# test.assessr 2.1.2

## New Features

- upgrade covr version
- update docs for transition to pharmaverse
- update docs for parameters

# test.assessr 2.1.1

## New Features

- update unpack_tarball
- add testit dependency

# test.assessr 2.1.0

## New Features

- get_package_coverage - has new parameter (package_installed) and stores test_pkg_data in covr_list
- install_package_local - 1st test ("install_package_local works correctly") has skip_on_cran

# test.assessr 2.0.0

## New Features

- improved defensive handling of packages with standard testing frameworks
- improved defensive handling of packages with testit frameworks
- able to handle non-standard testing frameworks: tinytest, RUnit-only
- able to process multiple testing frameworks

# test.assessr 1.1.1

## New Features

- this version fixed issues found in the CRAN check including:

  - run_covr_modes - sets up the coverage env to avoid writing by default 
    in the user's home filespace
  - run_covr_skip_stf.R - uses the coverage env to avoid writing in the user's 
    home filespace
  - run_covr_skip_nstf.R - uses the coverage env to avoid writing in the user's 
    home filespace
  - cleanup_and_return_null - cleans up the coverage env  

# test.assessr 1.1.0

## New Features

- this version fixed issues found in the CRAN check including:

  - removed unnecessary spaces in description field
  - fixed missing Rd-tags and reorganzing family documentation
  - check_pkg_tests_and_snaps() now an exported function
  - changed \dontrun{} to \donttest{} in examples
  - ensured that functions do not write in the user's home filespace
  - get_package_coverage() - resets wd and options() inside function
  - changed from .GlobalEnv. to baseenv
  - removed install packages from functions, examples and vignette
  - removed installed.packages()

# test.assessr 1.0.0

First package version 

Package features include:

 - Run test coverage for: 
     - standard testing frameworks (STF) - tests located in `testthat`, `testit`,
     - non-standard testing frameworks (NSTF) - tests located in `inst` or
       sub-folders and/or use the `RUnit` testing package.
       
 -  exclude tests with errors, warnings, skips and give approximate coverage.       
     
 - report on STF and NSTF test coverage.        
 
inspired by `covrpage` and `mpn.scorecard`.


















  