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


















  