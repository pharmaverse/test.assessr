## Test environments

-   local Windows 11 x64 (build 26100) R version 4.4.1 (2024-06-14 ucrt)
-   local CentOS Linux 7 (Core) R version 4.2.1 (2022-06-23)
-   macOS Sonoma 14.7.5 (on GitHub Actions), R version 4.5.0 (2025-04-11)
-   Windows Server 2022 x64 (build 20348) (on GitHub Actions), R version 4.5.0 (2025-04-11 ucrt)
-   Ubuntu 24.04.2 LTS (on GitHub Actions), R Under development (unstable) (2025-05-11 r88197)
-   Ubuntu 24.04.2 LTS (on GitHub Actions), R version 4.5.0 (2025-04-11)
-   Ubuntu 24.04.2 LTS (on GitHub Actions), R version 4.4.3 (2025-02-28)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

The rationale of the `test.assessr` package is to provide unti test coverage for R packages. As such, it runs processes
such as `test coverage` which may generate ERRORs, NOTEs, and WARNINGs in packages.
The tests in `test.assessr` produce the following outputs in the test process.

`test.assessr::run_coverage` produces these:

* **NOTE message** running code coverage for test.package.0001
Total coverage returned NaN. This likely means the package had non-standard characteristics.
R coverage for test.package.0001 failed. Read in the covr output to see what went wrong:

* **Cause**: This note occurs due to the mocks testing for empty tests.

running code coverage for test.package.0002
Total coverage returned NaN. This likely means the package had non-standard characteristics.
R coverage for test.package.0002 failed. Read in the covr output to see what went wrong: 

* **Cause**: This note occurs due to the mocks testing for NA total coverage.

running code coverage for test.package.0003
R coverage for test.package.0003 failed. Read in the covr output to see what went wrong: 

* **Cause**: This note occurs due to test coverage unsuccessful for the test package.

* **NOTE message** running code coverage for test.package.0004
code coverage for test.package.0004 successful
R coverage for test.package.0004 had notes: no testable functions found 

* **Cause**: This note occurs due to the mocks testing for no testable functions.

* **NOTE message** Running code coverage for tempPkg2fd8b5db4fbf0
R coverage for tempPkg2fd8b5db4fbf0 failed. Read in the covr output to see what went wrong: 

* **Cause**: This note occurs due to the mocks testing for run_coverage dependency errors.

`test.assessr::install_package_local` produces this:

* **NOTE message** installing non locally
installing invalidPackage locally
installing mock locally
mock_package installed locally
mock installed locally
installing non locally
No such file or directory: non_existent_directory

* **Cause**: This note occurs due to the mocks testing for
mocked package and non-existent directory. 

`test.assessr::run_coverage` produces these:

* **NOTE message** running code coverage for failing.pkg
R coverage for failing.pkg failed. Read in the covr output to see what went wrong: 
  
* **Cause**: This note occurs due to the mocks testing for package with failing tests.   

* **NOTE message** running code coverage for weird.pkg
R coverage for weird.pkg failed. Read in the covr output to see what went wrong: 
  
* **Cause**: This note occurs due to the mocks testing for package with failing tests.   

`test.assessr::run_covr_modes` produces these:

* **NOTE message** running standard testing framework
running skip testing with non-standard testing framework
Package fakepkg is not installed.

* **Cause**: This note occurs due to the mocks testing for package that is not installed. 

* **NOTE message** running non-standard testing framework
No recognised standard or non-standard testing configuration
No recognised standard or non-standard testing configuration
No recognised standard or non-standard testing configuration
Unloading mockpkg package to avoid coverage conflict...
Details: Simulated unload failure
No recognised standard or non-standard testing configuration

* **Cause**: These notes occur due to the mocks testing for non recognised testing framework
and simulated package unload failure

* **WARNING and NOTE messages** Unloading mockpkg package to avoid coverage conflict...
Warning: Failed to unload mockpkg package. Proceeding anyway.
No recognised standard or non-standard testing configuration
Unloading mockpkg package to avoid coverage conflict...
Details: mock error
Unloading mockpkg package to avoid coverage conflict...
Warning: Failed to unload mockpkg package. Proceeding anyway.

* **Cause**: These warnings occur when the mocks test for unloading mockpkg.
The notes occur due to the mocks testing for a mock error

* **NOTE message** Test message Test message
 
* **Cause**: These notes occur due to the mocks testing for messages emitted

* **NOTE message**  [1] "new_var" Removed new globals:
[1] "temp_data"
[1] "dry_var"

* **Cause**: These notes occur due to the mocks testing if global variables can be added
and deleted successfully
 
* **NOTE message** Installed version of stats (4.4.1) does not satisfy constraint 
Failed to install package: stats - couldn't find package 'stats'
Installed version of utils (4.4.1) does not satisfy constraint 
Failed to install package: utils - couldn't find package 'utils'

* **Cause**: These notes occur due to the mocks testing for failed installations
of stats and utils packages 

* **NOTE message** Failed to load package: pkgX - load error 
Failed to install package: pkgX - couldn't find package 'pkgX'

* **Cause**: This note occurs due to the mocks testing for failed installation
of a mocked package 

* **NOTE messages** Setting up coverage environment for: mockpkg
Special helper file created: tests/testthat/helper_covr_env.R
Loading helpers into coverage environment for: mockpkg
Loading package into coverage environment for: mockpkg
Locating project root for: mockpkg
Loading datasets into coverage environment for: mockpkg
Datasets loaded: data1, data2
Loading dependencies into coverage environment for: mockpkg
Unloading package: mockpkg

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and unloading of a mock package

`test.assessr::run_covr_skip_bioc_nstf` produces these:

* **NOTE message** performing source test mapping for mypkg
[BioC RUnit] preparing package before running tests...
identifying problem tests for mypkg
Datasets loaded: 
running coverage manually for mypkg
Total Coverage for mypkg is: 0.5%
Approx total coverage: (75.00%), counts: tested_functions=1, untested_functions=1, skipped_tests=0
performing source test mapping for mypkg
performing source test mapping for mypkg
[BioC RUnit] preparing package before running tests...

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock bioconductor package 
using the RUnit testing package

`test.assessr::run_covr_skip_nstf` produces these:

* **NOTE message**  performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Datasets loaded: dataset1
running coverage manually for mockpkg
Total Coverage for mockpkg is: 80%
Approx total coverage: (80.00%), counts: tested_files=0, untested_functions=1, skipped_tests=1

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock package with 80% coverage

* **NOTE message**  performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Datasets loaded: dataset1
running coverage manually for mockpkg
Total Coverage for mockpkg is: 100%
Approx total coverage: (100.00%), counts: tested_files=1, untested_functions=0, skipped_tests=0

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock package with 100% coverage

* **NOTE message** performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Datasets loaded: 
running coverage manually for mockpkg
No test coverage for this configuration
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Datasets loaded: 
running coverage manually for mockpkg
Total Coverage for mockpkg is: NA%
Approx total coverage: (NA%), counts: tested_files=1, untested_functions=0, skipped_tests=0

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock package with a problematic test

`test.assessr::run_covr_skip_stf` produces these:

* **NOTE message** performing source test mapping for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Datasets loaded: dataset1
running coverage manually for mockpkg
No test coverage for this configuration

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock package with a test that does not 
fit the specified configuration

* **NOTE message** Creating Default test reporter for mockpkg
Mapping package test structure for mockpkg
Creating test results for no skipped tests for mockpkg
Setting up coverage environment for : tests/testthat
Setting up coverage environment for: tests/testthat
Failed to create special helper file: invalid 'description' argument
Loading helpers into coverage environment for: tests/testthat
Error during setup for tests/testthat : invalid 'path' argument
Project root not found.
No helper files were loaded.
Identifying test lines to skip for : tests/testthat
Preparing test files to check for : tests/testthat
Running environment coverage for tests/testthat
Removing new globals from environment for tests/testthat
No new globals detected.
Cleaning up environment for tests/testthat
Project root not found.

* **Cause**: These notes occur due to the mocks testing for a successful loading
and testing of a mock package with a test that does not 
have helper files and test files set up correctly, no project root, cleaning up 
the testing environment

* **NOTE message**  Loading datasets used in testing: iris, mtcars
Running coverage manually for mockpkg

* **Cause**: These notes occur due to testing package mockpkg not having these datasets
 
* **NOTE message** Creating test reporter for: mockpkg
Creating test reporter for: mockpkg
Error running tests for mockpkg : Simulated error  
 
* **Cause**: These notes occur due to testing package mockpkg having a simulated error. 

`test.assessr::unpack_tarball` produces these:

* **message** 
unpacking empty.tar.gz locally
not able to unpack empty.tar.gz locally

* **Cause**: This test is testing if the code can handle an empty tar file.

## skipped tests on Windows

* **Cause**: Skipped on Windows: this test exercises
  POSIX-specific behaviour where R_TESTS
  must be set to '' then restored. Windows
  environment handling (cmd.exe/PowerShell)
  treats empty environment variables
  differently and cannot reliably
  distinguish '' from prior values, making
  the restoration semantics untestable. (1):
  'test-run_covr_skip_bioc_nstf.R:800:3'
* **Cause**: Skipped on Windows: this test exercises
  the POSIX-only behaviour of
  prepare_for_bioc_runit where R_TESTS is
  set to '' and restored to UNSET. Windows
  does not reliably distinguish empty vs
  UNSET environment variables. (1):
  'test-run_covr_skip_bioc_nstf.R:763:3'

## dontrun example

These functions are set as `/dontrun` as they require an API call or web request
or the user is allowed to choose a local input path or local output path:

- generate_test_report
- get_package_coverage
- run_covr_modes
- run_coverage
- run_covr_skip_bioc_nstf
- run_covr_skip_bioc_nstf
- run_covr_skip_nstf
- run_covr_skip_stf
- set_up_pkg
- install_package_local

## Additional comments
* I have ensured that the package passes all checks on different platforms.
* The package has been tested with the latest versions of its dependencies.
* I have verified that the package works correctly with the latest version of R.
* I have included a NEWS.md file to document the changes in this version.

Thank you for considering my submission.