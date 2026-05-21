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

The rationale of the `test.assessr` package is to provide unit test coverage for R packages. As such, it runs processes
such as `test coverage` which may generate ERRORs, NOTEs, and WARNINGs in packages.
The tests in `test.assessr` produce the following outputs in the test process.

`test.assessr::core_test_functions` produces these:

* **NOTE message** running code coverage for test.package.0001
running code coverage for test.package.0001
code coverage for test.package.0001 successful

* **Cause**: This note occurs due to successful code coverage run.

* **NOTE message** running code coverage for test.package.0001
Total coverage returned NaN. This likely means the package had non-standard characteristics.
R coverage for test.package.0001 failed. Read in the covr output to see what went wrong:

* **Cause**: This note occurs due to unsuccessful code coverage run.

* **NOTE message** running code coverage for test.package.0001
Total coverage returned NaN. This likely means the package had non-standard characteristics.
R coverage for test.package.0001 failed. Read in the covr output to see what went wrong:

* **Cause**: This note occurs due to unsuccessful code coverage run.

* **NOTE message** running code coverage for test.package.0002
running code coverage for test.package.0002
Total coverage returned NaN. This likely means the package had non-standard characteristics.
R coverage for test.package.0002 failed. Read in the covr output to see what went wrong: 

* **Cause**: This note occurs due to unsuccessful code coverage run.

* **NOTE message** running code coverage for test.package.0003
code coverage for test.package.0003 unsuccessful
running code coverage for test.package.0003
R coverage for test.package.0003 failed. Read in the covr output to see what went wrong: 

* **Cause**: This note occurs due to unsuccessful code coverage run.

* **NOTE message** running code coverage for test.package.0004
code coverage for test.package.0004 unsuccessful
R coverage for test.package.0004 failed. Read in the covr output to see what went wrong: 

* **Cause**: This note occurs due to unsuccessful code coverage run.

* **NOTE message** R coverage for test.package.0004 had notes: no testable functions found
running code coverage for test.package.0004
R coverage for test.package.0004 had notes: no testable functions found

* **Cause**: This note occurs due to unsuccessful code coverage run 
with no testable functions.

* **NOTE message** running code coverage for tempPkg685c4c7b7415
code coverage for tempPkg685c4c7b7415 unsuccessful
running code coverage for tempPkg685c4c7b7415
R coverage for tempPkg685c4c7b7415 failed. Read in the covr output to see what went wrong:

* **Cause**: This note occurs due to unsuccessful code coverage run.

`test.assessr::get_package_coverage` produces these:

* **NOTE messages** unpacking test.package.0001 locally
unpacked test.package.0001 locally
installing test.package.0001 locally
test.package.0001 is already installed
No safe roots found for orphan cleanup; skipping.
No safe roots found for orphan cleanup; skipping. 

* **Cause**: These notes occur due to the mocks testing for unpacking,
installing, checking for package installation, and skipping cleanup.

`test.assessr::run_coverage` produces these:

* **NOTE message** running code coverage for test.pkg
running code coverage for empty.pkg
code coverage for empty.pkg successful

* **Cause**: This note occurs due to the mocks testing for empty tests.

* **NOTE message** running code coverage for failing.pkg
R coverage for failing.pkg failed. Read in the covr output to see what went wrong: 

* **Cause**: This note occurs due to the mocks testing for failing tests.

* **NOTE message** code coverage for weird.pkg unsuccessful
R coverage for weird.pkg failed. Read in the covr output to see what went wrong:

* **Cause**: This note occurs due to the mocks testing for non-standard tests.

`test.assessr::install_package_local` produces this:

* **NOTE messages** packing file3502104167b6.tar.gz locally
unpacked file3502104167b6.tar.gz locally
installing test.package.0001 locally
test.package.0001 is already installed
installing invalidPackage locally
installing mock locally
mock_package installed locally
mock installed locally
installing non locally
No such file or directory: non_existent_directory
installing mock locally
mock is already installed

* **Cause**: These notes occur due to the mocks testing for
mocked packages that are invalid and valid as well as non-existent directory. 

`test.assessr:::load_dependencies_into_dev` produces this:

* **NOTE message** load_dependencies_into_env                                                   Installing package into '/home/u1004798/R/x86_64-pc-linux-gnu-library/4.4'
(as 'lib' is unspecified)
Installing package into '/home/u1004798/R/x86_64-pc-linux-gnu-library/4.4'
(as 'lib' is unspecified)
Installing package into '/home/u1004798/R/x86_64-pc-linux-gnu-library/4.4'
(as 'lib' is unspecified)

* **Cause**: This note occurs due to the mocks simulating loading dependencies
into an env that is not global. 

`test.assessr:::load_dependencies_into_dev` produces this:

* **NOTE message** load_dependencies_into_env                                                   Installing package into '/home/u1004798/R/x86_64-pc-linux-gnu-library/4.4'
(as 'lib' is unspecified)
Installing package into '/home/u1004798/R/x86_64-pc-linux-gnu-library/4.4'
(as 'lib' is unspecified)
Installing package into '/home/u1004798/R/x86_64-pc-linux-gnu-library/4.4'
(as 'lib' is unspecified)

* **Cause**: This note occurs due to the mocks simulating loading dependencies
into an env that is not global. 


`test.assessr::run_covr_modes` produces these:

* **NOTE messages** Cleaning coverage workspace for mockpkg
No safe roots found for orphan cleanup; skipping.
Package mockpkg is not installed; skipping installed datasets.
Package fakepkg is not installed; skipping installed datasets.
No safe roots found for orphan cleanup; skipping.

* **Cause**: These notes occur due to the mocks testing for packages that are not installed. 

* **NOTE message** decider: testthat -> run_coverage()
Cleaning coverage workspace for mockpkg
running non-standard Bioconductor testing framework
Cleaning coverage workspace for mockpkg
No safe roots found for orphan cleanup; skipping.

* **Cause**: These notes occur due to the mocks testing for a Bioconductor package. 

* **NOTE messages** decider: testthat -> run_coverage()
running skip testing with standard testing framework
Cleaning coverage workspace for mockpkg
running non-standard testing framework
Cleaning coverage workspace for mockpkg
No safe roots found for orphan cleanup; skipping.
No recognised standard or non-standard testing configuration
No recognised standard or non-standard testing configuration

* **Cause** These notes occur due to the mocks testing for a package
with no recognised testing framework

* **WARNING and NOTE messages** Unloading mockpkg package to avoid coverage conflict...
Warning: Failed to unload mockpkg package. Proceeding anyway.
No recognised standard or non-standard testing configuration
Unloading mockpkg package to avoid coverage conflict...
Details: mock error
Unloading mockpkg package to avoid coverage conflict...
Warning: Failed to unload mockpkg package. Proceeding anyway.

* **Cause**: These warnings occur when the mocks test for unloading mockpkg.
The notes occur due to the mocks testing for a mock error

* **NOTE message**  [1] "new_var" Removed new globals:
[1] "temp_data"
[1] "dry_var"

* **Cause**: These notes occur due to the mocks testing if global variables can be added
and deleted successfully
 

* **NOTE messages** Setting up coverage environment for: mockpkg
Special helper file created: tests/testthat/helper_covr_env.R
Loading helpers into coverage environment for: mockpkg
Loading package into coverage environment for: mockpkg
Locating project root for: mockpkg
Loading datasets into coverage environment for: mockpkg
Loading package datasets into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Loading optional nycflights13 datasets into coverage environment
Datasets loaded: data1, data2, datasets::mtcars, datasets::iris, flights
Base R exports loaded: 2
Loading dependencies into coverage environment for: mockpkg
Unloading package: mockpkg

* **Cause**: These notes occur due to the mocks testing for loading helpers, locating package root, loading base R and package datasets,
loading Base R packages, loading dependencies, and 
unloading a package

`test.assessr:::run_covr_skip_bioc_nstf` produces these:

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

`test.assessr:::run_covr_skip_dt_nstf` produces these:

* **NOTE message** data.table: MIRROR mode -> copying inst/tests/* -> tests/ to satisfy test.data.table()
data.table: tests/main.R not found; will rely on wrapper selection.
data.table: tests/main.R not found; will rely on wrapper selection.
data.table: tests/main.R not found; will rely on wrapper selection.
loading package and datasets for data.table
running coverage manually for data.table
Total Coverage for data.table is: 97.82%
Approx total coverage: (97.82%), counts: tested_files=1, untested_functions=39, skipped_tests=2
loading package and datasets for data.table
running coverage manually for data.table

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock data.table package 

`test.assessr:::run_covr_skip_nstf` produces these:

* **NOTE messages**  performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: dataset1
running coverage manually for mockpkg

* **Cause**: These notes occur due to the mocks testing for a successful loading, setting up, and manual testing of a mock package 

* **NOTE messages** performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: dataset1
running coverage manually for mockpkg

* **Cause**: These notes occur due to the mocks testing for a successful loading, setting up, and manual testing of a mock package

* **NOTE messages** performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: 
running coverage manually for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Error loading package or datasets for mockpkg : DESCRIPTION file not found at: /tmp/Rtmph2m5dg/mockpkg/DESCRIPTION
No safe roots found for orphan cleanup; skipping.
running coverage manually for mockpkg
No env coverage generated. Falling back to covr::package_coverage(type = 'tests').
package_coverage() failed: '/tmp/Rtmph2m5dg/mockpkg' does not contain a package!
No safe roots found for orphan cleanup; skipping.
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Error loading package or datasets for mockpkg : DESCRIPTION file not found at: /tmp/Rtmph2m5dg/mockpkg/DESCRIPTION
No safe roots found for orphan cleanup; skipping.
running coverage manually for mockpkg
No env coverage generated. Falling back to covr::package_coverage(type = 'tests').
No safe roots found for orphan cleanup; skipping.

* **Cause**: These notes occur due to the mocks testing for a successful loading, setting up, and testing of a mock package
without a DESCRIPTION file.

* **NOTE messages**  performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Datasets loaded: dataset1
running coverage manually for mockpkg
Total Coverage for mockpkg is: 80%
Approx total coverage: (80.00%), counts: tested_files=0, untested_functions=1, skipped_tests=1
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: dataset1
running tinytest coverage for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: 
running tinytest coverage for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: 
running tinytest coverage for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: 
running tinytest coverage for mockpkg
performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment

* **Cause**: These notes occur due to the mocks testing for a successful 
loading, setting up, testing of a mock package with tinytest testing

* **NOTE messages**  performing source test mapping for mockpkg
identifying problem tests for mockpkg
loading package and datasets for mockpkg
Loading dependencies into coverage environment for: mockpkg
Loading Base R packages into coverage environment
Datasets loaded: 
running coverage manually for mockpkg
Approx total coverage: (72.35%), counts: tested_files=4, untested_functions=1, skipped_tests=2

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock package with 72.35% coverage

* **NOTE message** Total Coverage for mockpkg is: 0%
Approx total coverage: (0.00%), counts: tested_files=1, untested_functions=1, skipped_tests=0

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock package with 0.0% coverage

`test.assessr:::run_covr_skip_runit_nstf` produces these:

* **NOTE messages** performing source test mapping for mypkg
[RUnit] preparing package before running tests...
identifying problem tests for mypkg
Datasets loaded: 
running coverage manually for mypkg
Using custom runner for mypkg (1 test files)
Total Coverage for mypkg is: 0.5%
Approx total coverage: (75.00%), counts: tested_functions=1, untested_functions=1, skipped_tests=0
No safe roots found for orphan cleanup; skipping.

* **Cause**: These notes occur due to the mocks testing for a successful loading,
setting up, and testing of a mock package with a RUnit framework and 
75% coverage

`test.assessr:::run_covr_skip_stf` produces these:

* **NOTE message** Setting up coverage environment for : mockpkg
No helper files were loaded.
Identifying test lines to skip for : mockpkg
Preparing test files to check for : mockpkg
Running environment coverage for mockpkg
Removing new globals from environment for mockpkg
No new globals detected.
Cleaning up environment for mockpkg
No safe roots found for orphan cleanup; skipping.

* **Cause**: These notes occur due to the mocks testing for a successful loading and identifying test lines and testing of a mock package, cleaning up 
the testing environment

* **NOTE message**  Loading datasets used in testing: iris, mtcars
Running coverage manually for mockpkg

* **Cause**: These notes occur due to testing package mockpkg not having these datasets
 
* **NOTE message** Creating package coverage for fakepkg
Creating package coverage for mockpkg
No test coverage for this configuration
Creating test reporter for: mockpkg
Creating test reporter for: mockpkg
Error running tests for mockpkg : Simulated error 
 
* **Cause**: These notes occur due to testing package mockpkg having a simulated error. 

* **NOTE messages** Setting up coverage environment for : dummyPkg
Setting up coverage environment for : dummyPkg
No helper files were loaded.
Identifying test lines to skip for : dummyPkg
Preparing test files to check for : dummyPkg
Running environment coverage for dummyPkg
Removing new globals from environment for dummyPkg
Cleaning up environment for dummyPkg

* **Cause**: These notes occur due to testing package dummyPkg for cleaning up the env. 

`test.assessr::set_up_pkg` produces these:

* **message** unpacking file9c943148a756.tar.gz locally
unpacked file9c943148a756.tar.gz locally

* **Cause**: This test is testing if the code can handle unpacking a tar file 
with mocked data.

`test.assessr::check_pkg_and_snaps` produces these:

* **messages** checking package test config
checking package test config
checking package test config
checking package test config
checking package test config

* **Cause**: This test is testing if the code can handle checking the test 
configuration with mocked data.

`test.assessr::unpack_tarball` produces these:

* **message**  unpacking file9c944797920f.tar.gz locally
unpacked file9c944797920f.tar.gz locally
unpacking file9c947e0929f6.tar.gz locally
not able to unpack file9c947e0929f6.tar.gz locally

* **Cause**: This test is testing if the code can handle unpacking a tar file 
and handle an empty tar file with mocked data.

## skipped tests on Windows

* **Cause**: 
* Could not make directory non-writable on this
  platform; skipping. (1):
  'test-run_covr_skip_stf.R:1346:5'
* On CRAN (2): 'test-run_covr_skip_stf.R:222:3',
  'test-run_covr_skip_stf.R:334:3'
* Skipped on Windows: this test exercises
  POSIX-specific behaviour where R_TESTS must be
  set to '' then restored. Windows environment
  handling (cmd.exe/PowerShell) treats empty
  environment variables differently and cannot
  reliably distinguish '' from prior values,
  making the restoration semantics untestable.
  (1): 'test-run_covr_skip_runit_nstf.R:811:3'
* Skipped on Windows: this test exercises
  POSIX-specific behaviour where R_TESTS must be
  set to '' then restored. Windows environment
  handling cannot reliably distinguish empty vs
  unset values. (1):
  'test-run_covr_skip_bioc_nstf.R:568:3'
* Skipped on Windows: this test exercises the
  POSIX-only behaviour of prepare_for_bioc_runit
  where R_TESTS is set to '' and restored to
  UNSET. Windows does not reliably distinguish
  empty vs UNSET environment variables. (2):
  'test-run_covr_skip_bioc_nstf.R:524:3',
  'test-run_covr_skip_runit_nstf.R:774:3'

## dontrun example

These functions are set as `\dontrun` as they require a local input path
or the user is allowed to choose a local input path or local output path:

- generate_test_report
- set_up_pkg
- install_package_local

## Additional comments
* I have ensured that the package passes all checks on different platforms.
* The package has been tested with the latest versions of its dependencies.
* I have verified that the package works correctly with the latest version of R.
* I have included a NEWS.md file to document the changes in this version.

Thank you for considering my submission.
