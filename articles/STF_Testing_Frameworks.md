# STF Testing Frameworks

The `test.assessr` package checks package structure and test drivers to
detect and classify testing configurations.

If it detects a standard testing framework (STF) (i.e. `testthat` or
`testit`), it runs a standard testing workflow involving a number of
steps:

`Step 1`

- run [`callr::r_safe`](https://callr.r-lib.org/reference/r.html) which
  calls
  [`covr::package_coverage`](http://covr.r-lib.org/reference/package_coverage.md).

- The `r_safe` function has the parameter `error = "error"`.

- This means if `package_coverage` encounters an error, then `r_safe`
  returns an error to the main testing process.

- If `Step 1` is successful, then a unit test data coverage object like
  this is produced:

`Step 1 unit test data coverage object`

| R object     | field               | element 1 | element 2     | value                                                |
|--------------|---------------------|-----------|---------------|------------------------------------------------------|
| test_package | pkg_name            |           |               | “test.package.0001”                                  |
|              | pkg_ver             |           |               | “0.1.0”                                              |
|              | date_time           |           |               | “2026-03-09 14:27:36.673915”                         |
|              | executor            |           |               | “xxxxxxx”                                            |
|              | sysname             |           |               | “Linux”                                              |
|              | version             |           |               | “#1 SMP PREEMPT_DYNAMIC Mon Dec 1 05:36:50 UTC 2025” |
|              | release             |           |               | “6.1.158-180.294.amzn2023.x86_64”                    |
|              | machine             |           |               | “x86_64”                                             |
|              | r_version           |           |               | “4.4.1”                                              |
|              | test_framework_type |           |               | standard testing framework                           |
|              | total_cov           |           |               | 1                                                    |
|              | res_cov             | name      |               | “test.package.0001”                                  |
|              |                     | coverage  | filecoverage  | R/myscript.R 100                                     |
|              |                     |           | totalcoverage | 100                                                  |
|              | errors              |           |               | NA                                                   |
|              | notes               |           |               | NA                                                   |

`Step 2`

- run `setup_covr_env` which

  - loads helper files from `tests/testthat`,
  - loads package namespace objects into `cov_env`,
  - resolves the project root,
  - loads package datasets into `cov_env`,
  - loads Base R datasets and exports into `cov_env`,
  - optionally loads `nycflights13` datasets if available,
  - loads dependencies into `cov_env`, and
  - unloads the package namespace afterward to reduce conflicts.

- run `create fail reporter` to find failing tests

- run `create test reporter` to create a reporter for all tests

- map `exported functions` to test files

- create a filter for test status i.e. tests that have
  `"FAILED", "ERROR", "SKIPPED"`

- identify tests to be skipped based on status

- comment out tests to be skipped

- run
  [`covr::environment_coverage`](http://covr.r-lib.org/reference/environment_coverage.md)
  on remaining test files.

- If `Step 2` is successful, then a unit test data coverage object like
  this is produced:

`Step 2 unit test data coverage object`

| R object     | field               | element 1 | element 2     | value                                                |
|--------------|---------------------|-----------|---------------|------------------------------------------------------|
| test_package | pkg_name            |           |               | “test.package.0003”                                  |
|              | pkg_ver             |           |               | “0.1.0”                                              |
|              | date_time           |           |               | “2026-04-09 10:17:27.71564”                          |
|              | executor            |           |               | “xxxxxxx”                                            |
|              | sysname             |           |               | “Linux”                                              |
|              | version             |           |               | “#1 SMP PREEMPT_DYNAMIC Mon Dec 1 05:36:50 UTC 2025” |
|              | release             |           |               | “6.1.158-180.294.amzn2023.x86_64”                    |
|              | machine             |           |               | “x86_64”                                             |
|              | r_version           |           |               | “4.4.1”                                              |
|              | test_framework_type |           |               | standard testing framework                           |
|              | total_cov           |           |               | 0                                                    |
|              | res_cov             | name      |               | “test.package.0003”                                  |
|              |                     | coverage  | filecoverage  | num \[1(1d)\] 0                                      |
|              |                     |           |               | attr(\*, “dimnames”)=List of 1 chr “myscript.R”      |
|              |                     |           | totalcoverage | 0                                                    |
|              | errors              |           |               | NA                                                   |
|              | notes               |           |               | NA                                                   |
| long_summary | file                |           |               | “test-myscript.R#L2_L2”                              |
|              | code_script         |           |               | “myscript”                                           |
|              | context             |           |               | “this works”                                         |
|              | status              |           |               | “FAILED”                                             |
|              | n                   |           |               | 1                                                    |
|              | time                |           |               | 0.291                                                |
| test_skip    | file                |           |               | “test-myscript.R”                                    |
|              | code_script         |           |               | “myscript”                                           |
|              | context             |           |               | “this works”                                         |
|              | status              |           |               | “FAILED”                                             |
|              | n                   |           |               | 1                                                    |
|              | time                |           |               | 0.291                                                |
|              | expectation:        |           |               | “expect_equal”                                       |
|              | line1               |           |               | 2                                                    |
|              | line2               |           |               | 2                                                    |

This data object differs from `Step 1 data object` in that it gives
information about which tests/test blocks were skipped and the status
tells the user why they were skipped.

`Step 3`

- run
  [`withr::with_dir`](https://withr.r-lib.org/reference/with_dir.html)
  which calls
  [`covr::package_coverage`](http://covr.r-lib.org/reference/package_coverage.md).

- [`covr::package_coverage`](http://covr.r-lib.org/reference/package_coverage.md)
  has the parameters `type = "tests", quiet = TRUE`

- If `Step 3` is successful, then a unit test data coverage object like
  this is produced:

`Step 3 unit test data coverage object`

| R object     | field               | element 1 | element 2     | value                                                                                                               |
|--------------|---------------------|-----------|---------------|---------------------------------------------------------------------------------------------------------------------|
| test_package | pkg_name            |           |               | “dplyr”                                                                                                             |
|              | pkg_ver             |           |               | “1.1.4”                                                                                                             |
|              | date_time           |           |               | “2026-04-09 10:17:27.71564”                                                                                         |
|              | executor            |           |               | “xxxxxxx”                                                                                                           |
|              | sysname             |           |               | “Linux”                                                                                                             |
|              | version             |           |               | “#1 SMP PREEMPT_DYNAMIC Mon Dec 1 05:36:50 UTC 2025”                                                                |
|              | release             |           |               | “6.1.158-180.294.amzn2023.x86_64”                                                                                   |
|              | machine             |           |               | “x86_64”                                                                                                            |
|              | r_version           |           |               | “4.4.1”                                                                                                             |
|              | test_framework_type |           |               | standard testing framework                                                                                          |
|              | total_cov           |           |               | 0                                                                                                                   |
|              | res_cov             | name      |               | “dplyr”                                                                                                             |
|              |                     | coverage  | filecoverage  | num \[1:109(1d)\] 98.3 76.7 97.5 100 100 …                                                                          |
|              |                     |           |               | attr(\*, “dimnames”)=List of 1 \[1:109\] “across.R” “all-equal.R” “arrange.R” “bind-cols.R” …                       |
|              |                     |           | totalcoverage | 0.907                                                                                                               |
|              | errors              |           |               | NA                                                                                                                  |
|              | notes               |           |               | NA                                                                                                                  |
| long_summary | file                |           |               | \[1:3432\] “test-across.R#L7_L7” “test-across.R#L14_L14” “test-across.R#L20_L20” “test-across.R#L24_L24” …          |
|              | code_script         |           |               | \[1:3432\] “across” “across” “across” “across” …                                                                    |
|              | context             |           |               | \[1:3432\] “across() works on one column data.frame” “across() does not select grouping variables” “across() corre” |
|              | status              |           |               | \[1:3432\] “PASS” “PASS” “PASS” “PASS” …                                                                            |
|              | n                   |           |               | \[1:3432\] 1 1 1 1 1 1 1 1 1 1 …                                                                                    |
|              | time                |           |               | \[1:3432\] 0.041 0.014 0.077 0.077 0.077 …                                                                          |
| test_skip    | file                |           |               | \[1:448\] “test-across.R” “test-across.R” “test-across.R” “test-across.R” …                                         |
|              | code_script         |           |               | \[1:448\] “across” “across” “across” “across” …                                                                     |
|              | context             |           |               | “across() gives meaningful messages” “across() gives meaningful messages” “across() gives meaningful mes”           |
|              | status              |           |               | \[1:448\] “FAILED” “FAILED” “FAILED” “FAILED” …                                                                     |
|              | n                   |           |               | \[1:448\] 1 1 1 1 1 1 1 1 1 1 …                                                                                     |
|              | time                |           |               | \[1:448\] 2.03 2.03 2.03 2.03 2.03 …                                                                                |
|              | expectation         |           |               | \[1:448\] “expect_snapshot” “expect_error” “expect_error” “expect_error” …                                          |
|              | line1               |           |               | \[1:448\] 209 212 216 222 226 230 234 238 243 244 …                                                                 |
|              | line2               |           |               | \[1:448\] 280 215 219 225 229 233 237 241 243 244 …                                                                 |

Both `Step 2 unit test data coverage object` and
`Step 3 unit test data coverage object` give the user details about the
status of the tests `FAILED`, `PASSED` and `SKIPPED`.

They also contain the test block start line (i.e. `line1` and `line2`)
and the expectation type in the `expectation` column
(e.g. `expect_snapshot` `expect_error`).
