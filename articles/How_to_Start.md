# How to Start

Run package coverage for a package

If the following code was run,

``` r

dp <- system.file("test-data", "test.package.0001_0.1.0.tar.gz",
                 package = "test.assessr")


test_package <- get_package_coverage(dp)
```

The result would be:

| R object | field | element 1 | element 2 | value |
|----|----|----|----|----|
| test_package | pkg_name |  |  | “test.package.0001” |
|  | pkg_ver |  |  | “0.1.0” |
|  | date_time |  |  | “2026-03-09 14:27:36.673915” |
|  | executor |  |  | “xxxxxxx” |
|  | sysname |  |  | “Linux” |
|  | version |  |  | “#1 SMP PREEMPT_DYNAMIC Mon Dec 1 05:36:50 UTC 2025” |
|  | release |  |  | “6.1.158-180.294.amzn2023.x86_64” |
|  | machine |  |  | “x86_64” |
|  | r_version |  |  | “4.4.1” |
|  | test_framework_type |  |  | standard testing framework |
|  | total_cov |  |  | 1 |
|  | res_cov | name |  | “test.package.0001” |
|  |  | coverage | filecoverage | R/myscript.R 100 |
|  |  |  | totalcoverage | 100 |
|  | errors |  |  | NA |
|  | notes |  |  | NA |
