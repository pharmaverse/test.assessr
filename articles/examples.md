# Examples

Run package coverage for a package

If the following code was run,

dp \<- system.file(“test-data”, “test.package.0001_0.1.0.tar.gz”,
package = “test.assessr”)

test_package \<- get_package_coverage(dp)

The result would be:

    test_package
    $pkg_name
    [1] "test.package.0001"

    $pkg_ver
    [1] "0.1.0"

    $date_time
    [1] "2026-02-16 09:35:06.964653"

    $executor
    [1] "u1004798"

    $sysname
    [1] "Linux"

    $version
    [1] "#1 SMP PREEMPT_DYNAMIC Mon Dec  1 05:36:50 UTC 2025"

    $release
    [1] "6.1.158-180.294.amzn2023.x86_64"

    $machine
    [1] "x86_64"

    $r_version
    [1] "4.4.1"

    $test_framework_type
    [1] "standard testing framework"

    $total_cov
    [1] 1


    $res_cov
    $res_cov$name
    [1] "test.package.0001"

    $res_cov$coverage
    $res_cov$coverage$filecoverage
    R/myscript.R 
             100 

    $res_cov$coverage$totalcoverage
    [1] 100


    $res_cov$errors
    [1] NA

    $res_cov$notes
    [1] NA
