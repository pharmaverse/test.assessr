pkgname <- "test.assessr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('test.assessr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("generate_test_report")
### * generate_test_report

flush(stderr()); flush(stdout())

### Name: generate_test_report
### Title: Generate HTML Report for Package Test Assessment
### Aliases: generate_test_report

### ** Examples

## Not run: 
##D test_results <- get_package_coverage()
##D # Always provide a directory; tempdir() is convenient in examples.
##D generate_test_report(test_results, output_dir = tempdir())
## End(Not run)



cleanEx()
nameEx("get_package_coverage")
### * get_package_coverage

flush(stderr()); flush(stdout())

### Name: get_package_coverage
### Title: get package test coverage
### Aliases: get_package_coverage

### ** Examples




cleanEx()
nameEx("get_pkg_name")
### * get_pkg_name

flush(stderr()); flush(stdout())

### Name: get_pkg_name
### Title: get package name for display
### Aliases: get_pkg_name

### ** Examples





cleanEx()
nameEx("install_package_local")
### * install_package_local

flush(stderr()); flush(stdout())

### Name: install_package_local
### Title: Install a Package from a Local Source Directory
### Aliases: install_package_local

### ** Examples

## Not run: 
##D results <- install_package_local("pkg_source_path")
##D print(results)
## End(Not run)



cleanEx()
nameEx("run_coverage")
### * run_coverage

flush(stderr()); flush(stdout())

### Name: run_coverage
### Title: Run Coverage and Return Structured Coverage Results
### Aliases: run_coverage

### ** Examples





cleanEx()
nameEx("run_covr_modes")
### * run_covr_modes

flush(stderr()); flush(stdout())

### Name: run_covr_modes
### Title: Run Coverage Analysis with Test Detection
### Aliases: run_covr_modes

### ** Examples





cleanEx()
nameEx("set_up_pkg")
### * set_up_pkg

flush(stderr()); flush(stdout())

### Name: set_up_pkg
### Title: Create Information on Local Package Installation
### Aliases: set_up_pkg

### ** Examples

## Not run: 
##D set_up_pkg(path/to/package, "mypackage")
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
