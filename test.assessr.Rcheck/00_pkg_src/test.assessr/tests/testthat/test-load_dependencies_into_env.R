test_that("fails if DESCRIPTION file is missing", {
  env <- new.env()
  mockery::stub(load_dependencies_into_env, "requireNamespace", TRUE)
  mockery::stub(load_dependencies_into_env, "file.exists", FALSE)
  expect_error(load_dependencies_into_env("fakepkg", env), "DESCRIPTION file not found")
})

test_that("parses and installs dependencies", {
  env <- new.env()
  desc <- matrix(
    c("pkgA (>= 1.0.0), pkgB", "pkgC", NA_character_),
    nrow = 1,
    dimnames = list(NULL, c("Depends", "Imports", "Suggests"))
  )
  
  # --- FS & DESCRIPTION stubs ---
  mockery::stub(load_dependencies_into_env, "file.exists", TRUE)
  mockery::stub(load_dependencies_into_env, "read.dcf", desc)
  
  # --- Availability check (requireNamespace) ---
  # Call order expected:
  #   1) requireNamespace("remotes", quietly = TRUE)
  #   2) requireNamespace("pkgA", quietly = TRUE) -> FALSE (needs install)
  #   3) requireNamespace("pkgB", quietly = TRUE) -> TRUE (already installed)
  #   4) requireNamespace("pkgC", quietly = TRUE) -> FALSE (needs install)
  req_ns_mock <- mockery::mock(TRUE, FALSE, TRUE, FALSE)
  mockery::stub(load_dependencies_into_env, "requireNamespace", req_ns_mock)
  
  # --- packageDescription (Version checks) ---
  # Call order expected:
  #   1) packageDescription("pkgA", fields = "Version") -> "0.9.0" (fails >= 1.0.0)
  pkg_desc_mock <- mockery::mock("0.9.0")
  mockery::stub(load_dependencies_into_env, "utils::packageDescription", pkg_desc_mock)
  
  # --- Installers ---
  # install.packages() should be called for pkgA (fails >= 1.0.0) and for pkgC (missing)
  install_mock <- mockery::mock(invisible(NULL), invisible(NULL))
  mockery::stub(load_dependencies_into_env, "utils::install.packages", install_mock)
  
  # --- loadNamespace() ---
  load_ns_mock <- mockery::mock(invisible(NULL), invisible(NULL), invisible(NULL))
  mockery::stub(load_dependencies_into_env, "loadNamespace", load_ns_mock)
  
  # --- import_namespace_exports() ---
  import_mock <- mockery::mock(invisible(TRUE), invisible(TRUE), invisible(TRUE))
  mockery::stub(load_dependencies_into_env, "import_namespace_exports", import_mock)
  
  # --- Run & assertions ---
  suppressWarnings(result <- load_dependencies_into_env("fakepkg", env))
  expect_equal(result, invisible(TRUE))
  
  # import_namespace_exports() called three times: pkgA, pkgB, pkgC
  mockery::expect_called(import_mock, 3)
  mockery::expect_args(import_mock, 1, "pkgA", env, overwrite = TRUE)
  mockery::expect_args(import_mock, 2, "pkgB", env, overwrite = TRUE)
  mockery::expect_args(import_mock, 3, "pkgC", env, overwrite = TRUE)
})


test_that("parses and installs dependencies from Imports/Suggests (no Depends import)", {
  env <- new.env(parent = baseenv())
  
  # DESCRIPTION with only Imports and Suggests
  desc <- matrix(
    c("pkgA (>= 1.0.0), pkgB",  # Imports
      "pkgC",                   # Suggests
      NA_character_),           # Depends (none)
    nrow = 1,
    dimnames = list(NULL, c("Imports", "Suggests", "Depends"))
  )
  
  # --- FS & DESCRIPTION stubs ---
  mockery::stub(load_dependencies_into_env, "file.exists", TRUE)
  mockery::stub(load_dependencies_into_env, "read.dcf", desc)
  
  # --- remotes availability check ---
  # Only called once at the beginning: requireNamespace("remotes", quietly = TRUE)
  req_ns_mock <- mockery::mock(TRUE)
  mockery::stub(load_dependencies_into_env, "requireNamespace", req_ns_mock)
  
  # --- Core seams for the NEW design ---
  # Do NOT reach out to CRAN; we just assert install_if_needed is called correctly
  installer_mock <- mockery::mock(invisible(TRUE), invisible(TRUE), invisible(TRUE))
  mockery::stub(load_dependencies_into_env, "install_if_needed", installer_mock)
  
  # No Depends => we must NOT import exports into env
  import_mock <- mockery::mock()
  mockery::stub(load_dependencies_into_env, "import_namespace_exports", import_mock)
  
  # --- Run ---
  expect_invisible(load_dependencies_into_env("fakepkg", env))
  
  # --- Assertions ---
  # remotes check once
  mockery::expect_called(req_ns_mock, 1)
  
  # install_if_needed called for each token in order: pkgA, pkgB, pkgC
  mockery::expect_called(installer_mock, 3)
  mockery::expect_args(installer_mock, 1, "pkgA", ">=", "1.0.0")
  mockery::expect_args(installer_mock, 2, "pkgB", NULL, NULL)
  mockery::expect_args(installer_mock, 3, "pkgC", NULL, NULL)
  
})


test_that("imports exports from Depends into env", {
  env <- new.env(parent = baseenv())
  
  desc <- matrix(
    c("grDevices, graphics, stats, utils",  # Depends
      NA_character_,                        # Imports
      NA_character_),                       # Suggests
    nrow = 1,
    dimnames = list(NULL, c("Depends", "Imports", "Suggests"))
  )
  
  mockery::stub(load_dependencies_into_env, "file.exists", TRUE)
  mockery::stub(load_dependencies_into_env, "read.dcf", desc)
  
  # Only remotes check at the start
  req_ns_mock <- mockery::mock(TRUE)
  mockery::stub(load_dependencies_into_env, "requireNamespace", req_ns_mock)
  
  # Avoid network/installation; just assert calls happen for each Depends package
  installer_mock <- mockery::mock(invisible(TRUE), invisible(TRUE), invisible(TRUE), invisible(TRUE))
  mockery::stub(load_dependencies_into_env, "install_if_needed", installer_mock)
  
  # Should be called for each Depends pkg with (pkg, env, overwrite=TRUE, filter=NULL)
  import_mock <- mockery::mock(invisible(TRUE), invisible(TRUE), invisible(TRUE), invisible(TRUE))
  mockery::stub(load_dependencies_into_env, "import_namespace_exports", import_mock)
  
  expect_invisible(load_dependencies_into_env("fakepkg", env))
  
  mockery::expect_called(req_ns_mock, 1)
  mockery::expect_called(installer_mock, 4)
  mockery::expect_called(import_mock, 4)
  
})


test_that("resolve_description_deps returns zero-row data.frame when all tokens are unparseable", {
  # dummy desc object – its structure is irrelevant because we stub get_field()
  desc <- new.env(parent = emptyenv())
  
  # 1) Stub get_field() to return non-empty fields so vals > 0
  #    (ensures the code reaches the res/res_filtered path rather than the earlier vals==0 return)
  mockery::stub(
    resolve_description_deps, 
    "get_field",
    function(d, f) {
      switch(f,
             "Depends"  = "A (>= 1.0), B",
             "Imports"  = "C, D (>= 2.0)",
             "Suggests" = "E",
             ""         # default (unused)
      )
    }
  )
  
  # 2) Stub parse_dep() to always return NULL so every token is dropped
  mockery::stub(
    resolve_description_deps,
    "parse_dep",
    function(x) NULL
  )
  
  # Call the function – it should go through res -> res_filtered -> return zero-row df
  out <- resolve_description_deps(desc, fields = c("Depends", "Imports", "Suggests"), exclude_R = TRUE)
  
  # Assertions: structure and zero rows
  expect_true(is.data.frame(out))
  expect_identical(names(out), c("package", "op", "ver", "field"))
  expect_identical(nrow(out), 0L)
  # Ensure character columns (stringsAsFactors = FALSE)
  expect_true(all(vapply(out, typeof, character(1)) == "character"))
})

test_that("resolve_description_deps filters NULL entries and binds remaining rows", {
  # Make sure an infix %||% exists for the function body
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  desc <- new.env(parent = emptyenv())
  
  # Return a mix of tokens across fields
  mockery::stub(
    resolve_description_deps,
    "get_field",
    function(d, f) {
      switch(f,
             "Depends"  = "A (>= 1.0), B",
             "Imports"  = "R (>= 4.0), C",  # 'R' should be excluded when exclude_R = TRUE
             "Suggests" = "D, E (>= 2.3)",
             ""
      )
    }
  )
  
  # parse_dep returns:
  #  - a proper parsed list for A, C, E
  #  - NULL for B and D (to test filtering via res_filtered line)
  mockery::stub(
    resolve_description_deps,
    "parse_dep",
    function(token) {
      token <- trimws(token)
      if (startsWith(token, "A")) list(pkg = "A", op = ">=", ver = "1.0")
      else if (token == "B")      NULL
      else if (token == "C")      list(pkg = "C", op = NULL, ver = NULL)  # will use %||% -> NA
      else if (token == "D")      NULL
      else if (startsWith(token, "E")) list(pkg = "E", op = ">=", ver = "2.3")
      else NULL
    }
  )
  
  out <- resolve_description_deps(desc, fields = c("Depends", "Imports", "Suggests"), exclude_R = TRUE)
  
  # Should have rows for A, C, E (B and D dropped; R excluded)
  expect_true(is.data.frame(out))
  expect_identical(names(out), c("package", "op", "ver", "field"))
  expect_setequal(out$package, c("A", "C", "E"))
  # Check op/ver handling with %||% -> NA_character_ where missing
  expect_true(is.character(out$op) && is.character(out$ver) && is.character(out$field))
  # Confirm that exactly 3 rows are present
  expect_equal(nrow(out), 3L)
  
  # Per-row checks (order-agnostic)
  row_A <- out[out$package == "A", , drop = FALSE]
  expect_identical(row_A$op, ">=")
  expect_identical(row_A$ver, "1.0")
  expect_identical(row_A$field, "Depends")
  
  row_C <- out[out$package == "C", , drop = FALSE]
  expect_true(is.na(row_C$op))
  expect_true(is.na(row_C$ver))
  expect_identical(row_C$field, "Imports")
  
  row_E <- out[out$package == "E", , drop = FALSE]
  expect_identical(row_E$op, ">=")
  expect_identical(row_E$ver, "2.3")
  expect_identical(row_E$field, "Suggests")
})


test_that("imports all exported symbols into environment", {
  env <- new.env()
  
  # Mock asNamespace to return a fake namespace
  mockery::stub(import_namespace_exports, "asNamespace", "fake_ns")
  
  # Mock getNamespaceExports to return a list of exported names
  mockery::stub(
    import_namespace_exports,
    "getNamespaceExports",
    c("func1", "func2", "data1")
  )
  
  # Mock getExportedValue to return mock functions/data
  get_exported_mock <- mockery::mock(
    function(x) x + 1,  # func1
    function(x) x * 2,  # func2
    data.frame(a = 1)   # data1
  )
  mockery::stub(import_namespace_exports, "getExportedValue", get_exported_mock)
  
  result <- import_namespace_exports("testpkg", env)
  
  # Verify return value
  expect_equal(result, invisible(TRUE))
  
  # Verify all symbols were imported into env
  expect_true(exists("func1", envir = env))
  expect_true(exists("func2", envir = env))
  expect_true(exists("data1", envir = env))
  
  # Verify getExportedValue was called 3 times
  mockery::expect_called(get_exported_mock, 3)
  mockery::expect_args(get_exported_mock, 1, "testpkg", "func1")
  mockery::expect_args(get_exported_mock, 2, "testpkg", "func2")
  mockery::expect_args(get_exported_mock, 3, "testpkg", "data1")
})

test_that("respects overwrite parameter when FALSE", {
  env <- new.env()
  
  # Pre-populate env with existing binding
  env$func1 <- "existing_value"
  
  mockery::stub(import_namespace_exports, "asNamespace", "fake_ns")
  mockery::stub(
    import_namespace_exports,
    "getNamespaceExports",
    c("func1", "func2")
  )
  
  get_exported_mock <- mockery::mock(
    function(x) x + 1,  # func1
    function(x) x * 2   # func2
  )
  mockery::stub(import_namespace_exports, "getExportedValue", get_exported_mock)
  
  # Import with overwrite = FALSE
  result <- import_namespace_exports("testpkg", env, overwrite = FALSE)
  
  expect_equal(result, invisible(TRUE))
  
  # func1 should NOT be overwritten
  expect_equal(env$func1, "existing_value")
  
  # func2 should be imported (didn't exist)
  expect_true(exists("func2", envir = env))
  
  # getExportedValue called only once (for func2, skipped func1)
  mockery::expect_called(get_exported_mock, 1)
  mockery::expect_args(get_exported_mock, 1, "testpkg", "func2")
})


test_that("overwrites existing bindings when overwrite = TRUE", {
  env <- new.env()
  
  # Pre-populate env with existing binding
  env$func1 <- "old_value"
  
  mockery::stub(import_namespace_exports, "asNamespace", "fake_ns")
  mockery::stub(
    import_namespace_exports,
    "getNamespaceExports",
    c("func1")
  )
  
  get_exported_mock <- mockery::mock(
    function(x) x + 1
  )
  mockery::stub(import_namespace_exports, "getExportedValue", get_exported_mock)
  
  # Import with overwrite = TRUE (default)
  result <- import_namespace_exports("testpkg", env, overwrite = TRUE)
  
  expect_equal(result, invisible(TRUE))
  
  # func1 should be overwritten with new value
  expect_true(is.function(env$func1))
  expect_false(identical(env$func1, "old_value"))
  
  mockery::expect_called(get_exported_mock, 1)
})


test_that("applies filter to exported symbols", {
  env <- new.env()
  
  mockery::stub(import_namespace_exports, "asNamespace", "fake_ns")
  
  # All available exports
  mockery::stub(
    import_namespace_exports,
    "getNamespaceExports",
    c("func1", "func2", "func3", "data1")
  )
  
  get_exported_mock <- mockery::mock(
    function(x) x + 1,  # func1
    data.frame(a = 1)   # data1
  )
  mockery::stub(import_namespace_exports, "getExportedValue", get_exported_mock)
  
  # Import only func1 and data1
  result <- import_namespace_exports(
    "testpkg",
    env,
    filter = c("func1", "data1")
  )
  
  expect_equal(result, invisible(TRUE))
  
  # Only filtered symbols should be imported
  expect_true(exists("func1", envir = env))
  expect_true(exists("data1", envir = env))
  expect_false(exists("func2", envir = env))
  expect_false(exists("func3", envir = env))
  
  # getExportedValue called only 2 times (for filtered symbols)
  mockery::expect_called(get_exported_mock, 2)
})


test_that("handles empty exports gracefully", {
  env <- new.env()
  
  mockery::stub(import_namespace_exports, "asNamespace", "fake_ns")
  mockery::stub(import_namespace_exports, "getNamespaceExports", character(0))
  
  get_exported_mock <- mockery::mock()
  mockery::stub(import_namespace_exports, "getExportedValue", get_exported_mock)
  
  result <- import_namespace_exports("emptypkg", env)
  
  expect_equal(result, invisible(TRUE))
  
  # getExportedValue should not be called at all
  mockery::expect_called(get_exported_mock, 0)
})


test_that("handles filter that matches no exports", {
  env <- new.env()
  
  mockery::stub(import_namespace_exports, "asNamespace", "fake_ns")
  mockery::stub(
    import_namespace_exports,
    "getNamespaceExports",
    c("func1", "func2")
  )
  
  get_exported_mock <- mockery::mock()
  mockery::stub(import_namespace_exports, "getExportedValue", get_exported_mock)
  
  # Filter for symbols that don't exist
  result <- import_namespace_exports(
    "testpkg",
    env,
    filter = c("nonexistent1", "nonexistent2")
  )
  
  expect_equal(result, invisible(TRUE))
  
  # getExportedValue should not be called (intersection is empty)
  mockery::expect_called(get_exported_mock, 0)
})


test_that("filter intersects with available exports", {
  env <- new.env()
  
  mockery::stub(import_namespace_exports, "asNamespace", "fake_ns")
  mockery::stub(
    import_namespace_exports,
    "getNamespaceExports",
    c("func1", "func2", "func3")
  )
  
  get_exported_mock <- mockery::mock(
    function(x) x + 1,  # func1
    function(x) x * 2   # func2
  )
  mockery::stub(import_namespace_exports, "getExportedValue", get_exported_mock)
  
  # Filter includes some that exist and some that don't
  result <- import_namespace_exports(
    "testpkg",
    env,
    filter = c("func1", "func2", "nonexistent")
  )
  
  expect_equal(result, invisible(TRUE))
  
  # Only the intersection (func1, func2) should be imported
  expect_true(exists("func1", envir = env))
  expect_true(exists("func2", envir = env))
  expect_false(exists("func3", envir = env))
  expect_false(exists("nonexistent", envir = env))
  
  # getExportedValue called 2 times (for the intersection)
  mockery::expect_called(get_exported_mock, 2)
})

test_that("installs package when not installed", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  result <- install_if_needed("missingpkg")
  
  expect_equal(result, invisible(TRUE))
  
  # install.packages should be called for missing package
  mockery::expect_called(install_packages_mock, 1)
  mockery::expect_args(install_packages_mock, 1, "missingpkg")
  
  # loadNamespace should be called
  mockery::expect_called(load_ns_mock, 1)
})


test_that("does not reinstall when package is installed with no version constraint", {
  mockery::stub(install_if_needed, "requireNamespace", TRUE)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  result <- install_if_needed("existingpkg")
  
  expect_equal(result, invisible(TRUE))
  
  # install.packages should NOT be called
  mockery::expect_called(install_packages_mock, 0)
  
  # loadNamespace should still be called
  mockery::expect_called(load_ns_mock, 1)
})


test_that("skips version check if installed but op is NULL", {
  mockery::stub(install_if_needed, "requireNamespace", TRUE)
  
  pkg_desc_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::packageDescription", pkg_desc_mock)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Call with op = NULL, ver = "1.0.0"
  result <- install_if_needed("pkg", op = NULL, ver = "1.0.0")
  
  expect_equal(result, invisible(TRUE))
  
  # packageDescription should NOT be called (op is NULL)
  mockery::expect_called(pkg_desc_mock, 0)
  
  # install.packages should NOT be called
  mockery::expect_called(install_packages_mock, 0)
})


test_that("skips version check if installed but ver is NULL", {
  mockery::stub(install_if_needed, "requireNamespace", TRUE)
  
  pkg_desc_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::packageDescription", pkg_desc_mock)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Call with op = ">=", ver = NULL
  result <- install_if_needed("pkg", op = ">=", ver = NULL)
  
  expect_equal(result, invisible(TRUE))
  
  # packageDescription should NOT be called (ver is NULL)
  mockery::expect_called(pkg_desc_mock, 0)
  
  # install.packages should NOT be called
  mockery::expect_called(install_packages_mock, 0)
})


test_that("checks version constraint when installed with both op and ver", {
  mockery::stub(install_if_needed, "requireNamespace", TRUE)
  
  # Simulate installed version "1.5.0" (satisfies >= 1.0.0)
  mockery::stub(install_if_needed, "utils::packageDescription", "1.5.0")
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(install_if_needed, "constraint_satisfied", constraint_satisfied_mock)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  result <- install_if_needed("pkg", op = ">=", ver = "1.0.0")
  
  expect_equal(result, invisible(TRUE))
  
  # constraint_satisfied should be called with correct args
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, "1.5.0", ">=", "1.0.0")
  
  # install.packages should NOT be called (constraint is satisfied)
  mockery::expect_called(install_packages_mock, 0)
})


test_that("reinstalls when version constraint is NOT satisfied", {
  mockery::stub(install_if_needed, "requireNamespace", TRUE)
  
  # Simulate installed version "0.9.0" (does NOT satisfy >= 1.0.0)
  mockery::stub(install_if_needed, "utils::packageDescription", "0.9.0")
  
  constraint_satisfied_mock <- mockery::mock(FALSE)
  mockery::stub(install_if_needed, "constraint_satisfied", constraint_satisfied_mock)
  
  install_packages_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Capture message about re-installation
  msgs <- testthat::capture_messages({
    result <- install_if_needed("pkg", op = ">=", ver = "1.0.0")
  })
  
  expect_equal(result, invisible(TRUE))
  
  # Should message about re-installation
  expect_true(
    any(grepl("does not satisfy constraint", msgs, fixed = TRUE)),
    info = paste("Messages were:", paste(msgs, collapse = "\n"))
  )
  
  # install.packages should be called (constraint NOT satisfied)
  mockery::expect_called(install_packages_mock, 1)
  mockery::expect_args(install_packages_mock, 1, "pkg")
})


test_that("uses install.packages for >= and > constraints", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  remotes_install_mock <- mockery::mock()
  mockery::stub(install_if_needed, "remotes::install_version", remotes_install_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Test with >= constraint
  result <- install_if_needed("pkg", op = ">=", ver = "1.0.0")
  
  expect_equal(result, invisible(TRUE))
  
  # install.packages should be called (>= constraint)
  mockery::expect_called(install_packages_mock, 1)
  
  # remotes::install_version should NOT be called
  mockery::expect_called(remotes_install_mock, 0)
})


test_that("uses install.packages for > constraint", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  remotes_install_mock <- mockery::mock()
  mockery::stub(install_if_needed, "remotes::install_version", remotes_install_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Test with > constraint
  result <- install_if_needed("pkg", op = ">", ver = "1.0.0")
  
  expect_equal(result, invisible(TRUE))
  
  # install.packages should be called (> constraint)
  mockery::expect_called(install_packages_mock, 1)
  
  # remotes::install_version should NOT be called
  mockery::expect_called(remotes_install_mock, 0)
})


test_that("uses remotes::install_version for <= constraint", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  remotes_install_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "remotes::install_version", remotes_install_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Test with <= constraint
  result <- install_if_needed("pkg", op = "<=", ver = "2.0.0")
  
  expect_equal(result, invisible(TRUE))
  
  # remotes::install_version should be called
  mockery::expect_called(remotes_install_mock, 1)
  mockery::expect_args(remotes_install_mock, 1, "pkg", version = "2.0.0", 
                       upgrade = "never", force = TRUE)
  
  # install.packages should NOT be called
  mockery::expect_called(install_packages_mock, 0)
})


test_that("uses remotes::install_version for < constraint", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  remotes_install_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "remotes::install_version", remotes_install_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Test with < constraint
  result <- install_if_needed("pkg", op = "<", ver = "2.0.0")
  
  expect_equal(result, invisible(TRUE))
  
  # remotes::install_version should be called
  mockery::expect_called(remotes_install_mock, 1)
  mockery::expect_args(remotes_install_mock, 1, "pkg", version = "2.0.0",
                       upgrade = "never", force = TRUE)
  
  # install.packages should NOT be called
  mockery::expect_called(install_packages_mock, 0)
})


test_that("uses remotes::install_version for == constraint", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", install_packages_mock)
  
  remotes_install_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "remotes::install_version", remotes_install_mock)
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Test with == constraint
  result <- install_if_needed("pkg", op = "==", ver = "1.5.0")
  
  expect_equal(result, invisible(TRUE))
  
  # remotes::install_version should be called
  mockery::expect_called(remotes_install_mock, 1)
  mockery::expect_args(remotes_install_mock, 1, "pkg", version = "1.5.0",
                       upgrade = "never", force = TRUE)
  
  # install.packages should NOT be called
  mockery::expect_called(install_packages_mock, 0)
})


test_that("handles installation errors gracefully", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", function(pkg, ...) {
    stop("CRAN mirror error")
  })
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Should not throw, should capture error message
  msgs <- testthat::capture_messages({
    result <- install_if_needed("pkg")
  })
  
  expect_equal(result, invisible(TRUE))
  
  # Should have error message
  expect_true(
    any(grepl("Failed to install package: pkg - CRAN mirror error", msgs, fixed = TRUE)),
    info = paste("Messages were:", paste(msgs, collapse = "\n"))
  )
})


test_that("loads namespace even if installation fails", {
  mockery::stub(install_if_needed, "requireNamespace", FALSE)
  
  install_packages_mock <- mockery::mock()
  mockery::stub(install_if_needed, "utils::install.packages", function(pkg, ...) {
    stop("install error")
  })
  
  load_ns_mock <- mockery::mock(invisible(NULL))
  mockery::stub(install_if_needed, "loadNamespace", load_ns_mock)
  
  # Suppress error messages
  suppressMessages({
    result <- install_if_needed("pkg")
  })
  
  expect_equal(result, invisible(TRUE))
  
  # loadNamespace should still be called despite install error
  mockery::expect_called(load_ns_mock, 1)
  mockery::expect_args(load_ns_mock, 1, "pkg")
})

test_that("returns TRUE when op is NULL", {
  result <- constraint_satisfied("1.5.0", op = NULL, ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("returns TRUE when ver is NULL", {
  result <- constraint_satisfied("1.5.0", op = ">=", ver = NULL)
  expect_equal(result, TRUE)
})


test_that("returns TRUE when both op and ver are NULL", {
  result <- constraint_satisfied("1.5.0", op = NULL, ver = NULL)
  expect_equal(result, TRUE)
})


test_that("handles >= operator when versions are equal", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  
  result <- constraint_satisfied("1.0.0", op = ">=", ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("handles >= operator when installed is greater", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  
  result <- constraint_satisfied("1.5.0", op = ">=", ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("handles >= operator when installed is less", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  
  result <- constraint_satisfied("0.9.0", op = ">=", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("handles > operator when versions are equal", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  
  result <- constraint_satisfied("1.0.0", op = ">", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("handles > operator when installed is greater", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  
  result <- constraint_satisfied("1.5.0", op = ">", ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("handles > operator when installed is less", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  
  result <- constraint_satisfied("0.9.0", op = ">", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("handles <= operator when versions are equal", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  
  result <- constraint_satisfied("1.0.0", op = "<=", ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("handles <= operator when installed is less", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  
  result <- constraint_satisfied("0.9.0", op = "<=", ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("handles <= operator when installed is greater", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  
  result <- constraint_satisfied("1.5.0", op = "<=", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("handles < operator when versions are equal", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  
  result <- constraint_satisfied("1.0.0", op = "<", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("handles < operator when installed is less", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  
  result <- constraint_satisfied("0.9.0", op = "<", ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("handles < operator when installed is greater", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  
  result <- constraint_satisfied("1.5.0", op = "<", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("handles == operator when versions are equal", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  
  result <- constraint_satisfied("1.0.0", op = "==", ver = "1.0.0")
  expect_equal(result, TRUE)
})


test_that("handles == operator when installed is greater", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  
  result <- constraint_satisfied("1.5.0", op = "==", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("handles == operator when installed is less", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  
  result <- constraint_satisfied("0.9.0", op = "==", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("returns FALSE for unknown operator", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  
  # Use an operator not in the switch statement
  result <- constraint_satisfied("1.0.0", op = "!=", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("returns FALSE for another unknown operator", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  
  result <- constraint_satisfied("1.5.0", op = "<>", ver = "1.0.0")
  expect_equal(result, FALSE)
})


test_that("converts installed_version to character before comparison", {
  compare_version_mock <- mockery::mock(0)
  mockery::stub(constraint_satisfied, "utils::compareVersion", compare_version_mock)
  
  # Pass a package_version object instead of character
  installed_ver <- package_version("1.0.0")
  result <- constraint_satisfied(installed_ver, op = ">=", ver = "1.0.0")
  
  expect_equal(result, TRUE)
  
  # Verify compareVersion was called with character version
  mockery::expect_called(compare_version_mock, 1)
  mockery::expect_args(compare_version_mock, 1, "1.0.0", "1.0.0")
})


test_that("handles numeric package versions", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  
  result <- constraint_satisfied("2.0", op = ">", ver = "1.0")
  expect_equal(result, TRUE)
})


test_that("handles complex version strings", {
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  
  result <- constraint_satisfied("1.2.3.4", op = ">=", ver = "1.2.3.4")
  expect_equal(result, TRUE)
})


test_that("calls compareVersion with correct arguments in order", {
  compare_version_mock <- mockery::mock(-1)
  mockery::stub(constraint_satisfied, "utils::compareVersion", compare_version_mock)
  
  result <- constraint_satisfied("0.5.0", op = ">=", ver = "1.0.0")
  
  expect_equal(result, FALSE)
  
  # Verify compareVersion was called with installed version first, constraint version second
  mockery::expect_called(compare_version_mock, 1)
  mockery::expect_args(compare_version_mock, 1, "0.5.0", "1.0.0")
})


test_that(">= operator with all three compareVersion outputs", {
  # Test with compareVersion returning -1 (less)
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  expect_equal(constraint_satisfied("0.5.0", op = ">=", ver = "1.0.0"), FALSE)
  
  # Test with compareVersion returning 0 (equal)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  expect_equal(constraint_satisfied("1.0.0", op = ">=", ver = "1.0.0"), TRUE)
  
  # Test with compareVersion returning 1 (greater)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  expect_equal(constraint_satisfied("1.5.0", op = ">=", ver = "1.0.0"), TRUE)
})


test_that("> operator with all three compareVersion outputs", {
  # Test with compareVersion returning -1 (less)
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  expect_equal(constraint_satisfied("0.5.0", op = ">", ver = "1.0.0"), FALSE)
  
  # Test with compareVersion returning 0 (equal)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  expect_equal(constraint_satisfied("1.0.0", op = ">", ver = "1.0.0"), FALSE)
  
  # Test with compareVersion returning 1 (greater)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  expect_equal(constraint_satisfied("1.5.0", op = ">", ver = "1.0.0"), TRUE)
})


test_that("<= operator with all three compareVersion outputs", {
  # Test with compareVersion returning -1 (less)
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  expect_equal(constraint_satisfied("0.5.0", op = "<=", ver = "1.0.0"), TRUE)
  
  # Test with compareVersion returning 0 (equal)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  expect_equal(constraint_satisfied("1.0.0", op = "<=", ver = "1.0.0"), TRUE)
  
  # Test with compareVersion returning 1 (greater)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  expect_equal(constraint_satisfied("1.5.0", op = "<=", ver = "1.0.0"), FALSE)
})


test_that("< operator with all three compareVersion outputs", {
  # Test with compareVersion returning -1 (less)
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  expect_equal(constraint_satisfied("0.5.0", op = "<", ver = "1.0.0"), TRUE)
  
  # Test with compareVersion returning 0 (equal)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  expect_equal(constraint_satisfied("1.0.0", op = "<", ver = "1.0.0"), FALSE)
  
  # Test with compareVersion returning 1 (greater)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  expect_equal(constraint_satisfied("1.5.0", op = "<", ver = "1.0.0"), FALSE)
})


test_that("== operator with all three compareVersion outputs", {
  # Test with compareVersion returning -1 (less)
  mockery::stub(constraint_satisfied, "utils::compareVersion", -1)
  expect_equal(constraint_satisfied("0.5.0", op = "==", ver = "1.0.0"), FALSE)
  
  # Test with compareVersion returning 0 (equal)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 0)
  expect_equal(constraint_satisfied("1.0.0", op = "==", ver = "1.0.0"), TRUE)
  
  # Test with compareVersion returning 1 (greater)
  mockery::stub(constraint_satisfied, "utils::compareVersion", 1)
  expect_equal(constraint_satisfied("1.5.0", op = "==", ver = "1.0.0"), FALSE)
})

test_that("returns invisibly TRUE when Depends field is NA", {
  desc <- matrix(NA_character_, nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", NA_character_)
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("returns invisibly TRUE when Depends field is empty string", {
  desc <- matrix("", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "")
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("returns invisibly TRUE when no R constraint found in Depends", {
  desc <- matrix("pkg1, pkg2", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "pkg1, pkg2")
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("returns invisibly TRUE when R constraint exists but normalize_constraint returns NULL", {
  desc <- matrix("R ()", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R ()")
  mockery::stub(validate_R_requirement, "normalize_constraint", NULL)
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("returns invisibly TRUE when normalized constraint is empty string", {
  desc <- matrix("R (   )", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (   )")
  mockery::stub(validate_R_requirement, "normalize_constraint", "")
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("returns invisibly TRUE when constraint string doesn't match operator pattern", {
  desc <- matrix("R (invalid)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (invalid)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "invalid")
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("throws error when R version constraint is not satisfied with >=", {
  desc <- matrix("R (>= 4.5.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>= 4.5.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">= 4.5.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.0"))
  mockery::stub(validate_R_requirement, "constraint_satisfied", FALSE)
  
  expect_error(
    validate_R_requirement(desc),
    "This package requires R version >= 4.5.0 but current R is 4.4.0",
    fixed = TRUE
  )
})


test_that("throws error when R version constraint is not satisfied with >", {
  desc <- matrix("R (> 4.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (> 4.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "> 4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.0.0"))
  mockery::stub(validate_R_requirement, "constraint_satisfied", FALSE)
  
  expect_error(
    validate_R_requirement(desc),
    "This package requires R version > 4.0.0 but current R is 4.0.0",
    fixed = TRUE
  )
})


test_that("returns invisibly TRUE when R version constraint is satisfied with >=", {
  desc <- matrix("R (>= 4.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>= 4.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">= 4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.0"))
  mockery::stub(validate_R_requirement, "constraint_satisfied", TRUE)
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("returns invisibly TRUE when R version constraint is satisfied with >", {
  desc <- matrix("R (> 4.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (> 4.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "> 4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.1.0"))
  mockery::stub(validate_R_requirement, "constraint_satisfied", TRUE)
  
  result <- validate_R_requirement(desc)
  expect_equal(result, invisible(TRUE))
})


test_that("normalizes single = to == operator", {
  desc <- matrix("R (= 4.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (= 4.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "= 4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.0.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  expect_equal(result, invisible(TRUE))
  
  # Verify constraint_satisfied was called with == operator
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.0.0"), "==", "4.0.0")
})


test_that("uses != operator without converting to ==", {
  desc <- matrix("R (!= 4.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (!= 4.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "!= 4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.1.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  expect_equal(result, invisible(TRUE))
  
  # Verify constraint_satisfied was called with != operator (not normalized)
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.1.0"), "!=", "4.0.0")
})


test_that("trims whitespace from version string", {
  desc <- matrix("R (>=  4.0.0  )", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>=  4.0.0  )")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">=  4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.1.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  # Verify version was trimmed
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.1.0"), ">=", "4.0.0")
})


test_that("extracts first R constraint when multiple are present", {
  desc <- matrix("R (>= 4.0.0), R (>= 3.5.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>= 4.0.0), R (>= 3.5.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">= 4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  # Should use first constraint (>= 4.0.0), not second
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.4.0"), ">=", "4.0.0")
})


test_that("handles <= operator in R requirement", {
  desc <- matrix("R (<= 4.5.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (<= 4.5.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "<= 4.5.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.4.0"), "<=", "4.5.0")
})


test_that("handles < operator in R requirement", {
  desc <- matrix("R (< 5.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (< 5.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "< 5.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.4.0"), "<", "5.0.0")
})


test_that("handles == operator in R requirement", {
  desc <- matrix("R (== 4.4.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (== 4.4.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "== 4.4.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.4.0"), "==", "4.4.0")
})


test_that("error message includes current R version", {
  desc <- matrix("R (>= 5.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>= 5.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">= 5.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.2"))
  mockery::stub(validate_R_requirement, "constraint_satisfied", FALSE)
  
  expect_error(
    validate_R_requirement(desc),
    "but current R is 4.4.2",
    fixed = TRUE
  )
})


test_that("error has attr simpleError", {
  desc <- matrix("R (>= 5.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>= 5.0.0)")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">= 5.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.4.0"))
  mockery::stub(validate_R_requirement, "constraint_satisfied", FALSE)
  
  error_obj <- tryCatch(
    validate_R_requirement(desc),
    error = function(e) e
  )
  
  # When call. = FALSE, the call is not included in the error
  expect_true(checkmate::check_class(error_obj, "simpleError"))
})


test_that("handles complex version strings with multiple dots", {
  desc <- matrix("R (>= 4.0.1.2)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>= 4.0.1.2)")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">= 4.0.1.2")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.1.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.1.0"), ">=", "4.0.1.2")
})


test_that("strips R() parentheses correctly from constraint string", {
  desc <- matrix("R (>= 4.0.0)", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R (>= 4.0.0)")
  # This stub simulates the gsub() operation
  mockery::stub(validate_R_requirement, "normalize_constraint", ">= 4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.1.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  # The constraint string passed should not have R( ) wrapper
  mockery::expect_called(constraint_satisfied_mock, 1)
  mockery::expect_args(constraint_satisfied_mock, 1, package_version("4.1.0"), ">=", "4.0.0")
})


test_that("handles whitespace variations in R constraint syntax", {
  desc <- matrix("R(  >=  4.0.0  )", nrow = 1, dimnames = list(NULL, c("Depends")))
  
  mockery::stub(validate_R_requirement, "get_field", "R(  >=  4.0.0  )")
  mockery::stub(validate_R_requirement, "normalize_constraint", ">=  4.0.0")
  mockery::stub(validate_R_requirement, "getRversion", package_version("4.1.0"))
  
  constraint_satisfied_mock <- mockery::mock(TRUE)
  mockery::stub(validate_R_requirement, "constraint_satisfied", constraint_satisfied_mock)
  
  result <- validate_R_requirement(desc)
  
  mockery::expect_called(constraint_satisfied_mock, 1)
})


test_that("all if statement branches are covered", {
  # Branch 1: is.na(deps) returns TRUE
  mockery::stub(validate_R_requirement, "get_field", NA_character_)
  result1 <- validate_R_requirement(matrix(NA_character_, nrow = 1, dimnames = list(NULL, c("Depends"))))
  expect_equal(result1, invisible(TRUE))
  
  # Branch 2: !nzchar(deps) returns TRUE
  mockery::stub(validate_R_requirement, "get_field", "")
  result2 <- validate_R_requirement(matrix("", nrow = 1, dimnames = list(NULL, c("Depends"))))
  expect_equal(result2, invisible(TRUE))
  
  # Branch 3: length(m) == 0
  mockery::stub(validate_R_requirement, "get_field", "pkg1, pkg2")
  result3 <- validate_R_requirement(matrix("pkg1, pkg2", nrow = 1, dimnames = list(NULL, c("Depends"))))
  expect_equal(result3, invisible(TRUE))
  
  # Branch 4: normalize_constraint returns NULL
  mockery::stub(validate_R_requirement, "get_field", "R ()")
  mockery::stub(validate_R_requirement, "normalize_constraint", NULL)
  result4 <- validate_R_requirement(matrix("R ()", nrow = 1, dimnames = list(NULL, c("Depends"))))
  expect_equal(result4, invisible(TRUE))
  
  # Branch 5: regexec doesn't match pattern
  mockery::stub(validate_R_requirement, "get_field", "R (malformed)")
  mockery::stub(validate_R_requirement, "normalize_constraint", "malformed")
  result5 <- validate_R_requirement(matrix("R (malformed)", nrow = 1, dimnames = list(NULL, c("Depends"))))
  expect_equal(result5, invisible(TRUE))
})

test_that("returns NULL when dep is empty string after trimming", {
  result <- parse_dep("")
  expect_null(result)
})


test_that("returns NULL when dep is empty string after trimming", {
  result <- parse_dep("")
  expect_null(result)
})


test_that("returns NULL when dep is whitespace only", {
  result <- parse_dep("   ")
  expect_null(result)
})


test_that("returns NULL when dep is NA", {
  result <- parse_dep(NA_character_)
  expect_null(result)
})


test_that("parses simple package name without version constraint", {
  result <- parse_dep("pkgA")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_null(result$op)
  expect_null(result$ver)
  expect_null(result$raw)
})


test_that("parses package name with whitespace", {
  result <- parse_dep("  pkgA  ")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_null(result$op)
  expect_null(result$ver)
  expect_null(result$raw)
})


test_that("parses package with >= constraint", {
  mockery::stub(parse_dep, "normalize_constraint", ">= 1.0.0")
  
  result <- parse_dep("pkgA (>= 1.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, ">=")
  expect_equal(result$ver, "1.0.0")
  expect_equal(result$raw, ">= 1.0.0")
})


test_that("parses package with > constraint", {
  mockery::stub(parse_dep, "normalize_constraint", "> 1.0.0")
  
  result <- parse_dep("pkgA (> 1.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, ">")
  expect_equal(result$ver, "1.0.0")
  expect_equal(result$raw, "> 1.0.0")
})


test_that("parses package with <= constraint", {
  mockery::stub(parse_dep, "normalize_constraint", "<= 2.0.0")
  
  result <- parse_dep("pkgA (<= 2.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, "<=")
  expect_equal(result$ver, "2.0.0")
  expect_equal(result$raw, "<= 2.0.0")
})


test_that("parses package with < constraint", {
  mockery::stub(parse_dep, "normalize_constraint", "< 2.0.0")
  
  result <- parse_dep("pkgA (< 2.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, "<")
  expect_equal(result$ver, "2.0.0")
  expect_equal(result$raw, "< 2.0.0")
})


test_that("parses package with == constraint", {
  mockery::stub(parse_dep, "normalize_constraint", "== 1.5.0")
  
  result <- parse_dep("pkgA (== 1.5.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, "==")
  expect_equal(result$ver, "1.5.0")
  expect_equal(result$raw, "== 1.5.0")
})


test_that("normalizes single = to == operator", {
  mockery::stub(parse_dep, "normalize_constraint", "= 1.0.0")
  
  result <- parse_dep("pkgA (= 1.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, "==")
  expect_equal(result$ver, "1.0.0")
  expect_equal(result$raw, "= 1.0.0")
})


test_that("returns NULL for malformed dep string that doesn't match regex", {
  result <- parse_dep("invalid (name (nested)")
  expect_equal(result$pkg, "invalid")
})

test_that("handles package name with dots", {
  result <- parse_dep("pkg.A")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkg.A")
  expect_null(result$op)
})


test_that("handles package name with numbers", {
  result <- parse_dep("pkg123")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkg123")
  expect_null(result$op)
})


test_that("handles package name with mixed alphanumeric and dots", {
  result <- parse_dep("pkg.1.2.A")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkg.1.2.A")
  expect_null(result$op)
})


test_that("handles whitespace around package name in constraint", {
  mockery::stub(parse_dep, "normalize_constraint", ">= 1.0.0")
  
  result <- parse_dep("  pkgA  (>= 1.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, ">=")
})


test_that("handles whitespace around operator and version", {
  mockery::stub(parse_dep, "normalize_constraint", ">=  1.0.0")
  
  result <- parse_dep("pkgA (>=  1.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, ">=")
  expect_equal(result$ver, "1.0.0")
})

test_that("returns op and ver as NULL when raw constraint is unrecognized", {
  mockery::stub(parse_dep, "normalize_constraint", "invalid constraint")
  
  result <- parse_dep("pkgA (invalid constraint)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_null(result$op)
  expect_null(result$ver)
  expect_equal(result$raw, "invalid constraint")
})


test_that("keeps raw constraint even when op/ver cannot be parsed", {
  mockery::stub(parse_dep, "normalize_constraint", "malformed")
  
  result <- parse_dep("pkgA (malformed)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_null(result$op)
  expect_null(result$ver)
  expect_equal(result$raw, "malformed")
})


test_that("handles != operator (not normalized)", {
  mockery::stub(parse_dep, "normalize_constraint", "!= 1.0.0")
  
  result <- parse_dep("pkgA (!= 1.0.0)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, "!=")
  expect_equal(result$ver, "1.0.0")
})


test_that("skips constraint parsing when raw is NULL after normalization", {
  mockery::stub(parse_dep, "normalize_constraint", NULL)
  
  result <- parse_dep("pkgA (   )")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_null(result$op)
  expect_null(result$ver)
  expect_null(result$raw)
})


test_that("handles complex version strings in constraint", {
  mockery::stub(parse_dep, "normalize_constraint", ">= 1.2.3.4")
  
  result <- parse_dep("pkgA (>= 1.2.3.4)")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, ">=")
  expect_equal(result$ver, "1.2.3.4")
})


test_that("trims whitespace from extracted version string", {
  mockery::stub(parse_dep, "normalize_constraint", ">=  1.0.0  ")
  
  result <- parse_dep("pkgA (>=  1.0.0  )")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_equal(result$op, ">=")
  expect_equal(result$ver, "1.0.0")
})


test_that("returns list with all four elements", {
  mockery::stub(parse_dep, "normalize_constraint", ">= 1.0.0")
  
  result <- parse_dep("pkgA (>= 1.0.0)")
  
  expect_true(all(c("pkg", "op", "ver", "raw") %in% names(result)))
  expect_equal(length(result), 4)
})


test_that("single = operator is specifically converted to ==", {
  mockery::stub(parse_dep, "normalize_constraint", "= 1.0.0")
  
  result <- parse_dep("pkgA (= 1.0.0)")
  
  expect_equal(result$op, "==")
  expect_false(identical(result$op, "="))
})


test_that("other operators are not converted", {
  mockery::stub(parse_dep, "normalize_constraint", ">= 1.0.0")
  
  result <- parse_dep("pkgA (>= 1.0.0)")
  
  expect_equal(result$op, ">=")
})


test_that("handles length(m) < 2 case with warning", {
  expect_warning(
    parse_dep("()"),
    "Could not parse dependency:"
  )
})


test_that("handles pattern with no version constraint (length m < 3)", {
  result <- parse_dep("pkgA")
  
  expect_true(checkmate::check_list(result))
  expect_equal(result$pkg, "pkgA")
  expect_null(result$raw)
})

test_that("all conditional branches for empty/NA dep are covered", {
  # Branch 1: empty string
  result1 <- parse_dep("")
  expect_null(result1)
  
  # Branch 2: NA
  result2 <- parse_dep(NA_character_)
  expect_null(result2)
  
  # Branch 3: whitespace only (becomes empty after trim)
  result3 <- parse_dep("   ")
  expect_null(result3)
})


test_that("all conditional branches for op normalization are covered", {
  # Branch 1: op == "="
  mockery::stub(parse_dep, "normalize_constraint", "= 1.0.0")
  result1 <- parse_dep("pkgA (= 1.0.0)")
  expect_equal(result1$op, "==")
  
  # Branch 2: op != "=" (stays as is)
  mockery::stub(parse_dep, "normalize_constraint", ">= 1.0.0")
  result2 <- parse_dep("pkgA (>= 1.0.0)")
  expect_equal(result2$op, ">=")
})


test_that("all conditional branches for constraint parsing are covered", {
  # Branch 1: length(m2) >= 3 (valid constraint)
  mockery::stub(parse_dep, "normalize_constraint", ">= 1.0.0")
  result1 <- parse_dep("pkgA (>= 1.0.0)")
  expect_equal(result1$op, ">=")
  expect_equal(result1$ver, "1.0.0")
  
  # Branch 2: length(m2) < 3 (unrecognized constraint)
  mockery::stub(parse_dep, "normalize_constraint", "invalid")
  result2 <- parse_dep("pkgA (invalid)")
  expect_null(result2$op)
  expect_null(result2$ver)
  expect_equal(result2$raw, "invalid")
})
