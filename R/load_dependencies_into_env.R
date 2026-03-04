

#' Null/empty coalescing operator
#'
#' Returns `b` when `a` is `NULL`, length-zero, or `NA`; otherwise returns `a`.
#'
#' @param a Any object.
#' @param b Fallback value.
#'
#' @return Either `a` or `b`.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a


#' Safe DESCRIPTION field accessor
#'
#' Fetches a DESCRIPTION field from a one-row `read.dcf()` result, returning
#' `NA_character_` if the field is not present.
#'
#' @param desc A one-row matrix returned by `read.dcf("DESCRIPTION")`.
#' @param field `character(1)`. Field name to retrieve.
#'
#' @return `character(1)` value or `NA_character_` if absent.
#'
#' @keywords internal
#' @noRd
get_field <- function(desc, field) {
  if (!is.null(desc[1, field])) desc[1, field] else NA_character_
}


' Normalize HTML-escaped version operators
#'
#' Converts HTML-escaped comparison operators (`&gt;`, `&lt;`) found in
#' DESCRIPTION constraints into their literal forms (`>`, `<`), trims
#' whitespace, and collapses internal spacing for consistent parsing.
#'
#' Returns `NULL` for empty or `NA` inputs after normalization.
#'
#' @param x `character(1)` or `NULL`. A raw constraint string (e.g., `"&gt;= 1.2.3"`).
#'
#' @return `character(1)` normalized constraint string, or `NULL` if empty/`NA`.
#'
#' @keywords internal
#' @noRd
normalize_constraint <- function(x) {
  if (is.null(x) || is.na(x)) return(NULL)
  x <- trimws(x)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("\\s+", " ", x)
  if (!nzchar(x)) return(NULL)
  x
}


#' Parse a single DESCRIPTION dependency entry
#'
#' Parses entries of the form `"pkg"` or `"pkg (op version)"` from DESCRIPTION
#' fields, where `op` can be one of `>=`, `>`, `<=`, `<`, `==`, or `=` (which is
#' normalized to `==`). HTML-escaped operators (`&gt;`, `&lt;`) are handled.
#'
#' @param dep `character(1)`. A single dependency token from DESCRIPTION, e.g.,
#'   `"pkg"` or `"pkg (>= 1.2.3)"`.
#'
#' @return A `list` with elements:
#'   - `pkg`: `character(1)` package name
#'   - `op`: `character(1)` operator or `NULL`
#'   - `ver`: `character(1)` version string or `NULL`
#'   - `raw`: `character(1)` raw normalized constraint, or `NULL`
#'   Returns `NULL` if parsing fails or token is empty.
#'
#' @keywords internal
#' @noRd
parse_dep <- function(dep) {
  dep <- trimws(dep)
  if (identical(dep, "") || is.na(dep)) return(NULL)
  m <- regmatches(dep, regexec("^([A-Za-z0-9\\.]+)\\s*(?:\\(([^)]+)\\))?$", dep))[[1]]
  if (length(m) >= 2) {
    pkg <- trimws(m[2])
    raw <- if (length(m) >= 3 && !is.na(m[3])) trimws(m[3]) else NULL
    raw <- normalize_constraint(raw)
    op <- ver <- NULL
    if (!is.null(raw)) {
      # accept one of: >=, >, <=, <, ==, = 
      m2 <- regmatches(raw, regexec("^([><=!]=?|==)\\s*(\\S.*)$", raw))[[1]]
      if (length(m2) >= 3) {
        op  <- m2[2]
        ver <- trimws(m2[3])
        if (op == "=") op <- "=="  # normalize
      } else {
        # Unrecognized constraint text; keep raw but unset op/ver
        op <- ver <- NULL
      }
    }
    return(list(pkg = pkg, op = op, ver = ver, raw = raw))
  } else {
    warning("Could not parse dependency: ", dep)
    return(NULL)
  }
}


#' Check a version constraint against an installed version
#'
#' Compares an installed version string against a constraint expressed as an
#' operator and version using `utils::compareVersion()`. If `op` or `ver` is
#' `NULL`, the constraint is treated as satisfied.
#'
#' @param installed_version `character(1)` or `package_version`. The installed version.
#' @param op `character(1)` operator among `>=`, `>`, `<=`, `<`, `==`.
#' @param ver `character(1)` version to compare against.
#'
#' @return `logical(1)`. `TRUE` if satisfied; otherwise `FALSE`.
#'
#' @keywords internal
#' @noRd
constraint_satisfied <- function(installed_version, op, ver) {
  if (is.null(op) || is.null(ver)) return(TRUE)
  cmp <- utils::compareVersion(as.character(installed_version), ver)
  switch(op,
         ">=" = cmp >= 0,
         ">"  = cmp >  0,
         "<=" = cmp <= 0,
         "<"  = cmp <  0,
         "==" = cmp == 0,
         # Unknown operator -> be conservative (not satisfied)
         FALSE
  )
}


#' Install a package if missing or version-mismatched
#'
#' Ensures a package is installed and its namespace can be loaded. If a version
#' constraint is provided and the installed version does not satisfy it,
#' re-installs the package. Uses `install.packages()` for `>=`/`>` constraints
#' (assumes latest satisfies), and `remotes::install_version()` for `<=`, `<`,
#' or `==` constraints to target a specific version.
#'
#' Always attempts to `loadNamespace(pkg)` at the end (suppressed), so the
#' namespace is available even if the package is not attached.
#'
#' @param pkg `character(1)`. Package name.
#' @param op `character(1)` or `NULL`. Constraint operator.
#' @param ver `character(1)` or `NULL`. Constraint version.
#'
#' @return Invisibly returns `TRUE`.
#'
#' @keywords internal
#' @noRd
install_if_needed <- function(pkg, op = NULL, ver = NULL) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  needs_install <- !installed
  
  if (installed && !is.null(op) && !is.null(ver)) {
    inst_ver <- utils::packageDescription(pkg, fields = "Version")
    if (!constraint_satisfied(inst_ver, op, ver)) {
      message("Installed version of ", pkg, " (", inst_ver,
              ") does not satisfy constraint ", op, " ", ver, ". Re-installing.")
      needs_install <- TRUE
    }
  }
  
  if (needs_install) {
    tryCatch({
      if (!is.null(op) && !is.null(ver)) {
        # Strategy:
        # - For >= / >: the latest should satisfy (install.packages)
        # - For <= / < / ==: try exact/targeted via remotes::install_version
        if (op %in% c(">=", ">")) {
          utils::install.packages(pkg)
        } else {
          remotes::install_version(pkg, version = ver, upgrade = "never", force = TRUE)
        }
      } else {
        utils::install.packages(pkg)
      }
    }, error = function(e) {
      message("Failed to install package: ", pkg, " - ", conditionMessage(e))
    })
  }
  
  # Ensure namespace is loaded
  try(suppressWarnings(loadNamespace(pkg)), silent = TRUE)
  invisible(TRUE)
}


#' Import namespace exports into an environment
#'
#' Copies all (or a filtered subset of) exported symbols from a package
#' namespace into a target environment. This enables bare symbol resolution
#' (e.g., `plot`, `lm`) inside environments that do not inherit from
#' `.GlobalEnv` (e.g., when the parent is `baseenv()`), independent of the
#' search path or attachment state.
#'
#' @param pkg `character(1)`. Package name.
#' @param env `environment`. Target environment to receive the exported symbols.
#' @param overwrite `logical(1)`. Overwrite existing bindings in `env`. Default `TRUE`.
#' @param filter `character()` or `NULL`. If provided, only these exported names
#'   are imported (intersection with `getNamespaceExports(pkg)`).
#'
#' @return Invisibly returns `TRUE`.
#'
#' @keywords internal
#' @noRd
import_namespace_exports <- function(pkg, env, overwrite = TRUE, filter = NULL) {
  ns <- asNamespace(pkg)
  exports <- getNamespaceExports(pkg)
  if (!is.null(filter)) {
    exports <- intersect(exports, filter)
  }
  for (nm in exports) {
    if (!overwrite && exists(nm, envir = env, inherits = FALSE)) next
    env[[nm]] <- getExportedValue(pkg, nm)
  }
  invisible(TRUE)
}


#' Resolve dependencies from DESCRIPTION into a data.frame
#'
#' Extracts dependency tokens from DESCRIPTION fields (default: `Depends`,
#' `Imports`, `Suggests`), optionally excluding the `R (>= x.y.z)` entry,
#' parses each token, and returns a structured data frame suitable for further
#' processing (installation, loading, and visibility control).
#'
#' The returned `field` values are trimmed of whitespace to avoid equality
#' issues when selecting rows (e.g., `"Depends "` vs `"Depends"`).
#'
#' @param desc A one-row matrix returned by `read.dcf("DESCRIPTION")`.
#' @param fields `character()`. DESCRIPTION fields to parse.
#' @param exclude_R `logical(1)`. If `TRUE`, drop `R (...)` tokens from results.
#'
#' @return A `data.frame` with columns:
#'   - `package`: package name
#'   - `op`: operator (may be `NA`)
#'   - `ver`: version string (may be `NA`)
#'   - `field`: the DESCRIPTION field source (trimmed)
#'
#' @keywords internal
#' @noRd
resolve_description_deps <- function(desc, fields = c("Depends", "Imports", "Suggests"),
                                     exclude_R = TRUE) {
  vals <- unlist(lapply(fields, function(f) {
    v <- get_field(desc, f)
    if (is.na(v) || !nzchar(v)) return(character(0))
    parts <- unlist(strsplit(v, ",\\s*"))
    parts <- parts[parts != "" & !is.na(parts)]
    if (exclude_R) {
      parts <- parts[!grepl("^R(\\s|$)", parts)]
    }
    # Tag each part with its originating field so we can decide import policy
    paste(f, ":::SEP:::", parts)
  }), use.names = FALSE)
  
  if (length(vals) == 0) {
    # create a an empty data frame that preserves the structure:
    # of column names, column types, zero rows.
    return(data.frame(package=character(), op=character(),
                                      ver=character(), field=character()))
  }
  
  res <- lapply(vals, function(x) {
    sp <- strsplit(x, ":::SEP:::", fixed = TRUE)[[1]]
    field <- sp[1]
    dep   <- sp[2]
    parsed <- parse_dep(dep)
    if (is.null(parsed)) return(NULL)
    data.frame(
      package = parsed$pkg,
      op = parsed$op %||% NA_character_,
      ver = parsed$ver %||% NA_character_,
      field = trimws(field),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, res[!vapply(res, is.null, logical(1))])
}


#' Validate the `R (>= x.y.z)` requirement in DESCRIPTION
#'
#' If a Depends entry of the form `R (op version)` exists, normalize the
#' constraint, parse it, and compare against the current interpreter's version
#' (`getRversion()`). Throws an error if the requirement is not satisfied.
#'
#' @param desc A one-row matrix returned by `read.dcf("DESCRIPTION")`.
#'
#' @return Invisibly returns `TRUE` (or throws on failure).
#'
#' @keywords internal
#' @noRd
validate_R_requirement <- function(desc) {
  deps <- get_field(desc, "Depends")
  if (is.na(deps) || !nzchar(deps)) return(invisible(TRUE))
  m <- regmatches(deps, gregexpr("R\\s*\\(([^)]+)\\)", deps))[[1]]
  if (length(m) == 0) return(invisible(TRUE))
  # take first constraint
  cstr <- gsub("^R\\s*\\(|\\)$", "", m[1])
  cstr <- normalize_constraint(cstr)
  if (!length(cstr) || is.null(cstr)) return(invisible(TRUE))
  m2 <- regmatches(cstr, regexec("^([><=!]=?|==)\\s*(\\S.*)$", cstr))[[1]]
  if (length(m2) >= 3) {
    op <- if (m2[2] == "=") "==" else m2[2]
    ver <- trimws(m2[3])
    rver <- getRversion()
    ok <- constraint_satisfied(rver, op, ver)
    if (!ok) {
      stop("This package requires R version ", op, " ", ver,
           " but current R is ", as.character(rver), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Load and Install Package Dependencies from a DESCRIPTION File
#'
#' Reads a package's `DESCRIPTION` file, parses dependency fields
#' (`Depends`, `Imports`, `Suggests`), installs missing packages or those that
#' do not satisfy version constraints, loads namespaces, and—for packages listed
#' under **Depends**—imports their exported symbols into a provided evaluation
#' environment. This makes bare function names visible when evaluating code in
#' environments that do not inherit from `.GlobalEnv` (e.g., environments with
#' `baseenv()` as parent).
#'
#' @section Behavior:
#'
#' 1. **Dependency parsing**  
#'    Parses entries of the form `"pkg"` or `"pkg (op version)"`, where the
#'    operator `op` is one of `>=`, `>`, `<=`, `<`, `==`. HTML-escaped forms
#'    (`&gt;`, `&lt;`) are normalized.
#'
#' 2. **Dependency fields**  
#'    Dependencies are extracted from the `Depends`, `Imports`, and `Suggests`
#'    fields. The `Depends: R (>= x.y.z)` entry is ignored for installation
#'    purposes, but the R version constraint is validated.
#'
#' 3. **Base/recommended packages**  
#'    Unlike tools such as `R CMD check`, this function **does not skip**
#'    recommended or base packages (e.g., `graphics`, `stats`, `utils`,
#'    `grDevices`) because packages listed in `Depends:` must be made visible
#'    in the target evaluation environment even when its parent is `baseenv()`.
#'    If a package is already installed with R, no installation is attempted.
#'
#' 4. **Availability and version checks**  
#'    Package presence is determined via `requireNamespace(pkg, quietly = TRUE)`
#'    instead of `installed.packages()`, for performance and portability.  
#'    If a version constraint is declared, the installed version (from
#'    `packageDescription(pkg, fields = "Version")`) is compared using
#'    `utils::compareVersion()`.
#'
#' 5. **Package installation**  
#'    - For constraints of the form `>=` or `>`:  
#'      `install.packages()` is used (latest version is assumed valid).  
#'    - For `<=`, `<`, or `==`:  
#'      `remotes::install_version()` is used to install an exact version.  
#'
#' 6. **Loading behavior**  
#'    Each dependency is namespace-loaded via `loadNamespace()`.  
#'    Packages that appear in the `Depends` field have all their **exported
#'    symbols imported** into the supplied `env`. This makes calls such as
#'    `plot()`, `lm()`, or `head()` resolve correctly inside environments whose
#'    parent is not `.GlobalEnv`.
#'
#' @param pkg_source_path `character(1)`. Path to a package source directory
#'   containing a `DESCRIPTION` file.
#'
#' @param env An environment into which exported symbols from packages listed
#'   in the **Depends** field will be imported. This allows code evaluated in
#'   `env` to resolve functions from those packages using bare names without
#'   requiring attachment on the search path.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @details
#' This function avoids calling `installed.packages()` because that function
#' scans all installed packages and is slow on some platforms (especially
#' Windows and network filesystems). Using `requireNamespace()` is the
#' recommended approach for checking whether a package is installed and usable.
#' `packageDescription()` is used only on a small set of packages when version
#' information or priority metadata is needed.
#'
#' When evaluating code in environments that do not inherit from `.GlobalEnv`,
#' attaching packages does not necessarily make their exported symbols available.
#' For this reason, exports from packages listed under `Depends` are explicitly
#' imported into the target environment.
#'
#' @keywords internal
load_dependencies_into_env <- function(pkg_source_path, env) {
  # Ensure 'remotes' exists for versioned installs
  
  if (!requireNamespace("remotes", quietly = TRUE)) {
    utils::install.packages("remotes")
  }
  
  desc_file <- file.path(pkg_source_path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    stop("DESCRIPTION file not found at: ", desc_file)
  }
  
  desc <- read.dcf(desc_file)
  
  # 1) Validate R version requirement if specified
  validate_R_requirement(desc)
  
  # 2) Resolve dependencies (Depends, Imports, Suggests)
  deps_df <- resolve_description_deps(desc,
                                      fields = c("Depends", "Imports", "Suggests"),
                                      exclude_R = TRUE)
  if (nrow(deps_df) == 0) return(invisible(TRUE))
  
  # 3) Install / load namespaces
  for (i in seq_len(nrow(deps_df))) {
    pkg <- deps_df$package[i]
    op  <- if (!is.na(deps_df$op[i])) deps_df$op[i] else NULL
    ver <- if (!is.na(deps_df$ver[i])) deps_df$ver[i] else NULL
    
    # Do NOT skip base packages (grDevices, graphics, stats, utils, methods)
    # Installation will be skipped automatically if already present.
    install_if_needed(pkg, op, ver)
  }
  
  # 4) Make symbols visible from 'env'
  depends_pkgs <- deps_df$package[deps_df$field %in% c("Depends", "Imports", "Suggests")]
  depends_pkgs <- unique(depends_pkgs)
  
  for (pkg in depends_pkgs) {
    # Ensure namespace is loaded (defensive)
    try(suppressWarnings(loadNamespace(pkg)), silent = TRUE)
    # Import all exports into env so code evaluated there can see them directly
    import_namespace_exports(pkg, env, overwrite = TRUE)
  }
  
  invisible(TRUE)
}
