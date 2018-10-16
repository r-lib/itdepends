#' @export
pkg_weigh <- function(packages, repos = c(CRAN = "https://cloud.r-project.org")) {

  if (is.null(the$db)) {
    the$db <- available.packages(repos = repos, type = "source")
  }

  deps <- list(
    direct_hard = tools::package_dependencies(packages, db = the$db, which = c("Depends", "Imports", "LinkingTo"), recursive = FALSE),
    direct_soft = tools::package_dependencies(packages, db = the$db, which = c("Suggests", "Enhances"), recursive = FALSE),

    total_hard = tools::package_dependencies(packages, db = the$db, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE),
    total_soft = tools::package_dependencies(packages, db = the$db, which = c("Suggests", "Enhances"), recursive = TRUE)
  )

  std_pkgs <- unlist(tools:::.get_standard_package_names())

  deps <- lapply(deps, function(dep_type) {
    lapply(dep_type, function(d) {
      Filter(Negate(function(x) x %in% std_pkgs), d)
    })
  })

  exported_funs <- lapply(packages, function(p) tryCatch(getNamespaceExports(p), error = function(e) NA))

  timings <- get_timings(packages, repos)

  timings_total_hard <- lapply(deps$total_hard, get_timings)

  deps_install <- vapply(timings_total_hard, function(x) sum(median_install_time(x)), numeric(1))

  tibble::tibble(
    package = packages,
    compiled = ifelse(the$db[packages, "NeedsCompilation"] == "yes", TRUE, FALSE),
    direct_hard  = lengths(deps$direct_hard),
    direct_soft = lengths(deps$direct_soft),
    total_hard  = lengths(deps$total_hard),
    total_soft = lengths(deps$total_soft),
    exported_funs = lengths(exported_funs),
    self_install = median_install_time(timings),
    deps_install = deps_install,
    timings = timings,
    deps_timings = timings_total_hard
  )
}

get_timings <- function(pkgs, repos = getOption("repos")) {
  urls <- sprintf("%s/web/checks/check_results_%s.html", repos[["CRAN"]] %||% repos, pkgs)
  out_files <- file.path(the$dir, sprintf("check_resutls_%s.html", pkgs))

  new <- !file.exists(out_files)

  if (any(new)) {
    download.file(urls[new], out_files[new], method = "libcurl", quiet = TRUE)
  }

  lapply(stats::setNames(out_files, pkgs), parse_timing)
}

parse_timing <- function(file) {
  rvest::html_table(xml2::read_html(file))[[1]]
}

median_install_time <- function(timings) {
  vapply(timings, function(x) median(x$Tinstall, na.rm = TRUE), numeric(1))
}

`%||%` <- function(x, y) if (is.null(x)) y else x


the <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  the$dir <- tempfile()
  dir.create(the$dir)
}

.onUnload <- function(libpath) {
  unlink(the$dir, recursive = TRUE)
}
