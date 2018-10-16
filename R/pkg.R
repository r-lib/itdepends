#' Calculate the package weight
#'
#' @return
#' - exported_funs - If you compare the # of exported funs vs the
#'   number of function you import / use in your package it gives you some idea
#'   of how much work it would take to remove the dependency. e.g. if you use 50
#'   functions from dplyr you would have to replicate 1/5 of more of the
#'   functionality from dplyr to remove the dependency, if you only use 1-5
#'   functions maybe it is more tractable
#'
#'   But for packages like rcmdcheck or glue that only have a few functions you
#'   will have to replicate a larger proportion of the package even if you only
#'   import one function
#'
#'   It also gives you some idea of the API size, which can give you an idea
#'   how likely it is to change in the future
#' @export
pkg_weight <- function(packages, repos = c(CRAN = "https://cloud.r-project.org")) {

  if (is.null(the$db)) {
    the$db <- available.packages(repos = repos, type = "source")
  }

  packages <- stats::setNames(packages, packages)

  user_deps = lapply(packages, find_deps, available = the$db, top_dep = NA, rec_dep = NA, include_pkgs = FALSE)
  dev_deps = lapply(packages, find_deps, available = the$db, top_dep = TRUE, rec_dep = NA, include_pkgs = FALSE)

  std_pkgs <- unlist(tools:::.get_standard_package_names())

  user_deps <- filter_deps(user_deps, std_pkgs)
  dev_deps <- filter_deps(dev_deps, std_pkgs)

  exported_funs <- lapply(packages, function(p) tryCatch(getNamespaceExports(p), error = function(e) NA))

  timings <- get_timings(packages, repos)

  timings_user <- lapply(user_deps, get_timings)

  deps_install <- vapply(timings_user, function(x) sum(median_install_time(x)), numeric(1))

  if (is.null(the$archive)) {
    the$archive <- tools:::read_CRAN_object(repos[["CRAN"]], "src/contrib/Meta/archive.rds")
  }
  if (is.null(the$current)) {
    the$current <- tools:::read_CRAN_object(repos[["CRAN"]], "src/contrib/Meta/current.rds")
  }

  size <- the$current[packages, "size"]

  # TODO: what timezone is the CRAN machine in?
  last_release <- as.POSIXct(the$current[packages, "mtime"])

  archived_releases <- the$archive[packages]
  archived_release_dates <- lapply(the$archive[packages], function(p) as.POSIXct(p[["mtime"]]))

  first_release <- .POSIXct(unlist(lapply(archived_release_dates, function(date) min(sort(date)))))

  time_52_weeks_ago <- Sys.time() - 24 * 60 * 60 * 7 * 52

  archived_releases_last_52 <- vapply(archived_release_dates, function(date) sum(date > time_52_weeks_ago), integer(1))

  releases_last_52 <- archived_releases_last_52 + as.integer(last_release > time_52_weeks_ago)

  tibble::tibble(
    package = packages,
    user_deps = lengths(user_deps),
    dev_deps = lengths(dev_deps),
    self_install = median_install_time(timings),
    deps_install = deps_install,
    compiled = ifelse(the$db[packages, "NeedsCompilation"] == "yes", TRUE, FALSE),
    exported_funs = lengths(exported_funs),
    size = size,
    first_release = first_release,
    last_release = last_release,
    total_releases = lengths(archived_release_dates) + 1,
    releases_last_52,
    timings = timings,
    deps_timings = timings_user
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


find_deps <- function(packages, available = available.packages(),
                      top_dep = TRUE, rec_dep = NA, include_pkgs = TRUE) {

  if (length(packages) == 0 || identical(top_dep, FALSE))
    return(character())

  top_dep <- standardise_dep(top_dep)
  rec_dep <- standardise_dep(rec_dep)

  top <- tools::package_dependencies(packages, db = available, which = top_dep)
  top_flat <- unlist(top, use.names = FALSE)

  if (length(rec_dep) != 0 && length(top_flat) > 0) {
    rec <- tools::package_dependencies(top_flat, db = available, which = rec_dep,
      recursive = TRUE)
    rec_flat <- unlist(rec, use.names = FALSE)
  } else {
    rec_flat <- character()
  }

  unique(c(if (include_pkgs) packages, top_flat, rec_flat))
}

standardise_dep <- function(x) {
  if (identical(x, NA)) {
    c("Depends", "Imports", "LinkingTo")
  } else if (isTRUE(x)) {
    c("Depends", "Imports", "LinkingTo", "Suggests")
  } else if (identical(x, FALSE)) {
    character(0)
  } else if (is.character(x)) {
    x
  } else {
    stop("Dependencies must be a boolean or a character vector", call. = FALSE)
  }
}

filter_deps <- function(deps, pkgs) {
  lapply(deps, function(d) {
    Filter(Negate(function(x) x %in% pkgs), d)
  })
}
