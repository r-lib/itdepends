#' Calculate the package weight
#'
#' @param packages Packages to weigh
#' @param repos The CRAN or CRAN-like repo to use
#' @inheritParams cranlogs::cran_downloads
#' @return
#' - funs - If you compare the # of exported funs vs the
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
#' @examples
#' \dontrun{
#'   dep_weight(c("dplyr", "data.table"))
#' }
dep_weight <- function(packages, repos = c(CRAN = "https://cloud.r-project.org"), when = "last-week") {

  packages <- stats::setNames(packages, packages)

  user_deps <- lapply(packages, find_deps, top_dep = NA, rec_dep = NA, include_pkgs = FALSE)
  dev_deps <- lapply(packages, find_deps, top_dep = TRUE, rec_dep = NA, include_pkgs = FALSE)

  std_pkgs <- unlist(get(".get_standard_package_names", asNamespace("tools"))())

  exported_funs <- lapply(packages, function(p) tryCatch(getNamespaceExports(p), error = function(e) NA))

  timings <- get_timings(packages, repos)

  timings_user <- lapply(user_deps, get_timings)

  timings_dev <- lapply(dev_deps, get_timings)

  bin_user <- vapply(user_deps, function(x) sum(bin_size(x), na.rm = TRUE), numeric(1))

  bin_dev <- vapply(dev_deps, function(x) sum(bin_size(x), na.rm = TRUE), numeric(1))

  deps_install <- vapply(timings_user, function(x) sum(median_install_time(x), na.rm = TRUE), numeric(1))

  dev_dep_install <- vapply(timings_dev, function(x) sum(median_install_time(x), na.rm = TRUE), numeric(1))

  if (is.null(the$archive)) {
    the$archive <- get("read_CRAN_object", asNamespace("tools"))(repos[["CRAN"]], "src/contrib/Meta/archive.rds")
  }
  if (is.null(the$current)) {
    the$current <- get("read_CRAN_object", asNamespace("tools"))(repos[["CRAN"]], "src/contrib/Meta/current.rds")
  }

  # TODO: what timezone is the CRAN machine in?
  last_release <- as.POSIXct(the$current[packages, "mtime"])

  archived_release_dates <- lapply(the$archive[packages], function(p) as.POSIXct(p[["mtime"]] %||% NA, ))

  first_release <- .POSIXct(unlist(lapply(archived_release_dates, function(date) min(sort(date)))))

  time_52_weeks_ago <- Sys.time() - 24 * 60 * 60 * 7 * 52

  archived_releases_last_52 <- vapply(archived_release_dates, function(date) sum(date > time_52_weeks_ago), integer(1))

  releases_last_52 <- archived_releases_last_52 + as.integer(last_release > time_52_weeks_ago)

  downloads <- vapply(packages, get_downloads, numeric(1), when = when)

  gh_info <- dplyr::bind_rows(lapply(packages, get_github_info))

  tibble::tibble(
    package = packages,
    num_user = lengths(user_deps),
    bin_self = bin_size(packages),
    bin_user = bin_self + bin_user,
    install_self = median_install_time(timings),
    install_user = install_self + deps_install,
    funs = vapply(exported_funs, function(x) if (all(is.na(x))) NA_integer_ else length(x), integer(1)),
    downloads = downloads,
    last_release = last_release,
    open_issues = gh_info$open_issues,
    last_updated = gh_info$last_updated,
    stars = gh_info$stars,
    forks = gh_info$forks,
    first_release = first_release,
    total_releases = lengths(archived_release_dates) + 1,
    releases_last_52,
    num_dev = lengths(dev_deps),
    install_dev = install_user + dev_dep_install,
    bin_dev = bin_self + bin_dev,
    src_size = src_size(packages),
    user_deps = user_deps,
    dev_deps = dev_deps,
    self_timings = timings,
    user_timings = timings_user,
    dev_timings = timings_dev
  )
}

get_timings <- function(pkgs, repos = getOption("repos")) {
  urls <- sprintf("%s/web/checks/check_results_%s.html", repos[["CRAN"]] %||% repos, pkgs)
  out_files <- file.path(the$dir, sprintf("check_results_%s.html", pkgs))

  new <- !file.exists(out_files)

  if (any(new)) {
    suppressWarnings(try(
        utils::download.file(urls[new], out_files[new], method = "libcurl", quiet = TRUE), silent = TRUE))
  }

  lapply(stats::setNames(out_files, pkgs), parse_timing)
}

parse_timing <- memoise::memoise(function(file) {
  if (!file.exists(file)) {
    return()
  }

  rvest::html_table(xml2::read_html(file))[[1]]
})

median_install_time <- function(timings) {
  if (is.null(timings)) {
    return(NA_real_)
  }

  vapply(timings, function(x) stats::median(x$Tinstall, na.rm = TRUE) %||% NA_real_, numeric(1))
}

`%||%` <- function(x, y) if (is.null(x)) y else x


the <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  the$dir <- "cache"
  dir.create(the$dir, showWarnings = FALSE)
}

.onUnload <- function(libpath) {
  #unlink(the$dir, recursive = TRUE)
}


find_deps <- function(packages,
                      top_dep = TRUE, rec_dep = NA, include_pkgs = TRUE) {

  if (is.null(the$cache)) {
    the$cache <- pkgcache::cranlike_metadata_cache$new(platforms = "source", bioc = TRUE)
  }

  if (length(packages) == 0 || identical(top_dep, FALSE))
    return(character())

  top_dep <- standardise_dep(top_dep)
  rec_dep <- standardise_dep(rec_dep)

  top <- the$cache$deps(packages, dependencies = top_dep, recursive = FALSE)$package

  if (length(rec_dep) != 0 && length(top) > 0) {
    rec <- the$cache$deps(top, dependencies = rec_dep, recursive = TRUE)$package
  } else {
    rec <- character()
  }

  pkgs <- unique(c(top, rec))
  if (!include_pkgs) {
    pkgs <- setdiff(pkgs, packages)
  }

  pkgs
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

src_size <- function(pkgs) {
  cache <- pkgcache::cranlike_metadata_cache$new(platforms = "source", bioc = TRUE)
  unique(cache$list(pkgs)$filesize)
}

bin_size <- function(pkgs, platform = "macos") {
  cache <- pkgcache::cranlike_metadata_cache$new(platforms = "macos", bioc = TRUE)
  cache$list(pkgs)$filesize
}

get_downloads <- function(pkg, when) {
  sum(cranlogs::cran_downloads(pkg, when = when)$count)
}
