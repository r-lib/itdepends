plot <- function(x) {
  out <- c(median_install_time(x$timings),
    median_install_time(x$user_dep_timings[[1]]),
    median_install_time(x$dev_dep_timings[[1]])
  )

  user_deps <- names(x$user_dep_timings[[1]])
  dev_deps <- names(x$dev_dep_timings[[1]])

  df <- tibble(
    package = names(out),
    time = out,
    type = case_when(
      package == x$package ~ "self",
      package %in% user_deps ~ "user",
      package %in% dev_deps ~ "dev"
    )
  )

  tibble::rownames_to_column(out, "package")


  #out <- dplyr::bind_rows(x$timings[[1]], x$user_dep_timings[[1]], x$dev_dep_timings[[1]])
  #out$type <- dplyr::case_when(
    #package
    #)
  #ggplot(out, aes(fill = ifelse())x$user_dep_timings, aes(x = )
}

# TODO: use binary size rather than install time
#' Plot of dependency weights
#'
#' @param pkg package to plot
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom forcats fct_relevel fct_reorder
#' @import ggplot2
#' @importFrom stringr str_glue
#' @export
dep_plot_time <- function(pkg) {

  direct_user_deps <- find_deps(pkg, top_dep = NA, rec_dep = FALSE, include_pkgs = FALSE)

  indirect_user_deps <- find_deps(pkg, top_dep = NA, rec_dep = NA, include_pkgs = FALSE) %>%
    setdiff(direct_user_deps)

  direct_dev_deps <- find_deps(pkg, top_dep = TRUE, rec_dep = FALSE, include_pkgs = FALSE) %>%
    setdiff(c(direct_user_deps, indirect_user_deps))

  indirect_dev_deps <- find_deps(pkg, top_dep = TRUE, rec_dep = NA, include_pkgs = FALSE) %>%
    setdiff(c(direct_user_deps, indirect_user_deps, direct_dev_deps))

  raw_weights <- bind_rows(
    tibble(package = pkg,  dep_type = "user", direct = "self"),
    tibble(package = direct_user_deps,  dep_type = "user", direct = "direct"),
    tibble(package = indirect_user_deps,  dep_type = "user", direct = "indirect"),
    tibble(package = direct_dev_deps,  dep_type = "dev", direct = "direct"),
    tibble(package = indirect_dev_deps,  dep_type = "dev", direct = "indirect"),
  )

  raw_weights$time <- vapply(raw_weights$package, function(x) median_install_time(get_timings(x)), numeric(1))

  weights <- raw_weights %>%
    mutate(
      package = fct_relevel(fct_reorder(package, time), pkg, after = Inf),
      dep_type = fct_relevel(dep_type, "user"),
      direct = fct_relevel(direct, "self")
    )

  ggplot(weights, aes(x = package, y = time, fill = direct)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = pretty_sec) +
    coord_flip() +
    facet_wrap(vars(dep_type), scales = "free") +
    labs(
      title = str_glue("{{{pkg}}} dependency weight"),
      subtitle = "median installation time",
      x = NULL,
      y = "seconds"
    )
}

#' @inheritParams dep_plot_timing
#' @export
dep_plot_size <- function(pkg, platforms = "source") {

  direct_user_deps <- find_deps(pkg, top_dep = NA, rec_dep = FALSE, include_pkgs = FALSE)

  indirect_user_deps <- find_deps(pkg, top_dep = NA, rec_dep = NA, include_pkgs = FALSE) %>%
    setdiff(direct_user_deps)

  direct_dev_deps <- find_deps(pkg, top_dep = TRUE, rec_dep = FALSE, include_pkgs = FALSE) %>%
    setdiff(c(direct_user_deps, indirect_user_deps))

  indirect_dev_deps <- find_deps(pkg, top_dep = TRUE, rec_dep = NA, include_pkgs = FALSE) %>%
    setdiff(c(direct_user_deps, indirect_user_deps, direct_dev_deps))

  raw_weights <- bind_rows(
    tibble(package = pkg, filesize = bin_size(pkg), dep_type = "user", direct = "self"),
    tibble(package = direct_user_deps, filesize = bin_size(direct_user_deps), dep_type = "user", direct = "direct"),
    tibble(package = indirect_user_deps, filesize = bin_size(indirect_user_deps), dep_type = "user", direct = "indirect"),
    tibble(package = direct_dev_deps, filesize = bin_size(direct_dev_deps), dep_type = "dev", direct = "direct"),
    tibble(package = indirect_dev_deps, filesize = bin_size(indirect_dev_deps), dep_type = "dev", direct = "indirect"),
  )

  weights <-
    weights <- raw_weights %>%
    mutate(
      package = fct_relevel(fct_reorder(package, filesize), pkg, after = Inf),
      dep_type = fct_relevel(dep_type, "user"),
      direct = fct_relevel(direct, "self")
    )

  ggplot(weights, aes(x = package, y = filesize, fill = direct)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = prettyunits::pretty_bytes) +
    coord_flip() +
    facet_wrap(vars(dep_type), scales = "free") +
    labs(
      title = str_glue("{{{pkg}}} dependency weight"),
      subtitle = "binary size",
      x = NULL,
      y = "seconds"
    )
}

#' Plot maintainers of dependencies
#'
#' @inheritParams dep_plot_timing
#' @export
dep_plot_maintainer <- function(pkg) {

  direct_user_deps <- find_deps(pkg, top_dep = NA, rec_dep = FALSE, include_pkgs = FALSE)

  indirect_user_deps <- find_deps(pkg, top_dep = NA, rec_dep = NA, include_pkgs = FALSE) %>%
    setdiff(direct_user_deps)

  direct_dev_deps <- find_deps(pkg, top_dep = TRUE, rec_dep = FALSE, include_pkgs = FALSE) %>%
    setdiff(c(direct_user_deps, indirect_user_deps))

  indirect_dev_deps <- find_deps(pkg, top_dep = TRUE, rec_dep = NA, include_pkgs = FALSE) %>%
    setdiff(c(direct_user_deps, indirect_user_deps, direct_dev_deps))

  raw_weights <- bind_rows(
    tibble(package = pkg, dep_type = "user", direct = "self"),
    tibble(package = direct_user_deps,  dep_type = "user", direct = "direct"),
    tibble(package = indirect_user_deps,  dep_type = "user", direct = "indirect"),
    tibble(package = direct_dev_deps,  dep_type = "dev", direct = "direct"),
    tibble(package = indirect_dev_deps,  dep_type = "dev", direct = "indirect"),
  )

  raw_weights$maintainer <- get_maintainer(raw_weights$package)

  lvls <- raw_weights %>%
    count(maintainer) %>%
    mutate(maintainer = fct_reorder(maintainer, n)) %>%
    { levels(.$maintainer) }

  weights <- raw_weights %>%
    count(dep_type, direct, maintainer) %>%
    mutate(
      maintainer = fct_relevel(maintainer, lvls),
      dep_type = fct_relevel(dep_type, "user"),
      direct = fct_relevel(direct, "self")
    )

  ggplot(weights, aes(x = maintainer, y = n, fill = direct)) +
    geom_bar(stat = "identity") +
    #scale_y_continuous(labels = prettyunits::pretty_bytes) +
    coord_flip() +
    facet_wrap(vars(dep_type), scales = "free") +
    labs(
      title = str_glue("{{{pkg}}} dependency weight"),
      subtitle = "binary size",
      x = NULL,
      y = "seconds"
    )
}

get_maintainer <- function(package) {
  purrr::map_chr(package, purrr::possibly(
    function(p) {
      desc::desc(package = p)$get_maintainer() %>%
        sub(" <.*", "", .)
    }, NA_character_)
  )
}

utils::globalVariables(c("maintainer", ".", "dep_type", "direct", "package",
    "filesize", "time", "install_self", "bin_self", "install_user"),
  "itdepends")
