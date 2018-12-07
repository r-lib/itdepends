#' Locate calls to a particular dependency
#'
#' @param pkg The package we should locale calls from
#' @param path Project path to search in.
#' @export
dep_locate <- function(pkg, path = ".") {
  dep_locater <- function(source_file) {
    function_linter(source_file,
      funcs = pkg_ls(pkg),
      type = "warning",
      msg = paste0(pkg, "::%s"),
      linter = "dep_locater")
  }

  lint_project(path, linters = list(dep_locater = dep_locater))
}

function_linter <- function(source_file, funcs, type,
  msg, linter) {
   bad <- which(
    source_file$parsed_content$token == "SYMBOL_FUNCTION_CALL" &
    source_file$parsed_content$text %in% funcs
  )
  # TODO: handle foo::bar calls
   lapply(
    bad,
    function(line) {
      parsed <- source_file$parsed_content[line, ]
      msg <- gsub("%s", source_file$parsed_content$text[line], msg)
      lintr::Lint(
        filename = source_file$filename,
        line_number = parsed$line1,
        column_number = parsed$col1,
        type = type,
        message = msg,
        line = source_file$lines[as.character(parsed$line1)],
        ranges = list(c(parsed$col1, parsed$col2)),
        linter = linter
      )
    }
  )
}

proj_critera <- function() {
  rprojroot::has_file(".here") |
    rprojroot::is_rstudio_project |
    rprojroot::is_r_package |
    rprojroot::is_git_root |
    rprojroot::is_remake_project |
    rprojroot::is_projectile_project
}

proj_find <- function(path = ".") {
  tryCatch(
    rprojroot::find_root(proj_critera(), path = path),
    error = function(e) path
  )
}

proj_files <- function(path = ".") {
  path <- proj_find(path)

  dir(path = file.path(path, c("R", "tests", "inst")),
    pattern = "[.][Rr](?:md)?$", recursive = TRUE,
    full.names = TRUE)
}


lint_project <- function(path = ".", ...) {
  path <- proj_find(path)

  files <- proj_files(path)

  lints <- lintr:::flatten_lints(lapply(files, function(file) {
      if (interactive()) {
        message(".", appendLF = FALSE)
      }
      lintr::lint(file, ..., parse_settings = FALSE)
  }))

  lints <- lintr:::reorder_lints(lints)
  lints[] <- lapply(lints, function(x) {
    x$filename <- sub(paste0(path, .Platform$file.sep), "", x$filename, fixed = TRUE)
    x
  })

  attr(lints, "path") <- path
  class(lints) <- "lints"
  lints
}

pkg_ls <- function(pkg) {
  ns <- getNamespace(pkg)
  exports <- getNamespaceExports(ns)

  names <- intersect(exports, ls(envir = ns, all.names = TRUE, sorted = FALSE))
  grep("^.__", names, invert = TRUE, value = TRUE)
}

#' Count calls by package for a package
#'
#' @inheritParams dep_locate
#' @export
dep_count_pkg <- function(pkg) {#}, recursive = TRUE) {
  imp <- getNamespaceImports(pkg) %||% list("base" = TRUE)

  full_imports <- purrr::map_lgl(imp, isTRUE)
  imp[full_imports] <- purrr::map(names(imp)[full_imports], pkg_ls)

  fun_to_pkg <- stats::setNames(
    rep(names(imp), lengths(imp)),
    unlist(imp, use.names = FALSE)
  )

  pkg_funs <- mget(ls(envir = asNamespace(pkg), all.names = TRUE, sorted = FALSE), envir = asNamespace(pkg), mode = "function", inherits = TRUE, ifnotfound = NA)

  pkg_calls <- do.call(rbind, c(lapply(pkg_funs, dep_count_lang), make.row.names = FALSE, stringsAsFactors = FALSE))

  # TODO: get this passing a proper NA
  missing_pkg <- pkg_calls$pkg == "NA"
  pkg_calls$pkg[missing_pkg] <- fun_to_pkg[pkg_calls$fun[missing_pkg]]

  # If anything is still missing it must be from the pkg
  ours <- is.na(pkg_calls$pkg)
  pkg_calls$pkg[ours] <- pkg

  #if (isTRUE(recursive)) {
    #searched_pkgs <<- c(pkg, searched_pkgs)
    #to_search <- setdiff(unique(pkg_calls$pkg), searched_pkgs)
    #if (length(to_search) > 0) {
      #pkg_calls <- rbind(pkg_calls, lapply(to_search, dep_count_pkg))
    #}
  #}
  pkg_calls
}

#' Count calls by package for a project
#'
#' @inheritParams dep_locate
#' @export
dep_count_proj <- function(path = ".") {
  files <- proj_files(path)

  default_pkgs <- c("base", strsplit(Sys.getenv("R_DEFAULT_PACKAGES"), ",")[[1]])

  pkgs <- c(default_pkgs, unlist(lapply(files, requirements::req_file)))

  funs <- purrr::map(pkgs, pkg_ls)

  fun_to_pkg <- stats::setNames(
    rep(pkgs, lengths(funs)),
    unlist(funs, use.names = FALSE)
  )

  pkg_calls <- do.call(rbind, c(lapply(files, dep_count_file), make.row.names = FALSE, stringsAsFactors = FALSE))

  # TODO: get this passing a proper NA
  missing_pkg <- pkg_calls$pkg == "NA"
  pkg_calls$pkg[missing_pkg] <- fun_to_pkg[pkg_calls$fun[missing_pkg]]

  pkg_calls
}

dep_count_file <- function(file) {
  exprs <- parse(file = file)
  dep_count_lang(exprs)
}


#' @import rlang
dep_count_lang <- function(x) {
  f <- function(x) {
    if (is_syntactic_literal(x) || is_symbol(x)) {
      return(NULL)
    }

    if (is_pairlist(x) || is.expression(x)) {
      return(flat_map_lst(x, f))
    }

    if (is_call(x, c("::", ":::"))) {
      return(list(pkg = char_or_sym(x[[2]]), fun = char_or_sym(x[[3]])))
    }

    if (is_call(x) && length(x[[1]]) == 1) {
      return(
        c(
          list(pkg = NA, fun = char_or_sym(x[[1]])),
          flat_map_lst(x, f)
          )
        )
    }

    flat_map_lst(x, f)
  }

  res <- f(x)
  if (length(res) > 0) {
    data.frame(
      pkg = as.character(res[seq(1, length(res), 2)]),
      fun = as.character(res[seq(2, length(res), 2)]), stringsAsFactors = FALSE)
  }
}

get_parse_data <- function(file) {
  p <- parse(file = file, keep.source = TRUE)
  xml2::read_xml(xmlparsedata::xml_parse_data(p))
}
