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

lint_project <- function(path = ".", ...) {
  path <- proj_find(path)

  files <- dir(path = file.path(path, c("R", "tests", "inst")),
    pattern = "[.][Rr](?:md)?$", recursive = TRUE,
    full.names = TRUE)

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
