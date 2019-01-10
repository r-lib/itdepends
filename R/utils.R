flat_map_lst <- function(x, f, ...) {
  if (length(x) == 0) {
    NULL
  } else {
    if (typeof(x) == "closure" && !inherits(x, "function")) {
      class(x) <- c(class(x), "function")
    }
    unlist(lapply(x, f, ...), recursive = FALSE, use.names = FALSE)
  }
}

char_or_sym <- function(x) {
  if (is.character(x)) {
    x
  } else if (is.symbol(x)) {
    as.character(x)
  } else {
    character()
  }
}

user_deps <- function() {
  c("Depends", "Imports", "LinkingTo")
}

dev_deps <- function() {
  c("Suggests", "Enhances")
}

parse_datetime_8601 <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ")
}

# This is the same as prettyunits::pretty_sec, but it also passes through NA values
pretty_sec <- function(x) {
  x[!is.na(x)] <- prettyunits::pretty_sec(x[!is.na(x)])
  x
}
