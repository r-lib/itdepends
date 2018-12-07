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
