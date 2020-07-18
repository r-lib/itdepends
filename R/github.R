get_github_info <- function(package) {
  repo <- github_repo(package)

  if (is.na(repo)) {
    return(tibble::tibble(package, open_issues = NA_real_, last_updated = as.POSIXct(NA_real_, origin = "1970-01-01"), stars = NA_real_, forks = NA_real_))
  }

  res <- gh::gh("GET /repos/:repo", repo = repo)

  tibble::tibble(package, open_issues = res$open_issues_count, last_updated = parse_datetime_8601(res$updated_at), stars = res$stargazers_count, forks = res$forks_count)
}

github_repo <- function(package) {
  desc <- desc::desc(package = package)
  url_fields <- c(desc$get_urls(), desc$get_field("BugReports", default = NULL))
  if (length(url_fields) == 0) {
    return(NA_character_)
  }
  pkg_urls <- unlist(strsplit(url_fields, "[[:space:]]*,[[:space:]]*"))
  pkg_urls <- sub("/issues$", "", pkg_urls)
  valid_domains <- "github[.]com"
  parts <- rematch2::re_match(pkg_urls, sprintf("^https?://(?<domain>%s)/(?<username>%s)/(?<repo>%s)(?:/(?<subdir>%s))?",
      domain = paste0(valid_domains, collapse = "|"), username = "[^/]+",
      repo = "[^/@#]+", subdir = "[^/@$ ]+"))[c("domain", "username",
    "repo", "subdir")]
  parts <- unique(stats::na.omit(parts))
  if (nrow(parts) != 1) {
    warning("Could not determine development repository", call. = FALSE)
    return(NA)
  }
  ref <- paste0(c(parts$username, parts$repo, if (nzchar(parts$subdir)) parts$subdir),
    collapse = "/")
}
