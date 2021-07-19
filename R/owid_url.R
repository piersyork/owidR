#' Get the GitHub and source URLs for a dataset
#'
#' @param data A tibble returned from `owid()`
#'
#' @return Returns a named vector of the owid GitHub URL and the source URL.
#' @export
#'

owid_url <- function(data) {
  if (!class(data)[1] == "owid") {
    stop("Data must be of class owid")
  }
  github_url <- attributes(data)$url
  source_url <- attributes(data)$datapackage$sources$link

  cat("\nGitHub URL:\n",
      github_url, "\n\n",
      "Source URL:\n",
      source_url, sep = "")

  invisible (c(github_url = github_url, source_url = source_url))
}
