#' Get the GitHub url for the data
#'
#' @param data A tibble returned from `owid()`
#'
#' @return The GitHub URL of the dataset.
#' @export
#'

owid_url <- function(data) {
  if (!class(data)[1] == "owid") {
    stop("Data must be of class owid")
  }
  url <- attributes(data)$url
  return (url)
}
