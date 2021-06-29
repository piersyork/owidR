#' Get the GitHub url for the data
#'
#' @param data A tibble returned from `owid`
#'
#' @return The GitHub url as a character string.
#' @export
#'

owid_get_url <- function(data) {
  if (!class(data)[1] == "owid") {
    stop("Data must be of class owid")
  }
  url <- attributes(data)$url
  return (url)
}
