#' Get source information on an OWID dataset
#'
#' @description A function to get source information from an OWID dataset and display it in the R console.
#'
#' @param data A tibble returned from owid().
#'
#' @return Displays the information in an easy to read format in the R console, also returns a list of data information.
#' @export
#'
#' @examples
#' \donttest{
#' rights <- owid("human-rights-scores")
#' owid_source(rights)
#' }
owid_source <- function(data) {

  if (class(data)[1] == "owid.no.connection") {
    message("owid object had not connected to ourworldindata.org")
    return(NULL)
  }

  if (!"owid" %in% class(data)) {
    stop("data must be of class 'owid', returned from `owid()`")
  }

  attrs <- attributes(data)
  data_info <- attributes(data)$data_info

  which_identical <- vector()
  for (i in 1:length(data_info)) {
    which_identical[i] <- identical(data_info[[1]], data_info[[i]])
  }
  all_identical <- if (FALSE %in% unique(which_identical)) FALSE else TRUE

  if (all_identical) {
    source <- data_info[[1]]$source
    c("Dataset Name: ", source$name, "\n\n", "Published By: ", source$dataPublishedBy, "\n\n",
      "Link: ", source$link, "\n\n", source$additionalInfo) %>%
      cat(sep = "")
  } else {
    for (i in 1:length(data_info)) {
      cat("Value: ", names(data_info)[i], "\n\n")
      source <- data_info[[i]]$source
      c("Dataset Name: ", source$name, "\n\n", "Published By: ", source$dataPublishedBy, "\n\n",
        "Link: ", source$link, "\n\n", source$additionalInfo) %>%
        cat(sep = "")
    }
  }
  invisible(data_info)
}

#' View an OWID chart in your browser
#'
#' @description A function that opens the original OWID chart in your browser.
#'
#' @param x Either a tibble returned by owid(), or a chart_id.
#'
#' @return Opens the chart in your browser.
#'
#' @export
#'
#' @examples
#' \donttest{
#' firearm_suicide <- owid("suicide-rate-by-firearm")
#' view_chart(firearm_suicide)
#' }
view_chart <- function(x) {
  if (class(x)[1] == "owid.no.connection") {
    message("owid object had not connected to ourworldindata.org")
    return(NULL)
  }

  if (requireNamespace("utils", quietly = TRUE)) {
    if ("owid" %in% class(x)) {
      chart_id <- attributes(x)$chart_id
    } else {
      chart_id <- x
    }
    url <- paste0("https://ourworldindata.org/grapher/", chart_id)
    utils::browseURL(url, browser = getOption("browser"),
                     encodeIfNeeded = FALSE)
  } else {
    stop("utils is required to launch browser, please go to", url, "instead")
  }

}


