#' Get source information on an OWID dataset
#'
#' @description A function to get source information from an OWID dataset and display it in the R console.
#'
#' @param data A dataset returned from owid().
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

  c(
    "Title: ", data_info$title, "\n\n", "Subtitle: ", data_info$subtitle, "\n\n",
    "Citation: ", data_info$citation, "\n\n", "Link: ", data_info$originalChartUrl, "\n\n", data_info$note
  ) %>%
    cat(sep = "")
  invisible(data_info)
}

#' View an OWID chart in your browser
#'
#' @description A function that opens the original OWID chart in your browser.
#'
#' @param x Either a dataset returned by owid(), or a chart_id.
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
    utils::browseURL(url,
      browser = getOption("browser"),
      encodeIfNeeded = FALSE
    )
  } else {
    stop("utils is required to launch browser, please go to", url, "instead")
  }
}
