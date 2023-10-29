globalVariables(c("name", "code", "years", "values", "entity", "year", ".", "title"))

#' Internal function to check whether there is internet, if there is internet check whether we can connect to owid
#'
#' @param url The url to check the connection to
#'
#' @noRd
#'
#' @return boolean
#'
#'
check_internet <- function(url) {
  out <- FALSE
  if (!curl::has_internet()) {
    message("No internet connection available: returning blank data.table")
  } else if (httr::http_error(url)) {
    message(paste0("Could not connect to ", url, ", site may be down. Returning blank data.table"))
  } else {
    out <- TRUE
  }
  out
}

#' Internal function to get datasets from our world in data
#'
#' @noRd
#'
#' @import data.table
#'
get_datasets <- function() {
  if (!check_internet("https://ourworldindata.org/charts")) {
    return(data.table(chart_id = NA, title = NA))
  }

  all_charts_page <- xml2::read_html("https://ourworldindata.org/charts")
  links <- all_charts_page %>%
    rvest::html_nodes("section") %>%
    rvest::html_nodes("a")

  titles <- rvest::html_text(links)
  urls <- rvest::html_attr(links, "href")

  datasets <- unique(data.table(titles, urls)[
    grepl("grapher", urls),
    .(chart_id = gsub("/grapher/", "", urls), title = titles)
  ])

  return(datasets)

}

#' Search the data sources used in OWID charts
#'
#' @param term A search term
#'
#' @return A matrix of chart titles and chard ids
#' @export
#'
#' @examples
#' \donttest{
#' # returns the titles and chart_ids of all charts containing the word 'emissions'
#' owid_search("emissions")
#' }
#'
owid_search <- function(term) {
  as.matrix(get_datasets()[grepl(term, title, ignore.case = TRUE)])
}

#' Internal function to get the dataset url
#'
#' @description thank you to Edouard Mathieu from OWID for this idea
#' @param chart_id
#'
#' @return The url to the page with json data
#'
#' @noRd
#'
get_data_url <- function(chart_id) {
  url <- sprintf("https://ourworldindata.org/grapher/%s", chart_id)
  page <- xml2::read_html(url)
  links <- rvest::html_nodes(page, "link")

  preload <- links[rvest::html_attr(links, "rel") == "preload"]

  all_urls <- rvest::html_attr(preload, "href")

  json_urls <- grep("json$", all_urls, value = TRUE)

  return(json_urls)
}

#' Get data from Our World in Data
#'
#' @description Get a dataset used in an OWID chart.
#'
#' @param chart_id The chart_id as returned by owid_search
#' @param rename Rename the value column. Currently only works if their is just one value column.
#' @param tidy.date If TRUE then a year column that should be a date column will automatically detected and transformed. If FALSE then the Year column will be kept as is. Defaults to TRUE.
#' @param ... Not to be used.
#'
#' @return A data.table of an owid dataset with the added class 'owid'.
#' @export
#'
#' @import magrittr
#'
#' @examples
#' \donttest{
#' owid_search("emissions")
#' emissions <- owid("per-capita-ghg-emissions")
#' }
#'
owid <- function(chart_id = NULL, rename = NULL, tidy.date = TRUE, ...) {
  if (is.null(chart_id)) {
    datasets <- get_datasets()
    random_no <- sample(nrow(datasets), 1)
    chart_id <- datasets$chart_id[random_no]
  }

  if (!check_internet(paste0("https://ourworldindata.org/grapher/", chart_id))) {
    out <- data.table(entity = NA, year = NA, value = NA)
    class(out) <- c("owid.no.connection", class(out))
    return(out)
  }

  year_is_day <- FALSE

  data_urls <- get_data_url(chart_id)

  if (length(data_urls) == 0) {
    message("Unable to get data for this chart_id")
    out <- data.table(entity = NA, year = NA, value = NA)
    class(out) <- c("owid.no.connection", class(out))
    return(out)
  } else if (length(data_urls) == 2) {
    df <- jsonlite::fromJSON(data_urls[1])
    metadata <- jsonlite::fromJSON(data_urls[2])

    entities <- data.table(metadata$dimensions$entities$values)
    out <- merge(as.data.table(df), entities, by.x = "entities", by.y = "id", all.x = TRUE)[
      order(name, years),
      .(entity = name, code, year = years, values)
    ]


    if (!is.null(metadata$display$yearIsDay)) year_is_day <- metadata$display$yearIsDay
    if (!is.null(metadata$display$conversionFactor)) out[[4]] <- out[[4]] * metadata$display$conversionFactor

    if (year_is_day & tidy.date) {
      out[, year := as.Date(metadata$display$zeroDay) + year]
    }

    display_name <- metadata$display$name
    colnames(out)[4] <- if (!is.null(display_name)) display_name else metadata$name

    data_info <- vector(mode = "list", length = 1)
    data_info[[1]]$source <- metadata$source
    data_info[[1]]$dataset_name <- metadata$name
    data_info[[1]]$display <- metadata$display
  } else {
    tables <- grep(".*\\.data\\.json$", data_urls, value = TRUE) %>%
      lapply(\(x) jsonlite::fromJSON(x))

    results <- vector("list", length(tables))

    data_info <- vector(mode = "list", length = length(tables))

    for (i in 1:length(tables)) {
      metadata <- jsonlite::fromJSON(data_urls[i * 2])
      entities <- as.data.table(metadata$dimensions$entities$values)

      results[[i]] <- merge(as.data.table(tables[[i]]), entities, by.x = "entities", by.y = "id", all.x = TRUE)[
        order(name, years),
        .(entity = name, code, year = years, values)
      ]


      display_name <- metadata$display$name
      colnames(results[[i]])[4] <- if (!is.null(display_name)) display_name else metadata$name


      data_info[[i]]$source <- if (is.null(metadata$source)) "" else metadata$source
      data_info[[i]]$dataset_name <- metadata$name
      data_info[[i]]$display <- metadata$display
    }

    out <- purrr::reduce(results, merge, by = c("entity", "code", "year"), all = TRUE)
  }

  if (year_is_day & tidy.date) {
    colnames(out)[3] <- "date"
  }

  n_rename <- length(rename)
  n_values <- length(colnames(out)) - 3

  if (!is.null(rename)) {
    if (!is.character(rename)) {
      stop("rename must be of class character")
    }
    if (n_rename == n_values) {
      colnames(out)[4:(3 + n_values)] <- rename
    } else {
      stop(paste0("Length of rename must be the same us number of value columns"))
    }
  }

  attributes(out)$data_info <- data_info
  attributes(out)$chart_id <- chart_id
  class(out) <- c("owid", class(out))

  return(out)
}

#' Get the Our World in Data covid-19 dataset
#'
#' @return A dataframe with multiple variables on the covid-19 pandemic.
#' @export
#'
owid_covid <- function() {
  if (!check_internet("https://covid.ourworldindata.org/data/owid-covid-data.csv")) {
    return(data.table())
  }

  data <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
  class(data) <- c("owid", class(data))
  return(data)
}
