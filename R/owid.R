globalVariables(c("name", "code", "years", "values", "entity", "year"))

#' Internal function to get datasets from our world in data
#'
#' @noRd
#'
get_datasets <- function() {
  if (!curl::has_internet()) {
    message("No internet connection available: returning blank tibble")
    return(tibble(titles = NA, urls = NA))
  } else if (httr::http_error("https://ourworldindata.org/charts")) {
    message("Could not connect to https://ourworldindata.org/charts, site may be down. Returning blank tibble")
    return(tibble(titles = NA, urls = NA))
  }

  all_charts_page <- xml2::read_html("https://ourworldindata.org/charts")
  links <- all_charts_page %>%
    rvest::html_nodes("section") %>%
    rvest::html_nodes("a")

  titles <- rvest::html_text(links)
  urls <- rvest::html_attr(links, "href")

  datasets <- tibble(titles, urls) %>%
    filter(grepl("grapher", urls)) %>%
    mutate(urls = stringr::word(urls, 3, -1, sep = "/")) %>%
    rename(chart_id = urls) %>%
    distinct()
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
  ds <- get_datasets()
  ds %>%
    filter(grepl(term, .data$titles, ignore.case = TRUE)) %>%
    as.matrix()
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

  full_urls <- sprintf("https://ourworldindata.org%s", rvest::html_attr(preload, "href"))

  return(full_urls)
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
#' @return A tibble of an owid dataset with the added class 'owid'.
#' @export
#'
#' @import dplyr
#' @importFrom rlang .data
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

  if (!curl::has_internet()) {
    message("No internet connection available: returning blank tibble")
    out <- tibble(entity = NA, year = NA, value = NA)
    class(out) <- c("owid.no.connection", class(out))
    return(out)
  } else if (httr::http_error(paste0("https://ourworldindata.org/grapher/", chart_id))) {
    message(paste0("Could not connect to https://ourworldindata.org/grapher/", chart_id, ", either the chart ID is invalid or the site may be down. Returning blank tibble."))
    out <- tibble(entity = NA, year = NA, value = NA)
    class(out) <- c("owid.no.connection", class(out))
    return(out)
  }

  data_urls <- get_data_url(chart_id)

  year_is_day <- FALSE

  if (length(data_urls) == 0) {
    message("Unable to get data for this chart_id")
    out <- tibble(entity = NA, year = NA, value = NA)
    class(out) <- c("owid.no.connection", class(out))
    return(out)
  } else if (length(data_urls) == 2) {
    df <- jsonlite::fromJSON(data_urls[1])
    metadata <- jsonlite::fromJSON(data_urls[2])

    entities <- as_tibble(metadata$dimensions$entities$values)
    out <- as_tibble(df) %>%
      left_join(entities, by = c("entities" = "id")) %>%
      select(entity = name, code, year = years, values) %>%
      arrange(entity, year)

    if (!is.null(metadata$display$yearIsDay)) year_is_day <- metadata$display$yearIsDay
    if (!is.null(metadata$display$conversionFactor)) out[[4]] <- out[[4]] * metadata$display$conversionFactor

    if (year_is_day & tidy.date) {
      out <- out %>%
        mutate(year = as.Date(metadata$display$zeroDay) + .data$year)
    }

    display_name <- metadata$display$name
    colnames(out)[4] <- if (!is.null(display_name)) display_name else metadata$name


    data_info <- vector(mode = "list", length = 1)
    data_info[[1]]$source <- metadata$source
    data_info[[1]]$dataset_name <- metadata$name
    data_info[[1]]$display <- metadata$display
  } else {
    tables <- grep("variables/data/", data_urls, value = TRUE) %>%
      lapply(\(x) jsonlite::fromJSON(x))

    results <- vector("list", length(tables))

    data_info <- vector(mode = "list", length = length(tables))

    for (i in 1:length(tables)) {
      metadata <- jsonlite::fromJSON(data_urls[i * 2])
      entities <- as_tibble(metadata$dimensions$entities$values)

      results[[i]] <- as_tibble(tables[[i]]) %>%
        left_join(entities, by = c("entities" = "id")) %>%
        select(entity = name, code, year = years, values) %>%
        arrange(entity, year)


      display_name <- metadata$display$name
      colnames(results[[i]])[4] <- if (!is.null(display_name)) display_name else metadata$name


      data_info[[i]]$source <- metadata$source
      data_info[[i]]$dataset_name <- metadata$name
      data_info[[i]]$display <- metadata$display
    }

    out <- purrr::reduce(results, full_join, by = c("entity", "code", "year"))
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

  # data_info <- vector(mode = "list", length = length(colnames(out)[4:length(colnames(out))]))
  # names(data_info) <- colnames(out)[4:length(colnames(out))]
  # for (i in 1:length(metadata)) {
  #   data_info[[i]]$source <- data$variables[[i]]$source
  #   data_info[[i]]$dataset_name <- data$variables[[i]]$datasetName
  #   data_info[[i]]$display <- data$variables[[i]]$display
  # }




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
  if (!curl::has_internet()) {
    message("No internet connection available: returning blank tibble")
    return(tibble())
  } else if (httr::http_error("https://covid.ourworldindata.org/data/owid-covid-data.csv")) {
    message("Could not connect to https://covid.ourworldindata.org/data/owid-covid-data.csv, returning blank tibble")
    return(tibble())
  }

  data <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
    col_types = readr::cols(
      .default = readr::col_double(),
      iso_code = readr::col_character(),
      continent = readr::col_character(),
      location = readr::col_character(),
      date = readr::col_date(format = ""),
      tests_units = readr::col_character()
    )
  )
  class(data) <- c("owid", class(data))
  return(data)
}
