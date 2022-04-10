#' Internal function to get datasets from our world in data
#'
#' @noRd
#'
get_datasets <- function() {

  if (!curl::has_internet()) {
    message("No internet connection available: returning blank tibble")
    return(tibble::tibble(titles = NA, urls = NA))
  } else if (httr::http_error("https://ourworldindata.org/charts")) {
    message("Could not connect to https://ourworldindata.org/charts, site may be down. Returning blank tibble")
    return(tibble::tibble(titles = NA, urls = NA))
  }

  all_charts_page <- xml2::read_html("https://ourworldindata.org/charts")
  links <- all_charts_page %>%
    rvest::html_nodes("section") %>%
    rvest::html_nodes("a")

  titles <- rvest::html_text(links)
  urls <- rvest::html_attr(links, "href")

  datasets <- data.table(titles, urls)[
    grepl("grapher", urls),
    .(urls = stringr::word(urls, 3, -1, sep = "/"), titles)
  ] %>%
    unique()

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
  ds <- get_datasets()

  ds[grepl(term, titles, ignore.case = TRUE)] %>%
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
  full_url <- sprintf("https://ourworldindata.org%s", rvest::html_attr(preload, "href"))
  return(full_url)
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
#' @import data.table
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' owid_search("emissions")
#' emissions <- owid("per-capita-ghg-emissions")
#' }
#'
#'
owid <- function(chart_id = NULL, rename = NULL, tidy.date = TRUE, ...) {

  if (is.null(chart_id)) {
    datasets <- get_datasets()
    random_no <- sample(nrow(datasets), 1)
    chart_id <- datasets$chart_id[random_no]
  }

  if (!curl::has_internet()) {
    message("No internet connection available: returning blank tibble")
    out <- tibble::tibble(entity = NA, year = NA, value = NA)
    class(out) <- c("owid.no.connection", class(out))
    return(out)

  } else if (httr::http_error(paste0("https://ourworldindata.org/grapher/", chart_id))) {
    message(paste0("Could not connect to https://ourworldindata.org/grapher/", chart_id, ", either the chart ID is invalid or the site may be down. Returning blank tibble."))
    out <- tibble::tibble(entity = NA, year = NA, value = NA)
    class(out) <- c("owid.no.connection", class(out))
    return(out)
  }

  data_url <- get_data_url(chart_id)


  data <- jsonlite::fromJSON(data_url)


  datasets <- list()
  for (i in 1:length(data$variables)) {
    val_name <- data$variables[[i]]$name

    datasets[[i]] <- data.table(
      entity_id = data$variables[[i]]$entities,
      year = data$variables[[i]]$years,
      value = data$variables[[i]]$values
    )
    colnames(datasets[[i]])[3] <- val_name

    yearIsDay <- if (is.null(data$variables[[i]]$display$yearIsDay)) {
      FALSE
    } else (data$variables[[i]]$display$yearIsDay)

    if (yearIsDay & tidy.date) {
      # colnames(datasets[[i]])[2] <- "date"
      # datasets[[i]] <- datasets[[i]] %>%
      #   mutate(year = as.Date(data$variables[[i]]$display$zeroDay) + .data$year)

      # datasets[[i]]$year <- datasets[[i]]$year + as.Date(data$variables[[i]]$display$zeroDay)

      datasets[[i]][, year := (as.Date(data$variables[[i]]$display$zeroDay) + year)]

    }

    if (colnames(datasets[[i]])[3] == "Countries Continents") {
      colnames(datasets[[i]])[3] <- "continent"
      # datasets[[2]] <- datasets[[2]][-2]
    }

  }
  all_data <- purrr::reduce(datasets, merge, by = c("entity_id", "year")) #%>%
    # arrange(desc(.data$year))

  entity <- vector(length = length(data$entityKey))
  code <- vector(length = length(data$entityKey))
  for (i in 1:length(data$entityKey)) {
    entity <- c(entity, data$entityKey[[i]]$name)
    if (is.null(data$entityKey[[i]]$code)) {
      code <- c(code, NA)
    } else {
      code <- c(code, data$entityKey[[i]]$code)
    }
  }
  entity_key <- data.table(
    entity_id = as.numeric(names(data$entityKey)),
    entity,
    code
  )

  # data$variables[[1]]$name
  all_data[entity_key, c("entity", "code") := .(entity, code),  on = "entity_id"] # left join entity key
  all_data[, entity_id := NULL] # remove entity_id
  setcolorder(all_data, c("entity", "code", names(all_data)[1:(length(names(all_data)) - 2)])) # set order of cols

  out <- all_data[order(entity, year)] %>%
    tibble::as_tibble()

  # out <- all_data %>%
  #   left_join(entity_key, by = "entity_id") %>%
  #   select(-.data$entity_id) %>%
  #   relocate(entity, code, .data$year) %>%
  #   arrange(entity, .data$year)

  if (yearIsDay & tidy.date) {
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

  data_info <- vector(mode = "list", length = length(colnames(out)[4:length(colnames(out))]))
  names(data_info) <- colnames(out)[4:length(colnames(out))]
  for (i in 1:length(data$variables)) {
    data_info[[i]]$source <- data$variables[[i]]$source
    data_info[[i]]$dataset_name <- data$variables[[i]]$datasetName
    data_info[[i]]$display <- data$variables[[i]]$display
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
  if (!curl::has_internet()) {
    message("No internet connection available: returning blank tibble")
    return(tibble::tibble())
  } else if (httr::http_error("https://covid.ourworldindata.org/data/owid-covid-data.csv")) {
    message("Could not connect to https://covid.ourworldindata.org/data/owid-covid-data.csv, returning blank tibble")
    return(tibble::tibble())
  }

  data <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                          col_types = readr::cols(.default = readr::col_double(),
                                                  iso_code = readr::col_character(),
                                                  continent = readr::col_character(),
                                                  location = readr::col_character(),
                                                  date = readr::col_date(format = ""),
                                                  tests_units = readr::col_character()))
  class(data) <- c("owid", class(data))
  return(data)
}
