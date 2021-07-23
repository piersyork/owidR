#' Get data from Our World in Data
#'
#' @description Get OWID datasets from the OWID datasets github repo.
#'
#' @param id Either the id of a dataset or a dataframe returned by owid_search().
#' @param datasets A dataframe returned by get_owid_datasets().
#' @param tidy.date If TRUE then a Year column that should be a Date column will automatically be transformed. If FALSE then the Year column will be kept as is. Defaults to TRUE.
#' @param ... Further arguments passed on to read_csv.
#'
#' @return A tibble of an owid dataset with the added class 'owid'.
#' @export
#'
#' @import dplyr
#'
#' @examples
#' ds <- owid_get_datasets()
#' owid_search(ds, "meat")
#' id <- owid_search(ds, "Meat consumption in EU28")$id
#' meat <- owid(id, ds)
owid <- function(id = NULL, datasets = NULL, tidy.date = TRUE, ...) {

  if (!length(names(attributes(datasets))) > 3) {
    stop ("datasets must be an object returned by 'get_owid_datasets'")
  }
  if (!names(attributes(datasets))[4] == "with_urls") {
    stop ("datasets must be an object returned by 'get_owid_datasets'")
  }

  if (is.data.frame(id)) {
    id <- id$id
  }

  if (is.null(id)) {
    .id <- sample(1:nrow(datasets), size = 1)
  } else if (length(id) > 1) {
    .id <- sample(id, 1)
  } else {
    .id <- id
  }

  url <- attr(datasets, "with_urls") %>%
    filter(id == .id) %>%
    pull(url)

  title <- attr(datasets, "with_urls") %>%
    filter(id == .id) %>%
    pull(title)

  cat(title)

  data_links <- url %>%
    paste0("https://github.com", .) %>%
    rvest::read_html() %>%
    rvest::html_nodes(".Details-content--hidden-not-important") %>%
    rvest::html_nodes(".js-navigation-open") %>%
    # extract(2) %>%
    rvest::html_attr("href") %>%
    stringr::word(6, -1, sep = "/")

  data_link <- grep(".csv", data_links, value = TRUE) %>%
    paste0("https://raw.github.com/owid/owid-datasets/master/", .)
  md_link <- grep("README", data_links, value = TRUE) %>%
    paste0("https://raw.github.com/owid/owid-datasets/master/", .)
  datapackage_link <- grep("datapackage.json", data_links, value = TRUE) %>%
    paste0("https://raw.github.com/owid/owid-datasets/master/", .)

  data <- readr::read_csv(data_link, ...)

  datapackage <- jsonlite::fromJSON(datapackage_link, flatten = TRUE)

  display_settings <- datapackage$resources$schema.fields[[1]]$owidDisplaySettings

  if (TRUE %in% (grepl("zeroDay", display_settings)) & tidy.date) {
    zero_days <- display_settings %>%
      na.omit() %>%
      stringr::str_sub(2, -2) %>%
      stringr::str_replace_all("\\\"", "") %>%
      stringr::str_match("zeroDay: [0-9]+-[0-9]+-[0-9]+") %>%
      na.omit() %>%
      as.vector() %>%
      unique()

    if (length(zero_days) == 1) {
      day_start <- gsub("zeroDay: ", "", zero_days) %>% as.Date()

      data <- data %>%
        mutate(Year = day_start + Year) %>%
        rename(Date = Year)
      warning("Year column automatically tranformed into Date column, use tidy.date = FALSE to keep original.")
    } else {
      warning("Year column might not be a year but could not transform due to ambiguous start date.")
    }
  }


  pasteReadme <- function(fileName){

    breakFun <- function(x){
      #function to replace empty lines with newline.
      if(nchar(x) == 0){
        return("\n\n") #double newline to give same space as in the .md-file
      } else {
        return(x)
      }
    }

    storeLines <- readLines(fileName)

    out <- (paste0(paste0(lapply(storeLines, FUN=function(x) breakFun(x)), collapse=""), "\n"))
    return(out)
  }

  readme <- suppressWarnings(pasteReadme(md_link))
  # readme <- readLines(md_link)

  attributes(data)$url <- paste0("https://github.com", url)
  attributes(data)$readme <- readme
  attributes(data)$datapackage <- datapackage
  # object$data <- data
  class(data) <- c("owid", class(data))
  return(data)

}

