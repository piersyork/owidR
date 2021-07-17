#' Get a dataframe of the titles of all owid datasets
#'
#' @description This function web scrapes the OWID datasets github repo to create a dataframe that lists all of the available datasets and gives each one a unique value.
#'
#' @return
#' @export
#'

owid_get_datasets <- function() {

  github_datasets <- rvest::read_html("https://github.com/owid/owid-datasets/tree/master/datasets")

  data_titles <- github_datasets %>%
    rvest::html_nodes(".js-navigation-open") %>%
    rvest::html_text()
  data_urls <- github_datasets %>%
    rvest::html_nodes(".js-navigation-open") %>%
    rvest::html_attr("href")

  data_titles[1:10]
  data_urls[1:10] %>%
    stringr::word(4, -1, sep = "/")

  df_aval <- tibble(title = data_titles[-1:-5], url = data_urls[-1:-5]) %>%
    mutate(id = row_number()) %>%
    relocate(id)

  # return(df_aval)
  # if(!length(owid::datasets) == 3) {
  #   owid <- namespace::makeNamespace("owid")
  # }
  # owid <- namespace::makeNamespace("owid")
  # assign("datasets", df_aval, envir = owid)
  # namespaceExport(owid, ls(owid))
  # owid_datasets <<- df_aval


  data <- df_aval[, -3]
  # class(data) <- c(class(data), "owid_ds")
  attributes(data)$with_urls <- df_aval
  return(data)
}
