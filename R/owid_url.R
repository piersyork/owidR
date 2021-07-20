#' Get the GitHub and source URLs for a dataset
#'
#' @param data A tibble returned from owid()
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

#' Show the source information of an owid dataset
#'
#' @param data A dataframe returned from owid() of class "owid".
#'
#' @return
#' @export
#'
owid_sources <- function(data) {
  if (!class(data)[1] == "owid") {
    stop("Data must be of class owid")
  }

  sources <- attributes(data)$datapackage$sources

  sou_vec <- lapply(sources, function(x) paste0(x, "\n\n")) %>%
    unlist() %>%
    na.omit()

  sou_names <- sou_vec %>% names()
  sou_names <- c("Name:", "Data Published By:", "Data Publisher Source",
                 "Source Link:", "Date Retrieved:", "Additional Info:")

  sources_out <- vector()
  for (i in 1:length(sou_vec)) {
    n <- i*2
    sources_out[n] <- sou_vec[i]
    sources_out[n-1] <- paste0(sou_names[i], "\n")
  }

  cat(sources_out, sep = "")
}


#' Show the description of the data
#'
#' @param data A dataframe returned from owid() of class "owid".
#'
#' @return
#' @export
#'
owid_description <- function(data) {
  name <- paste0(attributes(data)$datapackage$name, "\n\n")
  description <- attributes(data)$datapackage$description
  cat(name, description, sep = "")

}
