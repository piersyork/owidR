#' Search for terms in dataframe of OWID datasets
#'
#' @description Search through dataframe of OWID datasets using key word or regular expression.
#'
#' @param datasets A dataframe returned by owid_get_datasets().
#' @param term A search term. owid_search() uses grep with case ignored, so either a key word or a regular expression is acceptable.
#'
#' @return A tibble with the dataset title's matching the search.
#' @export
#'
#' @examples
#' ds <- owid_get_datasets()
#' owid_search(ds, "meat")
#' # Currently id is generated from the rownumber, as such id's are not fixed and future
#' # data uploads will change the id. It is recommended to full specify the name of the dataset and
#' # then pull the id from that search.
#' id <- owid_search(ds, "Meat consumption in EU28")$id
#' meat <- owid(id, ds)
owid_search <- function(datasets, term = NULL) {
  if (!length(names(attributes(datasets))) > 3) {
    stop ("datasets must be an object returned by 'get_owid_datasets'")
  }
  if (!names(attributes(datasets))[4] == "with_urls") {
    stop ("datasets must be an object returned by 'get_owid_datasets'")
  }
  if (is.null(term)) {
    stop ("a search term must be given")
  }
  out <- datasets %>%
    filter(grepl(term, title, ignore.case = TRUE))
  return(out)
}
