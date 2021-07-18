#' Print the readme for OWID data.
#'
#' @description Prints the readme for OWID data as found on the OWID datasets github repo.
#'
#' @param data An owid dataset.
#'
#' @return
#' @export
#'
#' @examples
#' ds <- owid_get_datasets()
#' id <- owid_search(ds, "YouGov-Imperial COVID-19 Behavior Tracker")$id
#'
#' covid_behav <- owid(id, ds)
#' owid_readme(covid_behav)
owid_readme <- function(data) {
  if (!class(data)[1] == "owid") {
    stop("Data must be of class owid")
  }
  readme <- attributes(data)$readme
  cat(readme)
  # return(readme)
}


