#' Create an Our World in Data style graph.
#'
#' @description Use any dataframe to create your own owid style graphs. The function currently requires an internet connection to work. **Warning:** This function is in very early stages of development, may not be fully functional and may undergo major changes.
#'
#' @param data The dataframe to be used in the graph
#' @param x The column to be displayed on the x axis. Currently only a column of years is supported.
#' @param y The column to be displayed on the y axis.
#' @param entity The column containing the entities included in the dataset, e.g. the country column.
#'
#' @return If executed from within an R script then the graph will be displayed within the viewer. If executed from within an RMarkdown file the graph will be displayed within the output document (only if the output is an html document).
#' @export
#'
#' @examples
#' library(dplyr)
#' owid_search("regime")
#' regime <- owid("political-regime-updated2016")
#'
#' \dontrun{
#' owid_grapher(regime, x = year, y = `Political Regime (OWID based on Polity IV and Wimmer & Min)`,
#'              entity = entity) %>%
#'   grapher_line() %>%
#'   grapher_map(palette = "RdYlGn") %>%
#'   grapher_labels(title = "Political Regime")
#' }
owid_grapher <- function(data, x, y, entity) {

  .x <- enquo(x)
  .value <- enquo(y)
  .entity <- enquo(entity)

  x_label <- as_label(.x)

  is_date <- (x_label == "date") | (x_label == "Date")


  grapher <- data %>%
    select(Entity = !!.entity,
           Year = !!.x,
           value = !!.value) %>%
    # rename(Entity = country,
    #        Year = year,
    #        value = gdp_per_capita) %>%
    stats::na.omit() %>%
    group_by(.data$Entity) %>%
    arrange(.data$Year) %>%
    mutate(id = cur_group_id()) %>%
    ungroup() %>%
    arrange(id) %>%
    # change entity names to match owid spelling if it isn't already. This list should translate work
    # for common other spellings such as World Bank Development index
    mutate(Entity = recode(.data$Entity,
                           "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                           "Congo, Dem. Rep." = "Democratic Republic of Congo",
                           "Republic of the Congo" = "Congo",
                           "Congo, Rep." = "Congo",
                           "Russian Federation" = "Russia",
                           "Lao PDR" = "Laos",
                           "Dem. Rep. Korea" = "North Korea",
                           "Korea, Dem. People's Rep." = "North Korea",
                           "Republic of Korea" = "South Korea",
                           "Korea, Rep." = "South Korea",
                           "The Gambia" = "Gambia",
                           "Brunei Darussalam" = "Brunei",
                           "Venezuela, RB" = "Venezuela",
                           "Egypt, Arab Rep." = "Egypt",
                           "Iran, Islamic Rep." = "Iran",
                           "Yemen, Rep." = "Yemen",
                           "Kyrgyz Republic" = "Kyrgyzstan",
                           "Czech Republic" = "Czechia",
                           "Slovak Republic" = "Slovakia",
                           "Syrian Arab Republic" = "Syria",
                           "Gambia, The" = "Gambia"))

  if (is_date) {
    grapher <- grapher %>%
      mutate(Year = as.numeric(.data$Year))
  }

  if (!is.numeric(grapher$value)) {
    stop("y column must be numeric")
  }


  class(grapher) <- c("grapher", "data.frame")

  attributes(grapher)$type <- ""
  attributes(grapher)$selected <- ""
  attributes(grapher)$title <- as_label(.value)
  attributes(grapher)$subtitle <- ""
  attributes(grapher)$note <- ""
  attributes(grapher)$source <- ""
  attributes(grapher)$include_map <- FALSE
  attributes(grapher)$include_chart <- FALSE
  attributes(grapher)$change_selected <- TRUE
  attributes(grapher)$raw_html <- FALSE
  attributes(grapher)$map_palette <- "Greens"
  attributes(grapher)$map_bins <- "auto"
  attributes(grapher)$is_date <- is_date
  attributes(grapher)$tab <- ""

  return(grapher)

}


#' Add a line graph to the grapher
#'
#' @param grapher An object of class "grapher".
#' @param selected The entities displayed when the graph first loads.
#' @param change_selected Allow the entities to be changed from within the graph.
#'
#' @export
#'
grapher_line <- function(grapher, selected = c("United Kingdom", "France", "Spain", "Ireland"),
                         change_selected = TRUE) {
  attributes(grapher)$selected <- selected
  attributes(grapher)$include_chart <- TRUE
  attributes(grapher)$change_selected <- change_selected

  if (attributes(grapher)$tab == "") {
    attributes(grapher)$tab <- "chart"
  }

  return(grapher)
}

#' Add a map to the grapher
#'
#' @param grapher An object of class "grapher".
#' @param palette An RColorBrewer palette.
#' @param bins The steps in the map legend bins.
#'
#' @export
#'
grapher_map <- function(grapher, palette = "Greens", bins = "auto") {
  attributes(grapher)$map_palette <- palette
  attributes(grapher)$map_bins <- bins
  attributes(grapher)$include_map <- TRUE

  if (attributes(grapher)$tab == "") {
    attributes(grapher)$tab <- "map"
  }

  return(grapher)
}

#' Add labels to the grapher
#'
#' @param grapher An object of class grapher
#' @param title The title of the graph
#' @param subtitle The subtitle displayed underneath the title
#' @param note A footnote for the graph
#' @param source The source of data
#'
#' @export
#'
grapher_labels <- function(grapher, title = "", subtitle = "", note = "", source = "") {
  if (!title == "") attributes(grapher)$title <- title
  attributes(grapher)$subtitle <- subtitle
  attributes(grapher)$note <- note
  attributes(grapher)$source <- source

  return(grapher)
}




