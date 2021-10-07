#' Create a choropleth world map using data from Our World in Data.
#'
#' @description A function to easily create a choropleth world map using data from Our World in Data.
#'
#' @param data A dataframe returned by owid(). This dataframe must have country names in the entity column, not all data returned by owid() will be like this.
#' @param col Either the column number to be treated as the value or a character string specifying the name of the column. Defaults to 3, which is the first possible value column.
#' @param palette The RColorBrewer palette to be used.
#' @param mode If "plot", the output will be a ggplot2 map. If "view", the output will be a leaflet interactive map.
#' @param year The year to be mapped. Defaults to NULL, which plots the most recent year with data available.
#'
#' @return Either a ggplot2 map (for mode = "plot") or a leaflet map (for mode = "view").
#' @export
#'
#' @import sf
#'
#' @examples
#' \donttest{
#' mental <- owid("share-with-mental-and-substance-disorders")
#'
#' # simple ggplot2 map
#' owid_map(mental)
#'
#' # interavtive map with blue palette
#' owid_map(mental, mode = "view", palette = "Blues")
#' }
owid_map <- function(data = data.frame(), col = 4, palette = "Reds", mode = "plot", year = NULL) {

  # owid_readme(data)

  if (colnames(data)[3] == "date") {
    colnames(data)[3] <- "year"
  }

  if (is.null(year)) {
    data <- data %>%
      # group_by(entity) %>%
      filter(year == max(year))
  } else {
    if (!is.numeric(year)) {
      stop("year must be numeric")
    } else if (!year %in% unique(data$year)) {
      stop(paste("There is no data for", year))
    }
    data <- data %>%
      filter(year == year)
  }


  if (is.numeric(col)) {
    value <- colnames(data)[col]
    colnames(data)[col] <- "value"
  } else {
    value <- col
    colnames(data)[colnames(data) == value] <- "value"
  }


  world <- world_map_data()

  map_data <- world %>%
    left_join(data, by = c("owid_name" = "entity"))



  # map_data$NAME_LONG

  if (mode == "plot") {
    map_data %>%
      ggplot2::ggplot(ggplot2::aes(fill = value, id = .data$owid_name)) +
      ggplot2::geom_sf(size = 0.05, colour = "black") + #, colour = "#dedfea"
      ggplot2::scale_fill_distiller(palette = palette, direction = 1, na.value = "grey80") +
      ggplot2::labs(title = value) +
      theme_owid() +
      ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.title = ggplot2::element_blank(),
                     legend.key.width = ggplot2::unit(2, units = "cm"),
                     legend.key.height = ggplot2::unit(0.3, units = "cm"),
                     plot.title = element_text(vjust = 1))
  } else if (mode == "view") {
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = map_data$value
    )
    pal_leg <- leaflet::colorNumeric(
      palette = palette,
      domain = map_data$value,
      na.color = NA
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>%g",
      map_data$owid_name, map_data$value
    ) %>% lapply(htmltools::HTML)

    map_data %>%
      leaflet::leaflet() %>%
      leaflet:: addPolygons(
        fillColor = ~pal(value),
        weight = 0.2,
        opacity = 1,
        color = "black",
        dashArray = "1",
        fillOpacity = 0.7,
        highlight = leaflet::highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = leaflet::labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      leaflet::addLegend(pal = pal_leg, values = ~value, opacity = 0.7, title = NULL,
                         position = "bottomleft", labFormat = leaflet::labelFormat()) %>%
      leaflet::addControl(paste0("<b>", value, "<b/>"), position = "topright") %>%
      leaflet::addTiles("", attribution = "<a href = 'https://ourworldindata.org/' title = 'Research and data to make progress against the world\u2019s largest problems'>Our World In Data | <a/><a href = 'https://www.naturalearthdata.com/' title = 'Made with Natural Earth. Free vector and raster map data'>Natural Earth Data<a/>")
  }
}


#' Get world map data.
#'
#' @description Function that returns a simple feature collection of class sf. Map data is from naturalearthdata.com. Designed to be used internally.
#'
#' @return An object of class sf.
#' @export
#'
world_map_data <- function() {
  world <- readRDS(system.file("extdata", "world_map_sf.rds", package = "owidR"))
  return(world)
}




