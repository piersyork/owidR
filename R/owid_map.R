#' Create a choropleth world map using data from Our World in Data.
#'
#' @description A function to easily create a choropleth world map using data from Our World in Data.
#'
#' @param data A dataframe returned by owid(). This dataframe must have country names in the Entity column, not all data returned by owid() will be like this.
#' @param col The column number to be treated as the value. Defaults to 3.
#' @param palette The RColorBrewer palette to be used.
#' @param mode If "plot", the output will be a ggplot2 map. If "view", the output will be a leaflet interactive map.
#'
#' @return Either a ggplot2 map (for mode = "plot") or a leaflet map (for mode = "view").
#' @export
#'
#' @examples
#' ds <- owid_get_datasets()
#' id <- owid_search(ds, "Mental and substance use disorder")$id
#' mental <- owid(ds, id)
#'
#' # simple ggplot2 map
#' owid_map(mental)
#'
#' # interavtive map with blue palette
#' owid_map(mental, mode = "view", palette = "Blues")
#'
owid_map <- function(data = dataframe(), col = 3, palette = "Reds", mode = "plot") {

  owid_readme(data)

  data <- data %>%
    # group_by(Entity) %>%
    filter(Year == max(Year))
  value <- colnames(data)[col]
  colnames(data)[col] <- "value"

  world <- world_map_data()

  map_data <- world %>%
    left_join(data, by = c("owid_name" = "Entity"))

  # map_data$NAME_LONG

  if (mode == "plot") {
    map_data %>%
      ggplot2::ggplot(ggplot2::aes(fill = value, id = owid_name)) +
      ggplot2::geom_sf(size = 0.05, colour = "black") + #, colour = "#dedfea"
      ggplot2::scale_fill_distiller(palette = palette, direction = 1, na.value = "grey80") +
      ggplot2::labs(title = value) +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank(),
                     legend.key.width = ggplot2::unit(2, units = "cm"),
                     legend.key.height = ggplot2::unit(0.3, units = "cm"),
                     plot.title = ggplot2::element_text(hjust = 0.1, vjust = 0.3, face = "bold"),
                     plot.title.position = "plot")
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
      leaflet::addTiles("", attribution = "<a href = 'https://ourworldindata.org/' title = 'Research and data to make progress against the world’s largest problems'>Our World In Data | <a/><a href = 'https://www.naturalearthdata.com/' title = 'Made with Natural Earth. Free vector and raster map data'>Natural Earth Data<a/>")
  }
}


#' Get world map data.
#'
#' @return An object of class sf.
#' @export
#'
world_map_data <- function() {
  world <- readRDS(system.file("extdata", "world_map_sf.rds", package = "owidR"))
  return(world)
}




