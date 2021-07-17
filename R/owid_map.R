#' Title
#' Create a world map chloropleth of owid data.
#'
#' @description A function to easily create a world map chloropleth of owid data.
#'
#' @param data A tibble returned by `owid()`. This tibble must have country names as the Entity, not all data returned by `owid()` will be like this.
#' @param col The column number to be treated as the value.
#' @param palette The `RColorBrewer` palette to be used.
#'
#' @return Either a ggplot
#' @export
#'
#' @examples
#' `owid_map()`
owid_map <- function(data, col = 3, palette = "Reds", mode = "plot") {

  owid_readme(data)

  data <- data %>%
    # group_by(Entity) %>%
    filter(Year == max(Year))
  value <- colnames(data)[col]
  colnames(data)[col] <- "value"




  world <- readRDS(system.file("extdata", "world_map_sf.rds", package = "owidR")) %>%
    mutate(owid_name = recode(NAME_LONG,
                              "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                              "Republic of the Congo" = "Congo",
                              "Côte d'Ivoire" = "Cote d'Ivoire",
                              "Russian Federation" = "Russia",
                              "Lao PDR" = "Laos",
                              "Dem. Rep. Korea" = "North Korea",
                              "Republic of Korea" = "South Korea"))

  world$NAME_LONG %>% grep("Korea", ., value = TRUE)
  data$Entity %>% unique() %>% grep("Korea", ., value = TRUE)

  test <- ggplot2::ggplot(world) +
    ggplot2::geom_sf()
  # world <- map_data("world") %>%
  #   select(lon = long, lat, group, region) %>%
  #   filter(region != "Antarctica")


  # world$region <- recode(world$region,
  #                        "USA" = "United States",
  #                        "UK" = "United Kingdom",
  #                        "Republic of Congo" = "Congo",
  #                        "Democratic Republic of the Congo" = "Democratic Republic of Congo",
  #                        "Ivory Coast" = "Cote d'Ivoire")


  map_data <- world %>%
    left_join(data, by = c("owid_name" = "Entity"))

  # map_data$NAME_LONG

  if (mode == "plot") {
    map_data %>%
      ggplot2::ggplot(ggplot2::aes(fill = value, id = owid_name)) +
      ggplot2::geom_sf(size = 0.05) + #, colour = "#dedfea"
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
    tmap::tmap_mode("view")
    map_data %>%
      tmap::qtm("value")
  }


}
