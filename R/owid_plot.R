#' Plot an owid dataset
#'
#' @description A wrapper around ggplot to provide an quick visualisation of owid data.
#'
#'
#' @param data A tibble returned from `owid()`
#' @param col The column number of the value to be plotted. Defaults to 3, and must be at least 3.
#' @param summarise If TRUE, plot takes the mean value. If FALSE, each Entity is plotted, it is recommended to use this in conjunction with the filter argument to avoid too many Entity's being plotted.
#' @param filter The Entity's to include in the plot.
#' @param years The Years to be included in the plot.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' ds <- owid_get_datasets()
#' owid_search(ds, "meat")
#' id <- owid_search(ds, "Meat consumption in EU28") %>% pull(id)
#' meat <- owid(ds, id)
#'
#' owid_plot(meat)
owid_plot <- function(data = NULL, col = 3, summarise = TRUE, filter = NULL, years = NULL) {

  if ("owid" %in% class(data)) {
    owid_readme(data)
  }

  if (col < 3) {
    stop("col value cannot point to Entity or Year")
  }


  val_name <- colnames(data)[col]
  colnames(data)[col] <- "value"

  if (!is.null(filter)) {
    data <- data %>%
      filter(Entity %in% filter)
  }
  if (!is.null(years)) {
    data <- data %>%
      filter(Year %in% years)
  }

  data$value <- as.numeric(data$value)

  owid_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(title = ggplot2::element_text(face = "bold"),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top",
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_line(linetype = "dashed", colour = "grey"),
                   axis.text = ggplot2::element_text(size = 11),
                   plot.margin = ggplot2::margin(r = 15, b = 4, t = 4, l = 2))

  ggplot2::theme_set(owid_theme)

  if (colnames(data)[2] == "Year") {

    if (length(unique(data$Year)) > 1) {
      if (summarise) {
        plot <- data %>%
          group_by(Year) %>%
          summarise(value = mean(value, na.rm = TRUE)) %>%
          ggplot2::ggplot(ggplot2::aes(Year, value)) +
          ggplot2::geom_line(colour = "#377EB8") +
          ggplot2::labs(title = val_name, x = "", y = "") +
          ggplot2::coord_cartesian(expand = FALSE)

      } else {
        plot <- data %>%
          ggplot2::ggplot(ggplot2::aes(Year, value, colour = Entity)) +
          ggplot2::geom_line() +
          ggplot2::labs(title = val_name, x = "", y = "") +
          ggplot2::scale_colour_brewer(palette="Set1") +
          ggplot2::coord_cartesian(expand = FALSE)
      }
    } else {
      entities <- unique(data$Entity)
      if (length(entities) > 20) {
        warning(paste("Too many entities to plot, plotting a sample of 20 out of", length(entities)))
        set.seed(20)
        entities <- sample(entities, 20)
      }

      plot <- data %>%
        filter(Entity %in% entities) %>%
        ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(factor(Entity), value), value)) +
        ggplot2::geom_col(fill = "#377EB8") +
        ggplot2::labs(title = val_name, x = "", y = "") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1)) +
        ggplot2::coord_cartesian(expand = FALSE)
    }
  }
  return(plot)
}
