#' Plot an owid dataset
#'
#' @description A wrapper around ggplot to provide an quick visualisation of owid data.
#'
#'
#' @param data A tibble returned from `owid()`
#' @param col Either the column number to be treated as the value or a character string specifying the name of the value column. Defaults to 3, which is the first possible value column.
#' @param summarise A logical value. If TRUE, plot takes the mean value. If FALSE, each entity is plotted, it is recommended to use this in conjunction with the filter argument to avoid too many entity's being plotted.
#' @param filter The entity's to include in the plot.
#' @param years The years to be included in the plot.
#' @param show.all A logical value indicating weather all Entities should be included in the plot.
#'
#' @return A ggplot object.
#' @export
#'
#'
#' @examples
#' protein <- owid("protein-efficiency-of-meat-and-dairy-production")
#'
#' owid_plot(protein)
#'
#' human_rights <- owid("human-rights-scores")
#'
#' # Plot average score over time
#' owid_plot(human_rights)
#'
#' # Plot score for a selection of countries
#' owid_plot(human_rights, summarise = FALSE,
#'           filter = c("United Kingdom", "Sweden", "North Korea", "South Korea"))
#'
#'
owid_plot <- function(data = NULL, col = 4, summarise = TRUE, filter = NULL,
                      years = NULL, show.all = FALSE) {

  if (col < 4) {
    stop("col value cannot point to entity, year or code")
  }

  if (!is.numeric(pull(data[, col]))) {
    stop("value column of data must be numeric")
  }

  if (colnames(data)[4] == "date") {
    colnames(data)[4] <- "year"
  }

  if (is.numeric(col)) {
    val_name <- colnames(data)[col]
    colnames(data)[col] <- "value"
  } else {
    val_name <- col
    colnames(data)[colnames(data) == val_name] <- "value"
  }

  if (!is.null(filter)) {
    data <- data %>%
      filter(entity %in% filter)
  }
  if (!is.null(years)) {
    data <- data %>%
      filter(year %in% years)
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

  if (colnames(data)[3] == "year") {
    entities <- unique(data$entity)

    n_entries <- data %>%
      group_by(entity) %>%
      count() %>%
      magrittr::use_series(n) %>%
      max()

    if (n_entries > 1) {
      if (summarise) {
        plot <- data %>%
          group_by(year) %>%
          summarise(value = mean(value, na.rm = TRUE)) %>%
          ggplot2::ggplot(ggplot2::aes(year, value)) +
          ggplot2::geom_line(colour = "#377EB8") +
          ggplot2::labs(title = val_name, x = "", y = "") +
          ggplot2::coord_cartesian(expand = FALSE)

      } else {
        if (length(entities) > 10) {
          if (show.all) {
            warning("show.all is true but the number of entities may be too large to show in a graph. Consider using `show.all = FALSE`")

          } else {
            warning(paste0("Too many entities to plot, plotting a sample of 9 out of ", length(entities),
                           ". Use the filter argument to select which entities are shown."))
            # set.seed(20) # show same countries on repeated calls?
            entities <- sample(entities, 9)

          }

        }
        plot <- data %>%
          filter(entity %in% entities) %>%
          ggplot2::ggplot(ggplot2::aes(year, value, colour = entity)) +
          ggplot2::geom_line() +
          ggplot2::labs(title = val_name, x = "", y = "") +
          ggplot2::coord_cartesian(expand = FALSE)
        if (length(entities) <= 9){
          plot <- plot + ggplot2::scale_colour_brewer(palette="Set1")
        }

      }
    } else {

      if (length(entities) > 20) {
        if (show.all) {
          warning("show.all is true but the number of entities may be too large to show in a graph. Consider using `show.all = FALSE`")
        } else {
          warning(paste("Too many entities to plot, plotting a sample of 20 out of", length(entities)))
          # set.seed(20) # show same countries on repeated calls?
          entities <- sample(entities, 20)
        }
      }

      plot <- data %>%
        filter(entity %in% entities) %>%
        ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(factor(entity), value), value)) +
        ggplot2::geom_col(fill = "#377EB8") +
        ggplot2::labs(title = val_name, x = "", y = "") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, vjust = 1)) +
        ggplot2::coord_cartesian(expand = FALSE)
    }
  }
  return(plot)
}
