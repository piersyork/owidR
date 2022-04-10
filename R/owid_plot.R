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
#'
#' human_rights <- owid("human-rights-scores")
#'
#' # Plot average score over time
#' owid_plot(human_rights)
#' \donttest{
#' # Plot score for a selection of countries
#' owid_plot(human_rights, summarise = FALSE,
#'           filter = c("United Kingdom", "Sweden", "North Korea", "South Korea"))
#' }
#'
owid_plot <- function(data = NULL, col = 4, summarise = TRUE, filter = NULL,
                      years = NULL, show.all = FALSE) {

  if (class(data)[1] == "owid.no.connection") {
    message("owid object had not connected to ourworldindata.org")
    return(ggplot())
  }

  data <- as.data.table(data)


  if (col < 4) {
    stop("col value cannot point to entity, year or code")
  }

  if (!is.numeric(data[[col]])) { # need to fix for when col is column name
    stop("value column of data must be numeric")
  }

  if (colnames(data)[3] == "date") {
    colnames(data)[3] <- "year"
  }

  if (is.numeric(col)) {
    val_name <- colnames(data)[col]
    colnames(data)[col] <- "value"
  } else {
    val_name <- col
    colnames(data)[colnames(data) == val_name] <- "value"
  }

  if (!is.null(filter)) {
    data <- data[entity %in% filter]
  }
  if (!is.null(years)) {
    data <- data[year %in% years]
  }

  title <- attributes(data)$data_info[[1]]$display$name

  data$value <- as.numeric(data$value)

  if (colnames(data)[3] == "year") {
    entities <- unique(data$entity)

    n_entries <- data[, .N, by = entity][, max(N)]

    if (n_entries > 1) {
      if (summarise) {
        plot <- data[, .(value = mean(value, na.rm = TRUE)), by = year] %>%
          ggplot2::ggplot(ggplot2::aes(year, value)) +
          ggplot2::geom_line(colour = "#57677D") +
          ggplot2::labs(title = title, x = "", y = "") +
          theme_owid() +
          ggplot2::theme(panel.grid.major.x = element_blank())

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
        max_string_length <- max(nchar(entities))

        data[, label := ifelse(year == max(year), entity, NA), by = entity]

        plot <- data[entity %in% entities] %>%
          ggplot2::ggplot(ggplot2::aes(.data$year, .data$value, colour = .data$entity)) +
          ggplot2::geom_line() +
          ggplot2::labs(title = title, x = "", y = "") +
          theme_owid() +
          coord_cartesian(clip = "off") +
          ggplot2::theme(panel.grid.major.x = element_blank(),
                         plot.margin = margin(11, 6*max_string_length, 5, 10),
                         legend.position = "none") +
          ggrepel::geom_text_repel(aes(label = .data$label),
                                   hjust = 0, xlim = Inf,
                                   na.rm = TRUE, segment.colour = "grey")

        if (length(entities) <= 10){
          plot <- plot + scale_colour_owid()
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

      plot <- data[entity %in% entities] %>%
        ggplot2::ggplot(ggplot2::aes(.data$value,
                                     forcats::fct_reorder(factor(.data$entity), .data$value))) +
        ggplot2::geom_col(fill = "#57677D") +
        ggplot2::labs(title = title, x = "", y = "") +
        theme_owid() +
        ggplot2::theme(panel.grid.major.y = element_blank(),
                       plot.margin = margin(11, 5, 5, 10)) +
        ggplot2::coord_cartesian(expand = FALSE)
    }
  }
  return(plot + ggplot2::theme(axis.title = element_blank()))
}
