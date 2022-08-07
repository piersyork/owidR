#' Internal function to get the fonts used for Our World in Data graphs
#'
#' @return Allows access to new fonts
#'
#' @noRd
get_owid_fonts <- function() {
  get_file <- function(url) {
    dest <- file.path(tempdir(), basename(url))
    curl::curl_download(url, dest, handle = curl::new_handle())
    dest
  }

  r.url <- "http://fonts.gstatic.com/s/lato/v20/S6uyw4BMUTPHvxk6XweuBCY.ttf"
  b.url <- "http://fonts.gstatic.com/s/lato/v20/S6u9w4BMUTPHh6UVew-FGC_p9dw.ttf"
  i.url <- "http://fonts.gstatic.com/s/lato/v20/S6u8w4BMUTPHjxswWyWrFCbw7A.ttf"
  bi.url <- "http://fonts.gstatic.com/s/lato/v20/S6u_w4BMUTPHjxsI5wqPHA3s5dwt7w.ttf"

  r.file <- get_file(r.url)
  b.file <- get_file(b.url)
  i.file <- get_file(i.url)
  bi.file <- get_file(bi.url)

  sysfonts::font_add("Lato", r.file, b.file, i.file, bi.file)

  r.url <- "http://fonts.gstatic.com/s/playfairdisplay/v22/nuFvD-vYSZviVYUb_rj3ij__anPXJzDwcbmjWBN2PKdFvUDQZNLo_U2r.ttf"
  b.url <- "http://fonts.gstatic.com/s/playfairdisplay/v22/nuFvD-vYSZviVYUb_rj3ij__anPXJzDwcbmjWBN2PKeiukDQZNLo_U2r.ttf"
  i.url <- "http://fonts.gstatic.com/s/playfairdisplay/v22/nuFRD-vYSZviVYUb_rj3ij__anPXDTnCjmHKM4nYO7KN_qiTbtbK-F2rA0s.ttf"
  bi.url <- "http://fonts.gstatic.com/s/playfairdisplay/v22/nuFRD-vYSZviVYUb_rj3ij__anPXDTnCjmHKM4nYO7KN_naUbtbK-F2rA0s.ttf"

  r.file <- get_file(r.url)
  b.file <- get_file(b.url)
  i.file <- get_file(i.url)
  bi.file <- get_file(bi.url)


  sysfonts::font_add("Playfair Display", r.file, b.file, i.file, bi.file)

  showtext::showtext_auto()
}

#' Colour palettes based on the colours used by Our World in Data
#'
#' @param alpha Transparency level, a real number in (0, 1).
#'
#' @return A ggproto object to be used in the context of ggplot2.
#'
#' @export
#'
pal_owid <- function(alpha) {
  owid_palette <- c(
    "BlueGrey" = "#57677D",
    "RedOrange" = "#CB480E",
    "Teal" = "#019B94",
    "Purple" = "#8D4DA2",
    "Pink" = "#EC0175",
    "Grey" = "#737373",
    "Green" = "#019B5D",
    "Maroony" = "#A54551",
    "Blue" = "#0184A9",
    "PastelPink" = "#DC5E78"
  )
  if (alpha > 1L | alpha <= 0L) {
    stop("alpha must be in (0, 1]")
  }
  raw_cols <- owid_palette
  raw_cols_rgb <- grDevices::col2rgb(raw_cols)
  alpha_cols <- grDevices::rgb(raw_cols_rgb[1L, ], raw_cols_rgb[2L, ],
    raw_cols_rgb[3L, ],
    alpha = alpha * 255L, names = names(raw_cols),
    maxColorValue = 255L
  )
  scales::manual_pal(unname(alpha_cols))
}

#' Our World in Data Colour Scales
#'
#' @inheritParams pal_owid
#' @param ... additional parameters for \code{\link[ggplot2]{discrete_scale}}
#'
#' @return A ggproto object to be used in the context of ggplot2.
#'
#' @export scale_fill_owid
#'
#' @rdname scale_owid
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(dplyr)
#' library(ggrepel)
#'
#' # make an Our World in Data style chart
#'
#' venom <- owid("incidence-of-venomous-animal-contact")
#'
#' colnames(venom) <- c("entity", "code", "year", "venom")
#'
#' # venom %>%
#' #   filter(entity %in% c("India", "Australia", "United States", "Guyana")) %>%
#' #   group_by(entity) %>%
#' #   mutate(label = ifelse(year == max(year), entity, NA)) %>%
#' #   ggplot(aes(x = year,
#' #              y = venom,
#' #              colour = entity)) +
#' #   geom_line() +
#' #   geom_point(size = 1) +
#' #   geom_text_repel(aes(label = label),
#' #                   hjust = 0, xlim = Inf,
#' #                   na.rm = TRUE, segment.colour = "grey") +
#' #   coord_cartesian(clip = "off")  +
#' #   scale_colour_owid() +
#' #   scale_y_continuous(limits = c(0, 1000)) +
#' #   labs(title = "Incidence of venomous animal contact, 1990 to 2017") +
#' #   theme_owid(import_fonts = FALSE) + # set true to use same fonts as owid
#' #   theme(plot.margin = margin(10, 80, 5, 10), legend.position = "none",
#' #         panel.grid.major.x = element_blank(), axis.title = element_blank())
#' }
#'
scale_fill_owid <- function(alpha = 1, ...) {
  ggplot2::discrete_scale("fill", "owid", pal_owid(alpha), ...)
}


#' @export scale_colour_owid
#'
#' @rdname scale_owid
scale_colour_owid <- function(alpha = 1, ...) {
  discrete_scale("color", "owid", pal_owid(alpha), ...)
}

#' @export scale_color_owid
#'
#' @rdname scale_owid
scale_color_owid <- scale_colour_owid

#' ggplot2 Theme in the Style of Our World in Data
#'
#' @param import_fonts Import the fonts used by Our World in Data
#'
#' @return A ggplot2 theme to be added to a ggplot2 plot.
#'
#' @import ggplot2
#'
#' @export
#'
theme_owid <- function(import_fonts = TRUE) {
  thm <- theme_minimal(base_family = "serif") %+replace%
    theme(
      text = element_text(colour = "#373737"),
      plot.title = element_text(size = "20", hjust = 0, vjust = 3.2),
      plot.subtitle = element_text(hjust = 0, vjust = 3.5),
      plot.title.position = "plot",
      legend.position = "right",
      axis.text = element_text(face = "plain"),
      panel.grid.major = element_line(linetype = "dashed"), panel.grid.minor = element_blank(),
      plot.margin = margin(11, 15, 5, 10),
      axis.line.x = element_line(colour = "#8e8e8e"),
      axis.ticks = element_line(colour = "#8e8e8e")
    )

  if (import_fonts) {
    if (requireNamespace("showtext")) {
      if (curl::has_internet()) {
        get_owid_fonts()
        thm <- thm + theme(
          text = element_text(family = "Lato", face = "plain", colour = "#373737"),
          plot.title = element_text(
            family = "Playfair Display",
            size = "20"
          ),
          plot.subtitle = element_text(family = "Lato", hjust = 0)
        )
      } else {
        warning("importing fonts requires an internet connection please use import_fonts = FALSE")
      }
    } else {
      warning("importing fonts requires the showtext pacakge")
    }
  }
  thm
}
