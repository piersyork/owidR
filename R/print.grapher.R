#' Internal function to transform dataframe into json format
#'
#' @param df A dataframe object
#'
#' @return The dataset in json form to be used in the iframe, formatted as a string.
#'
#' @noRd
#'
create_dataset_json <- function(df, is_date) {
  n_entities <- df$Entity %>% unique() %>% length()

  entityKey <- sprintf(
    '"%s": {"id": %s, "name": "%s"}',

    paste0(1:n_entities),
    paste0(1:n_entities),
    paste0(unique(df$Entity))
  ) %>% paste0(collapse = ", ")

  if (is_date) {
    display <- '"yearIsDay": true, "zeroDay": "1970-01-01"'
  } else display <- ""



  owidDataset <- sprintf(
    '"variables": {"1": {"id": 1, "name": "dummy",
  "years": [%s],
  "entities": [%s],
  "values": [%s],
  "display": {%s}}},
  "entityKey": {%s}',

  paste0(df$Year, collapse = ", "),
  paste0(df$id, collapse = ", "),
  paste0(df$value, collapse = ", "),
  display,
  entityKey
  )
  return(owidDataset)
}
#' Internal function to create the jsonConfig string.
#'
#' @description uses the grapher settings to create a jsonConfig string used to change the chart settings
#'
#' @param title The chart title
#' @param subtitle The chart subtitle
#' @param note The chart note
#' @param source The chart source
#' @param type The type of chart
#' @param hasMapTab Should a map be included
#' @param owidDataset A string returned by create_dataset_json().
#' @param selectedData The entities to be included in the chart
#'
#' @return A string of the jsonConfig
#' @noRd
create_config_json <- function(map_config, tab, title, subtitle, note, source, type, hasChartTab, hideEntityControls,
                               hasMapTab, owidDataset, selectedData) {
  jsonConfig <- paste0(
    '{map: {', map_config, '}, "tab": "', tab, '", "title": "', title, '", "subtitle": "', subtitle, '", "note": "', note,
    '", "sourceDesc": "', source, '", "hideLogo": true, "isPublished": false, "type": "', type,
    '","hasChartTab": ', hasChartTab, ', "hideTitleAnnotation": false, "hideLegend": false, "hideEntityControls": ', hideEntityControls,', "hideRelativeToggle": true, "hasMapTab": ', hasMapTab, ', "stackMode": "absolute", "yAxis": {},
  "owidDataset": {', owidDataset, '}, "dimensions": [{"property": "y", "variableId": 1, "display": {}}], "selectedData": [', selectedData, ']};'
  )
}
#' Internal function to create the iframe html
#'
#'
#' @param height The height of the chart, to be changed for whether chart is displayed in viewer of in rmarkdown
#' @param jsonConfig
#'
#' @return An html string of the iframe. This html text is able to produce an owid graph.
#' @noRd
create_iframe <- function(height, jsonConfig) {
  frame_id <- paste0(sample(rep(letters, 10), 20), collapse = "")

  html <- paste0(
    '<iframe id="', frame_id,'" style="width: 100%; height: ', height, '; border: 0px none;" src="about:blank" data-external="1"></iframe>
        <script>
            document.getElementById("', frame_id, '").contentDocument.write(`
<!DOCTYPE html>
<html>
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <link
      href="https://fonts.googleapis.com/css?family=Lato:300,400,400i,700,700i|Playfair+Display:400,700&amp;display=swap"
      rel="stylesheet"
    />
    <link
      rel="stylesheet"
      href="https://ourworldindata.org/assets/commons.css"
    />
    <link rel="stylesheet" href="https://ourworldindata.org/assets/owid.css" />
    <meta property="og:image:width" content="850" />
    <meta property="og:image:height" content="600" />
    <script>
      if (window != window.top)
        document.documentElement.classList.add("IsInIframe");
    <\\/script>
    <noscript
      ><style>
        figure {
          display: none !important;
        }
      </style></noscript
    >
  </head>
  <body class="StandaloneGrapherOrExplorerPage">
    <main>
      <figure data-grapher-src>
      </figure>
    </main>
      <div class="site-tools"></div>
      <script src="https://polyfill.io/v3/polyfill.min.js?features=es6,fetch,URL,IntersectionObserver,IntersectionObserverEntry"><\\/script>
      <script src="https://ourworldindata.org/assets/commons.js"><\\/script>
      <script src="https://ourworldindata.org/assets/owid.js"><\\/script>
      <script>
        window.runSiteFooterScripts();
      <\\/script>
    <script>
      const jsonConfig = ', jsonConfig,
'
      window.Grapher.renderSingleGrapherOnGrapherPage(jsonConfig);
    <\\/script>
  </body>
</html>
`)
        </script>
'
  )
}

#' Internal function to create mapConfig
#'
#' @param map_palette R color brewer palette
#' @param map_bins The steps of the legend bins or "auto".
#'
#' @return The map config
#' @noRd
create_map_config <- function(map_palette = "Greens", map_bins = "auto") {
  if (length(map_bins) == 1) {
    binning_strategy <- "auto"
    num_values <- ""
  } else {
    binning_strategy <- "manual"
    num_values <- map_bins
  }
  paste0(
    '   "colorScale": {
          "equalSizeBins": true,
          "baseColorScheme": "', map_palette,'",
          "binningStrategy": "', binning_strategy,'",
          "customNumericColors": [null],
          "customNumericLabels": [],
          "customNumericValues": [', paste(num_values, collapse = ", "), '],
          "customCategoryColors": {},
          "customCategoryLabels": {},
          "customHiddenCategories": {}
        },
        "projection": "World",
        "targetYear": 2020'
  )
}

#' The print method for owid_grapher()
#'
#' @description Create the final graph and presents it in the viewer
#'
#' @param x An object of class "grapher"
#' @param ... Further arguments for future versions
#'
#' @return
#' @export
#'
print.grapher <- function(x, ...) {
  grapher <- x

  type <- attributes(grapher)$type
  selected <- attributes(grapher)$selected
  title <- attributes(grapher)$title
  subtitle <- attributes(grapher)$subtitle
  note <- attributes(grapher)$note
  source <- attributes(grapher)$source
  include_map <- attributes(grapher)$include_map
  include_chart <- attributes(grapher)$include_chart
  change_selected <- attributes(grapher)$change_selected
  raw_html <- attributes(grapher)$raw_html
  map_palette <- attributes(grapher)$map_palette
  map_bins <- attributes(grapher)$map_bins
  is_date <- attributes(grapher)$is_date
  tab <- attributes(grapher)$tab

  owidDataset <- create_dataset_json(grapher, is_date)

  selected_entities <- selected

  selected_ids <- grapher %>%
    select(Entity, id) %>%
    distinct() %>%
    filter(Entity %in% selected_entities) %>%
    pull(id)

  selectedData <- paste0(sprintf('{"entityId": %s}', selected_ids), collapse = ", ")

  subtitle <- subtitle
  note <- note
  source <- source
  type <- type

  if (include_map) hasMapTab <- "true" else hasMapTab <- "false"
  if (include_chart) hasChartTab <- "true" else hasChartTab <- "false"
  if (change_selected) hideEntityControls <- "false" else hideEntityControls <- "true"

  map_config <- create_map_config(map_palette, map_bins)

  jsonConfig <- create_config_json(map_config, tab, title, subtitle, note, source, type, hasChartTab, hideEntityControls,
                                   hasMapTab, owidDataset, selectedData)
  # print(jsonConfig)
  is_markdown <- !is.null(knitr::opts_knit$get("out.format"))

  if (is_markdown) height <- "600px" else height <- "100%"

  frame_id <- paste0(sample(rep(letters, 10), 20), collapse = "")

  html <- create_iframe(height, jsonConfig)

  if (is_markdown) {
    # cat("OWID graph:") # <- this works
    cat("<br>") # <- this works also :)
    htmltools::HTML(html)

  } else {
    temp <- tempfile(fileext = ".html")
    cat(html, file = temp)
    rstudioapi::viewer(temp)
  }
}
