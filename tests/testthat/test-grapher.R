library(dplyr)

covid <- owid_covid()

test_that("test grapher with owid covid data: blank graph", {


  blank_graph <- covid %>%
    owid_grapher(date, new_deaths_smoothed_per_million, location)

  expect_equal(class(covid)[1], "owid")
  expect_equal(class(blank_graph), c("grapher", "data.frame"))
  expect_equal(attributes(blank_graph)$include_chart, FALSE)
  expect_equal(attributes(blank_graph)$include_map, FALSE)



})

test_that("line grapher", {
  line_graph <- covid %>%
    owid_grapher(date, new_deaths_smoothed_per_million, location) %>%
    grapher_line()

  expect_equal(attributes(line_graph)$include_chart, TRUE)
  expect_equal(attributes(line_graph)$include_map, FALSE)
})

test_that("map grapher", {
  map_graph <- covid %>%
    owid_grapher(date, new_deaths_smoothed_per_million, location) %>%
    grapher_map()

  expect_equal(attributes(map_graph)$include_chart, FALSE)
  expect_equal(attributes(map_graph)$include_map, TRUE)

  map_graph_2 <- covid %>%
    owid_grapher(date, new_deaths_smoothed_per_million, location) %>%
    grapher_map(palette = "PuBuGn", bins = c(0, 1, 2, 3, 4, 5, 6))

  expect_equal(attributes(map_graph_2)$map_palette, "PuBuGn")
  expect_equal(attributes(map_graph_2)$map_bins, c(0, 1, 2, 3, 4, 5, 6))

})







