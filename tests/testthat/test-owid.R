skip_on_cran()
skip_if(!curl::has_internet())
skip_if(httr::http_error("ourworldindata.org"))

test_that("test core owid function", {
  search_result <- owid_search("gdp")
  data <- owid("water-productivity", rename = "water_prod")
  expect_equal(class(data), c("owid", "tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(data), c("entity", "code", "year", "water_prod"))
})

test_that("test multiple cols", {
  data <- owid("learning-outcomes-vs-gdp-per-capita",
               rename = c("pop", "continent", "learning", "gdp"))
  expect_equal(colnames(data), c("entity", "code", "year", "pop", "continent", "learning", "gdp"))
})

test_that("test http error", {
  data <- owid("this_is_not_a_valid_id")

  expect_equal(class(data)[1], "owid.no.connection")
  expect_equal(dplyr::tibble(data), dplyr::tibble(entity = NA, year = NA, value = NA))

})
