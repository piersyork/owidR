
test_that("test core owid function", {
  search_result <- owid_search("gdp")
  data <- owid(search_result[1, 2])
  expect_equal(class(data), c("owid", "tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(data)[1:3], c("entity", "code", "year"))
})
