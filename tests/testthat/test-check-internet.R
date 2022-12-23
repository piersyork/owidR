skip_on_cran()
skip_if(!curl::has_internet())
skip_if(httr::http_error("ourworldindata.org"))

test_that("test failure on bad url", {
  out <- owid("this-is-definitely-not-right")
  expect_equal(class(out)[1], "owid.no.connection")
  outDT <- as.data.table(out)
  expect_equal(outDT, data.table(entity = NA, year = NA, value = NA))
})


test_that("test good connection", {
  out <- check_internet("ourworldindata.org")
  expect_equal(out, TRUE)
})
