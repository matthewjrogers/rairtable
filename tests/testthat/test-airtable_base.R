test_that("airtable_base works", {
  skip_if_no_pat()
  url <- get_rairtable_test_url()
  ids <- parse_airtable_url(url)

  expect_error(
    airtable_base()
  )

  expect_type(
    airtable_base(ids$base),
    "list"
  )

  expect_s3_class(
    list_airtable_bases(),
    "data.frame"
  )
})
