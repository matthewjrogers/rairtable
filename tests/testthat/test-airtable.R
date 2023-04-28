test_that("airtable works", {
  url <- get_rairtable_test_url()
  ids <- parse_airtable_url(url)

  expect_error(airtable())

  expect_error(
    airtable(table = ids$table)
  )
  expect_error(
    airtable(table = c(ids$table, "extra_table"), base = ids$base)
  )

  skip_if_no_pat()
  expect_s3_class(
    airtable(
      table = ids$table,
      base = ids$base
    ),
    "airtable"
  )

  expect_s3_class(
    airtable(
      table = url,
      base = ids$base
    ),
    "airtable"
  )
})
