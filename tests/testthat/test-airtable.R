test_that("airtable errors", {
  expect_error(airtable())

  expect_error(
    airtable(table = "tbl123")
  )

  expect_error(
    airtable(table = c("tbl123", "extra_table"), base = "app123")
  )
})

httptest2::with_mock_dir("airtable", {
  test_that("airtable works", {
    url <- get_rairtable_test_url()
    ids <- parse_airtable_url(url)

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

    tables <- list_base_tables(base = ids$base)

    expect_s3_class(
      airtable(
        table = tables$name[[1]],
        base = ids$base
      ),
      "airtable"
    )
  })
})
