test_that("airtable_base errors", {
  expect_error(
    airtable_base()
  )
})

httptest2::with_mock_dir("airtable_base", {
  test_that("airtable_base works", {
    # skip_if_no_pat()
    url <- get_rairtable_test_url()
    ids <- parse_airtable_url(as.character(url))

    expect_type(
      airtable_base(ids[["base"]]),
      "list"
    )

    base_list <- list_bases()

    expect_s3_class(
      base_list,
      "data.frame"
    )

    expect_identical(
      nrow(list_bases(base_list[["id"]][1])),
      1L
    )
  })
})
