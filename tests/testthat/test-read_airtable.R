test_that("read_airtable works", {
  skip_if_no_pat()

  atbl <- get_rairtable_test_obj()

  expect_error(
    read_airtable()
  )

  expect_error(
    read_airtable(atbl, id_to_col = "")
  )

  atbl_df <- read_airtable(atbl)

  expect_s3_class(
    atbl_df,
    "data.frame"
  )

  record_df <- read_airtable_records(atbl)

  expect_s3_class(
    record_df,
    "data.frame"
  )

  expect_s3_class(
  read_airtable_record(
    atbl,
    record = atbl_df[1,][[getOption("rairtable.id_col", "airtable_record_id")]]
    ),
  "data.frame"
  )
})
