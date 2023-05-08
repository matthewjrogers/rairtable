test_that("read_airtable works", {
  skip_if_no_pat()

  atbl <- get_rairtable_test_obj()

  expect_error(
    read_airtable()
  )

  expect_error(
    read_airtable(atbl, id_to_col = "")
  )

  insert_records(
    data.frame(
      "Name" = "Test"
    ),
    atbl
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

  record <- atbl_df[!is.na(atbl_df$Name), ][[getOption("rairtable.id_col", "airtable_record_id")]]

  expect_s3_class(
    read_airtable_record(
      atbl,
      record = record
    ),
    "data.frame"
  )

  delete_records(
    airtable = atbl,
    records = record,
    safely = FALSE
  )
})
