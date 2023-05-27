test_that("delete_records works", {
  skip_if_no_pat()
  atbl <- get_rairtable_test_obj()

  initial_records <- read_airtable(atbl)

  test_records <-
    data.frame(
      "Name" = c("Record 1", "Record 2"),
      "Notes" = c("Notes 1", "Notes 2")
    )

  insert_records(test_records, atbl)

  records_updated <- read_airtable(atbl)

  expect_identical(
    nrow(records_updated),
    nrow(initial_records) + nrow(test_records)
  )

  records_deleted <-
    records_updated[records_updated$Name %in% test_records$Name, ]

  delete_records(records_deleted, airtable = atbl, safely = FALSE)

  expect_identical(
    nrow(initial_records),
    nrow(records_updated) - nrow(records_deleted)
  )
})
