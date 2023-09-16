test_that("update_records errors", {
  expect_error(update_records(data = data.frame("airtable_record_id" = 1)))
})
