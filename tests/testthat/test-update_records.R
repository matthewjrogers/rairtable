test_that("update_records errors", {
  expect_error(update_records(data = data.frame("airtable_record_id" = 1)))
})

local_test_airtable <- function(env = parent.frame()) {
  withr::local_envvar(AIRTABLE_PAT = "test_pat", .local_envir = env)
  withr::local_options(rairtable.safely = FALSE, .local_envir = env)
  airtable("tblabc123", "appxyz123", description = "Test base")
}

mock_ok <- function(req) {
  httr2::response_json(body = list(records = list()))
}

test_that("update_records supports tidyselect columns", {
  atbl <- local_test_airtable()
  data <- data.frame(
    airtable_record_id = c("rec1", "rec2"),
    Name = c("a", "b"),
    Count = 1:2
  )

  httr2::with_mocked_responses(mock_ok, {
    expect_no_error(update_records(data, atbl))
    expect_no_error(update_records(data, atbl, columns = "Name"))
    expect_no_error(
      update_records(data, atbl, columns = tidyselect::starts_with("Cou"))
    )
  })

  expect_error(
    httr2::with_mocked_responses(
      mock_ok,
      update_records(data, atbl, columns = "Missing")
    ),
    "exist"
  )
})

test_that("update_records errors if records and data have different lengths", {
  atbl <- local_test_airtable()

  expect_error(
    update_records(
      data.frame(Name = c("a", "b")),
      atbl,
      records = c("rec1", "rec2", "rec3")
    ),
    "record ID"
  )
})
