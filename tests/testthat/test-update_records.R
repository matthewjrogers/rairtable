test_that("update_records errors", {
  expect_error(update_records(data = data.frame("airtable_record_id" = 1)))
})

# Capture the method, url, and JSON body of requests instead of performing them
mock_capture <- function(env) {
  function(req) {
    env$requests <- c(
      env$requests,
      list(list(method = req$method, url = req$url, body = req$body$data))
    )
    httr2::response_json(body = list(records = list()))
  }
}

local_test_airtable <- function(env = parent.frame()) {
  withr::local_envvar(AIRTABLE_PAT = "test_pat", .local_envir = env)
  withr::local_options(rairtable.safely = FALSE, .local_envir = env)
  airtable("tblabc123", "appxyz123", description = "Test base")
}

test_that("update_records sends record IDs for multiple records", {
  atbl <- local_test_airtable()
  captured <- new.env()
  data <- data.frame(
    airtable_record_id = c("rec1", "rec2", "rec3"),
    Name = c("a", "b", "c"),
    Count = 1:3
  )

  httr2::with_mocked_responses(
    mock_capture(captured),
    update_records(data, atbl)
  )

  expect_length(captured$requests, 1)
  req <- captured$requests[[1]]
  expect_identical(req$method, "PATCH")
  expect_named(req$body, c("records", "returnFieldsByFieldId", "typecast"))
  expect_identical(
    vapply(req$body$records, function(x) x[["id"]], ""),
    c("rec1", "rec2", "rec3")
  )
  expect_identical(
    req$body$records[[2]][["fields"]],
    list(Name = "b", Count = 2L)
  )
})

test_that("update_records supports tidyselect columns", {
  atbl <- local_test_airtable()
  captured <- new.env()
  data <- data.frame(
    airtable_record_id = c("rec1", "rec2"),
    Name = c("a", "b"),
    Count = 1:2
  )

  httr2::with_mocked_responses(
    mock_capture(captured),
    update_records(data, atbl, columns = "Name")
  )

  expect_named(captured$requests[[1]]$body$records[[1]][["fields"]], "Name")

  captured <- new.env()
  httr2::with_mocked_responses(
    mock_capture(captured),
    update_records(data, atbl, columns = tidyselect::starts_with("Cou"))
  )

  expect_named(captured$requests[[1]]$body$records[[1]][["fields"]], "Count")
})

test_that("update_records batches records and keeps IDs aligned with data", {
  atbl <- local_test_airtable()
  captured <- new.env()
  data <- data.frame(
    airtable_record_id = sprintf("rec%02d", 1:12),
    Name = letters[1:12]
  )

  httr2::with_mocked_responses(
    mock_capture(captured),
    update_records(data, atbl, typecast = TRUE)
  )

  expect_length(captured$requests, 2)
  expect_length(captured$requests[[1]]$body$records, 10)
  expect_length(captured$requests[[2]]$body$records, 2)
  expect_identical(captured$requests[[2]]$body$records[[1]][["id"]], "rec11")
  expect_identical(captured$requests[[2]]$body$records[[1]][["fields"]][["Name"]], "k")
  expect_true(captured$requests[[2]]$body$typecast)
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
