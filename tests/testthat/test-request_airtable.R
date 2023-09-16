test_that("request_airtable works", {
  atbl <- get_rairtable_test_obj()

  expect_s3_class(
    request_airtable(airtable = atbl),
    "httr2_request"
  )

  expect_s3_class(
    request_airtable(url = get_rairtable_test_url()),
    "httr2_request"
  )

  expect_s3_class(
    request_airtable(base = "app", table = "tbl"),
    "httr2_request"
  )
})

test_that("request_airtable errors", {
  expect_error(
    request_airtable(),
    "`airtable`, `url`, or `base` and `table` must be supplied."
  )

  expect_error(
    request_airtable(airtable = 1),
    "`airtable` must be an <airtable> object, not a number."
  )

  expect_error(
    request_airtable(url = "airtable.com"),
    "`url` must be a valid url."
  )

  expect_error(
    request_airtable(url = "https://airtable.com"),
    "`url` is not valid."
  )
})

test_that("req_airtable works", {
  atbl <- get_rairtable_test_obj()
  req <- request_airtable(airtable = atbl)
  req <- req_airtable(req)

  expect_s3_class(
    req,
    "httr2_request"
  )

  expect_identical(
    req$options$useragent,
    getOption(
      "rairtable.useragent",
      default = "rairtable (https://github.com/matthewjrogers/rairtable)"
    )
  )

  expect_identical(
    req$url,
    paste0(c(getOption("rairtable.api_url", "https://api.airtable.com/v0"), atbl$base, atbl$table), collapse = "/")
  )
})
