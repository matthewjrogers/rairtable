test_that("is_airtable_url works", {
  expect_true(
    is_airtable_url("https://airtable.com")
  )
  expect_false(
    is_airtable_url("https://airtable.co")
  )
  expect_true(
    is_airtable_url(
      "https://custom.airtable.com",
      "https://custom.airtable.com"
    )
  )
})

test_that("is_airtable_api_url works", {
  expect_true(
    is_airtable_api_url("https://api.airtable.com")
  )
  expect_false(
    is_airtable_api_url("https://api.airtable.co")
  )
  expect_true(
    is_airtable_api_url(
      "https://custom.airtable.com",
      "https://custom.airtable.com"
    )
  )
})


test_that("check_airtable_url works", {
  expect_error(
    check_airtable_url()
  )
  expect_null(
    check_airtable_url(NULL, allow_null = TRUE)
  )
  expect_null(
    check_airtable_url("https://airtable.com")
  )
  expect_error(
    check_airtable_url("https://api.airtable.com")
  )
  expect_null(
    check_airtable_url(
      "https://custom.airtable.com",
      "https://custom.airtable.com"
    )
  )
})


test_that("check_airtable_api_url works", {
  expect_error(
    check_airtable_api_url()
  )
  expect_error(
    check_airtable_api_url("https://api.airtable.com")
  )

  skip_if_no_pat()

  atbl <- get_rairtable_test_obj()

  expect_null(
    check_airtable_api_url(atbl[["request_url"]])
  )
})
