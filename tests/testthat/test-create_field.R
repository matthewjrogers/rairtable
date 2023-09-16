httptest2::with_mock_dir("create_field", {
  test_that("create_field works", {
    skip_if_no_pat()

    atbl <- get_rairtable_test_obj()

    atbl_recs <- list_records(atbl)

    atbl_fields <- names(atbl_recs)

    expect_false(
      "Test_create_field" %in% atbl_fields
    )

    create_field(
      atbl,
      name = "Test_create_field"
    )

    tables <- get_table_models(atbl)

    expect_true(
      "Test_create_field" %in% tables[["fields"]][[1]][["name"]]
    )
  })
})

test_that("create_field errors", {
  expect_error(
    create_field(field = "field"),
    '`field` must be a list, not the string "field".'
  )

  expect_error(
    create_field(name = ""),
    '`name` must be a single string, not the empty string "".'
  )

  expect_error(
    create_field(
      name = "name",
      description = paste0(rep("x", 20001), collapse = "")
    ),
    "`description` must be a string no longer than 20,000 characters."
  )

  expect_error(
    create_field(name = "name", type = "type"),
    "`type` must be one of "
  )
})
