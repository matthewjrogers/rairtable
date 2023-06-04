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

    tables <- list_base_tables(atbl)

    expect_true(
      "Test_create_field" %in% tables[["fields"]][[1]][["name"]]
    )
  })
})
