httptest2::with_mock_dir("create_table", {
  test_that("create_table and update_table work", {
    skip_if_no_pat()

    atbl <- get_rairtable_test_obj()
    random_abb <- sample(state.abb, 1)

    atbl_tbl <- create_table(
      airtable = atbl,
      name = random_abb,
      fields = list(list(name = "Primary field", type = "singleLineText"))
    )

    expect_named(
      atbl_tbl,
      c("id", "name", "primaryFieldId", "fields", "views")
    )

    expect_identical(
      atbl_tbl$fields[[1]]$name,
      "Primary field"
    )

    atbl_tbl_update <- update_table(
      base = atbl$base,
      table = atbl_tbl$id,
      name = paste0("updated", random_abb)
    )

    expect_identical(
      atbl_tbl_update$name,
      paste0("updated", random_abb)
    )
  })
})
