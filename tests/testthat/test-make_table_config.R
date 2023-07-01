test_that("make_table_config errors", {
  expect_error(
    make_table_config(),
    "`name` must be a valid name, not `NULL`."
  )

  expect_error(
    make_table_config(name = "name"),
    "`fields` must be a list or data frame."
  )

  expect_error(
    make_table_config(name = "name", fields = list(name = "field")),
    '`field` must have the names "name" and "type".'
  )
})

test_that("make_table_config works", {
  expect_identical(
    make_table_config(
      name = "table",
      fields = list(list(name = "field", type = "singleLineText"))
    ),
    list(
      name = "table",
      fields = list(list(name = "field", type = "singleLineText"))
    )
  )
})
