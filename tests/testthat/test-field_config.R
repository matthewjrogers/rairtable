test_that("make_field_config works", {
  # named parameters and list field are equivalent
  expect_equal(
    make_field_config(name = "Field name", type = "singleLineText"),
    make_field_config(list(name = "Field name", type = "singleLineText"))
  )

  # named parameters and data frame field are equivalent
  expect_equal(
    make_field_config(name = "Field name", type = "singleLineText"),
    make_field_config(data.frame(name = "Field name", type = "singleLineText"))
  )

  # name and type are required
  expect_error(
    make_field_config(type = "singleLineText"),
    "`name` must be a valid name, not `NULL`."
  )

  expect_error(
    make_field_config(name = "name", type = "type"),
    "`type` must be one of "
  )
})

test_that("make_list_of_lists works", {
  test_comparison <- list(list(name = "Field name", type = "singleLineText"))

  expect_equal(
    make_list_of_lists(data = list(name = "Field name", type = "singleLineText")),
    test_comparison
  )

  expect_equal(
    make_list_of_lists(data.frame(name = "Field name", type = "singleLineText")),
    test_comparison
  )

  expect_error(
    make_list_of_lists(
      data = data.frame(name = c("A", "B"), type = c("singleLineText", "singleLineText")),
      max_rows = 1
    ),
    "must be a list or data frame with 1 row, not 2 rows."
  )
})
