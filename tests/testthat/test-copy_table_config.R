test_that("get_field_config drops fields that can't be copied", {
  fields <- list(
    list(name = "a", type = "singleLineText"),
    list(name = "created", type = "createdTime"),
    list(name = "modified", type = "lastModifiedTime"),
    list(name = "b", type = "number")
  )

  config <- suppressMessages(get_field_config(fields))

  expect_length(config, 2)
  expect_identical(
    vapply(config, function(x) x[["name"]], ""),
    c("a", "b")
  )
})

test_that("copy_table_config works with createdTime fields", {
  model <- list(
    name = "Table",
    fields = list(
      list(name = "a", type = "singleLineText"),
      list(name = "created", type = "createdTime")
    )
  )

  config <- suppressMessages(copy_table_config(model = model))

  expect_identical(config[["name"]], "Table")
  expect_length(config[["fields"]], 1)
})
