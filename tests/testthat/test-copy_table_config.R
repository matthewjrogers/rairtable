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

test_that("create_table uses model if config is NULL", {
  withr::local_envvar(AIRTABLE_PAT = "test_pat")
  atbl <- airtable("tblabc123", "appxyz123", description = "Test base")
  captured <- new.env()

  model <- list(
    name = "Copy",
    fields = list(
      list(name = "a", type = "singleLineText"),
      list(name = "created", type = "createdTime")
    )
  )

  suppressMessages(
    httr2::with_mocked_responses(
      function(req) {
        captured$body <- req$body$data
        captured$method <- req$method
        httr2::response_json(body = list(id = "tblnew"))
      },
      create_table(atbl, model = model)
    )
  )

  expect_identical(captured$body[["name"]], "Copy")
  expect_length(captured$body[["fields"]], 1)
})

test_that("create_table warns if model and config are both supplied", {
  withr::local_envvar(AIRTABLE_PAT = "test_pat")
  atbl <- airtable("tblabc123", "appxyz123", description = "Test base")
  config <- make_table_config(
    name = "New",
    fields = list(list(name = "a", type = "singleLineText"))
  )

  suppressMessages(
    expect_message(
      httr2::with_mocked_responses(
        function(req) httr2::response_json(body = list(id = "tblnew")),
        create_table(atbl, config = config, model = list(name = "x"))
      ),
      "ignored"
    )
  )
})
