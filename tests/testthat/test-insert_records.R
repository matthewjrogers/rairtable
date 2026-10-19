test_that("insert_records passes typecast to every batch", {
  withr::local_envvar(AIRTABLE_PAT = "test_pat")
  atbl <- airtable("tblabc123", "appxyz123", description = "Test base")
  captured <- new.env()
  captured$requests <- list()

  httr2::with_mocked_responses(
    function(req) {
      captured$requests <- c(captured$requests, list(req$body$data))
      httr2::response_json(body = list(records = list()))
    },
    {
      insert_records(data.frame(Name = letters[1:12]), atbl, typecast = FALSE)
      insert_records(data.frame(Name = letters[1:12]), atbl, typecast = TRUE)
    }
  )

  expect_length(captured$requests, 4)
  expect_false(captured$requests[[1]]$typecast)
  expect_false(captured$requests[[2]]$typecast)
  expect_true(captured$requests[[3]]$typecast)
  expect_true(captured$requests[[4]]$typecast)
})
