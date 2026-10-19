test_that("insert_records completes when records are sent in batches", {
  withr::local_envvar(AIRTABLE_PAT = "test_pat")
  atbl <- airtable("tblabc123", "appxyz123", description = "Test base")
  n_requests <- 0

  httr2::with_mocked_responses(
    function(req) {
      n_requests <<- n_requests + 1
      httr2::response_json(body = list(records = list()))
    },
    expect_no_error(insert_records(data.frame(Name = letters[1:12]), atbl))
  )

  expect_identical(n_requests, 2)
})
