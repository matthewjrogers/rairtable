test_that("set_airtable_api_key works", {
  withr::with_envvar(
    c("AIRTABLE_API_KEY" = NULL),
    {
      expect_identical(
        suppressWarnings(set_airtable_pat(pat = "test_key")),
        "test_key"
      )

      expect_identical(
        suppressWarnings(set_airtable_api_key(key = "test_key", install = TRUE, overwrite = TRUE)),
        "test_key"
      )

    })
})
