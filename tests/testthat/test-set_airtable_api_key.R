test_that("set_airtable_api_key works", {
  withr::local_envvar(
    c("AIRTABLE_API_KEY" = NULL),
    {
      expect_identical(
        suppressWarnings(set_airtable_api_key(key = "test_key")),
        "test_key"
      )

      expect_identical(
        suppressWarnings(set_airtable_api_key(
          key = "test_key",
          install = FALSE, overwrite = TRUE
        )),
        "test_key"
      )
    }
  )
})
