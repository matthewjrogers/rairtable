test_that("set_airtable_pat works", {
  withr::local_envvar(
    c("AIRTABLE_PAT" = NULL),
    {
      expect_identical(
        set_airtable_pat(pat = "test_pat"),
        "test_pat"
      )

      expect_identical(
        set_airtable_pat(pat = "test_pat", install = FALSE, overwrite = TRUE),
        "test_pat"
      )
    }
  )
})
