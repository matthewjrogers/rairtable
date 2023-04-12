test_that("airtable_base works", {
  skip_if_no_pat()

  withr::with_envvar(
    get_rairtable_test_args(),
    {
      expect_error(
        airtable_base()
      )
      base <- Sys.getenv("rairtable.test_base")
      skip_if(is_null(base))
      expect_type(
        airtable_base(base),
        "environment"
      )
    }
  )
})
