test_that("airtable works", {
  withr::with_envvar(
    get_rairtable_test_args(),
    {
      table = Sys.getenv("rairtable.test_table")
      base = Sys.getenv("rairtable.test_base")
      view = Sys.getenv("rairtable.test_view")

      expect_error(airtable())
      expect_error(
        airtable(table = table)
      )
      expect_error(
        airtable(table = c(table, "extra_table"), base = base)
      )

      skip_if(is_null(base) | is_null(table))
      expect_s3_class(
        airtable(
          table = table,
          base = base
        ),
        "airtable"
      )
      })
})
