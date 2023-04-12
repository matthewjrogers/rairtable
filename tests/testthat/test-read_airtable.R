test_that("read_airtable works", {
  skip_if_no_pat()

  withr::with_envvar(
    get_rairtable_test_args(),
    {
      atbl <- get_rairtable_test_obj()

      expect_error(
        read_airtable()
      )

      skip_if(is_null(atbl))
      expect_error(
        read_airtable(atbl, id_to_col = "")
      )
      expect_error(
        read_airtable(atbl, max_rows = 50001)
      )

      expect_s3_class(
        read_airtable(atbl),
        "data.frame"
      )
    }
  )
})
