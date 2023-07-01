#' Create and perform a request for the Airtable Metadata API
#'
#' Create a request to get metadata or update metadata for bases, tables, or
#' fields using the Airtable metadata API. Note that the metadata API does *not*
#' support the legacy Airtable API key so a personal access token is required.
#' Save a personal access token with [set_airtable_pat()]. This function does
#' not currently support the API for API for views or permissions which are both
#' limited to Enterprise user accounts.
#'
#' @inheritParams request_airtable
#' @param column Column ID, Defaults to `NULL`. Only required when `meta =
#'   "update_field"`. Column can be a field URL.
#' @param ... Additional parameters passed to [req_airtable()]
#' @param meta Type of metadata API call to use for bases, tables, or fields.
#'   Options include "schema_base", "create_base", "list_base", "update_table",
#'   "create_table", "update_field", or "create_field". Optional if template is
#'   provided.
#' @param template URL template passed to [req_airtable()] if meta is `NULL`.
#' @returns An HTTP response as an S3 list with class httr2_request.
#' @keywords internal
#' @importFrom vctrs list_drop_empty
#' @importFrom httr2 req_perform
request_airtable_meta <- function(url = NULL,
                                  base = NULL,
                                  table = NULL,
                                  column = NULL,
                                  airtable = NULL,
                                  ...,
                                  meta = NULL,
                                  template = NULL,
                                  data = NULL,
                                  token = NULL,
                                  call = caller_env()) {
  if (is_null(template)) {
    meta <-
      arg_match(meta,
        c(
          "create_base", "list_base", "schema_base",
          "create_table", "update_table",
          "create_field", "update_field"
        ),
        error_call = call
      )

    base_required <-
      !(meta %in% c("create_base", "list_base"))
    table_required <-
      meta %in% c("update_table", "update_field", "create_field")
    column_required <-
      meta == "update_field"
  }


  template <- template %||%
    paste0(
      "meta/bases",
      switch(meta,
        "create_base" = "",
        "list_base" = "",
        "schema_base" = "/{base}/tables",
        "create_table" = "/{base}/tables",
        "update_table" = "/{base}/tables/{table}",
        "create_field" = "/{base}/tables/{table}/fields",
        "update_field" = "/{base}/tables/{table}/fields/{column}"
      )
    )

  if (base_required) {
    base <- get_base_id(
      base = base,
      airtable = airtable,
      url = url,
      call = call
    )

    check_string(base, allow_empty = FALSE, call = call)
  }

  if (table_required) {
    table <- get_table_id(
      table = table,
      airtable = airtable,
      url = url,
      call = call
    )

    check_string(table, allow_empty = FALSE, call = call)
  }

  if (column_required) {
    column <- get_field_id(column, url = url, call = call)

    check_string(column, allow_empty = FALSE, call = call)
  }

  req <-
    req_airtable(
      base = base,
      table = table,
      column = column,
      template = template,
      ...,
      data = vctrs::list_drop_empty(data),
      token = token,
      call = call,
      require_base = FALSE,
      require_table = FALSE,
      allow_key = FALSE
    )

  httr2::req_perform(req, error_call = call)
}
