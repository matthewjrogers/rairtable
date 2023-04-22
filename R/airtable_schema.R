#' Create a request for the Airtable metadata API
#'
#' Create a request to get metadata or update metadata for bases, tables, or
#' fields using the Airtable metadata API. Note that the metadata API does not
#' support the legacy Airtable API key so a personal access token is required.
#' You can save a personal access token with [set_airtable_pat()].
#'
#' @inheritParams airtable_request
#' @param column Column ID, Defaults to `NULL`. Only required when type =
#'   "update" and from = "field".
#' @param ... Additional parameters passed to [req_query_airtable()]
#' @param type Type of API call to use, Default: c("schema", "list", "update",
#'   "create")
#' @param from Is the API call for bases, tables, or fields? Default: c("base",
#'   "table", "field"). Not all combinations of type and from are supported.
#'   "base" works with "schema", "create", and "list", "table" and "field" both
#'   only work with "update" and "create".
#' @returns A modified HTTP request.
#' @keywords internal
#' @export
req_airtable_schema <- function(url = NULL,
                                base = NULL,
                                table = NULL,
                                column = NULL,
                                airtable = NULL,
                                ...,
                                type = c("schema", "list", "update", "create"),
                                from = c("base", "table", "field"),
                                token = NULL,
                                call = caller_env()) {
  type <- arg_match(type, call = call)
  from <- arg_match(from, call = call)
  type <- paste0(type, "_", from)

  template <- "meta/bases"

  template <-
    paste0(
      template,
      switch(type,
        "schema_base" = "/{base}/tables",
        "create_base" = "",
        "list_base" = "",
        "update_table" = "/{base}/tables/{table}",
        "create_table" = "/{base}/tables",
        "update_field" = "/{base}/tables/{table}/fields/{column}",
        "create_field" = "/{base}/tables/{table}/fields"
      )
    )

  base_required <- !(type %in% c("create_base", "list_base"))
  table_required <- type %in% c("update_table", "update_field", "create_field")
  column_required <- type == "update_field"

  if (base_required) {
    if (!is_null(c(url, airtable))) {
      base <- get_airtable_base(
        base = base,
        url = url,
        airtable = airtable,
        call = call
      )
    }

    check_string(base, allow_empty = FALSE, call = call)
  }

  if (table_required) {
    table <- get_airtable_table(
      table = table,
      url = url,
      airtable = airtable,
      call = call
    )

    check_string(table, allow_empty = FALSE, call = call)
  }

  if (column_required) {
    if (is_null(column)) {
      cli_abort(
        "{.arg column} must be supplied to update fields.",
        call = call
      )
    }

    check_string(column, allow_empty = FALSE, call = call)
  }

  req_query_airtable(
    base = base,
    table = table,
    template = template,
    token = token,
    ...,
    call = call,
    allow_key = FALSE
  )
}

#' Is x an Airtable base ID?
#'
#' Return TRUE if x is a string starting with "app".
#'
#' @param x Object to test.
#' @noRd
is_base_id <- function(x) {
  if (is_null(x) | !is_string(x)) {
    return(FALSE)
  }

  grepl("^app", x)
}

#' Get an Airtable base ID from a URL or Airtable
#'
#' Return the provided base ID (if valid) or a base ID extracted from a URL or
#' airtable object.
#'
#' @inheritParams airtable_request
#' @noRd
get_airtable_base <- function(base = NULL,
                              url = NULL,
                              airtable = NULL,
                              call = caller_env()) {
  base <- base %||%
    airtable_request(
      url = url,
      airtable = airtable,
      call = call
    )[["url"]]

  if (is_base_id(base)) {
    return(base)
  }

  if (is_airtable_obj(base)) {
    return(base[["base"]])
  }

  if (is_url(base)) {
    return(parse_airtable_url(base)[["base"]])
  }
}


#' Get an Airtable table ID from a URL or Airtable
#'
#' Return the provided base ID (if valid) or a base ID extracted from a URL or
#' airtable object.
#'
#' @inheritParams airtable_request
#' @noRd
get_airtable_table <- function(table = NULL,
                               url = NULL,
                               airtable = NULL,
                               table_name = NULL,
                               call = caller_env()) {
  table <- table %||% airtable_request(
    url = url,
    airtable = airtable,
    call = call
  )[["url"]]

  if (is_airtable_obj(table)) {
    return(airtable[["table"]])
  }

  if (is_url(table)) {
    return(parse_airtable_url(
      table,
      table_name = table_name,
      call = call
    )[["table"]])
  }

  if (is_string(table)) {
    return(table)
  }
}
