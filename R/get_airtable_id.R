#' Is x an Airtable ID?
#'
#' @param x Object to test.
#' @noRd
is_rairtable_id <- function(x, pattern, start = "^", end = "[[:alnum:]]+") {
  if (is_null(x) || !is_string(x)) {
    return(FALSE)
  }

  grepl(paste0(start, pattern, end), x)
}

#' Is x an Airtable base ID?
#'
#' Return TRUE if x is a string starting with "app".
#'
#' @param x Object to test.
#' @noRd
is_base_id <- function(x) {
  is_rairtable_id(x, "app")
}

#' Is x an Airtable table ID?
#'
#' Return TRUE if x is a string starting with "tbl" or matches the supplied
#' table_name.
#'
#' @param x Object to test.
#' @noRd
is_table_id <- function(x, table_name = NULL) {
  # FIXME: table_name is not exposed so there is no option to pass a table
  # name to is_table_id from get_table_id
  table_name <- table_name %||% "^tbl[[:alnum:]]+$"
  is_rairtable_id(x, table_name, "", "")
}

#' Is x an Airtable field ID?
#'
#' Return TRUE if x is a string starting with "fld".
#'
#' @param x Object to test.
#' @noRd
is_field_id <- function(x) {
  is_rairtable_id(x, "fld")
}

#' Is x an Airtable record ID?
#'
#' Return TRUE if x is a string starting with "rec".
#'
#' @param x Object to test.
#' @noRd
is_record_id <- function(x) {
  is_rairtable_id(x, "rec")
}

#' Is x an Airtable comment ID?
#'
#' Return TRUE if x is a string starting with "com".
#'
#' @param x Object to test.
#' @noRd
is_comment_id <- function(x) {
  is_rairtable_id(x, "com")
}

#' Is x an Airtable workspace ID?
#'
#' Return TRUE if x is a string starting with "wsp".
#'
#' @param x Object to test.
#' @noRd
is_workspace_id <- function(x) {
  is_rairtable_id(x, "wsp")
}

#' Get an Airtable base ID from a URL or Airtable
#'
#' Return the provided base ID (if valid) or a base ID extracted from a URL or
#' airtable object.
#'
#' @noRd
get_base_id <- function(base = NULL,
                        airtable = NULL,
                        url = NULL,
                        ...,
                        call = caller_env()) {
  base <- base %||% airtable %||% url

  if (is_base_id(base)) {
    return(base)
  }

  if (is_airtable_obj(base)) {
    return(base[["base"]])
  }

  if (is_url(base)) {
    return(parse_url_base_id(base))
  }

  cli_abort(
    "{.arg base} must be a base ID string, an {.cls airtable} object,
    or an Airtable URL, not {.obj_type_friendly {base}}.",
    call = call
  )
}

#' Get an Airtable table ID from a URL or Airtable
#'
#' Return the provided table ID (if valid) or a table ID from a URL or airtable
#' object.
#'
#' @noRd
get_table_id <- function(table = NULL,
                         airtable = NULL,
                         url = NULL,
                         table_name = NULL,
                         ...,
                         call = caller_env()) {
  table <- table %||% airtable %||% url

  if (is_table_id(table, table_name)) {
    return(table)
  }

  if (is_airtable_obj(table)) {
    return(airtable[["table"]])
  }

  if (is_url(table)) {
    return(parse_url_table_id(table, table_name, call))
  }

  cli_abort(
    "{.arg table} must be a table ID string, an {.cls airtable} object,
    or an Airtable URL, not {.obj_type_friendly {table}}.",
    call = call
  )
}

#' Get an Airtable column/field ID from a string or URL
#'
#' Return the provided field ID or a field ID parsed from a URL.
#'
#' @noRd
get_field_id <- function(column = NULL,
                         url = NULL,
                         ...,
                         call = caller_env()) {
  column <- column %||% url

  if (is_field_id(column)) {
    return(column)
  }

  if (is_url(column)) {
    return(parse_url_field_id(column))
  }

  cli_abort(
    "{.arg column} must be a field ID string or an Airtable URL with a field ID,
    not {.obj_type_friendly {column}}.",
    call = call
  )
}

#' Get an Airtable record ID from a string or URL
#'
#' Return the provided record ID or a record ID parsed from a URL.
#'
#' @noRd
get_record_id <- function(record = NULL,
                          url = NULL,
                          ...,
                          call = caller_env()) {
  record <- record %||% url

  if (is_record_id(record)) {
    return(record)
  }

  if (is_url(record)) {
    return(parse_url_record_id(record))
  }

  cli_abort(
    "{.arg record} must be a record ID string or an Airtable URL
    with a record ID, not {.obj_type_friendly {record}}.",
    call = call
  )
}

#' Get an Airtable workspace ID from a string or URL
#'
#' Return the provided workspace ID or a workspace ID parsed from a URL.
#'
#' @noRd
get_workspace_id <- function(workspace = NULL,
                             url = NULL,
                             ...,
                             call = caller_env()) {
  workspace <- workspace %||% url

  if (is_workspace_id(workspace)) {
    return(workspace)
  }

  if (is_url(workspace)) {
    return(parse_url_workspace_id(workspace))
  }

  cli_abort(
    "{.arg workspace} must be a workspace ID string or an Airtable URL
    with a workspace ID, not {.obj_type_friendly {workspace}}.",
    call = call
  )
}
