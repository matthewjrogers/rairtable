#' Is x an Airtable base ID?
#'
#' Return TRUE if x is a string starting with "app".
#'
#' @param x Object to test.
#' @noRd
is_base_id <- function(x) {
  if (is_null(x) || !is_string(x)) {
    return(FALSE)
  }

  grepl("^app[[:alnum:]]+$", x)
}

#' Is x an Airtable table ID?
#'
#' Return TRUE if x is a string starting with "tbl" or matches the supplied
#' table_name.
#'
#' @param x Object to test.
#' @noRd
is_table_id <- function(x, table_name = NULL) {
  if (is_null(x) || !is_string(x)) {
    return(FALSE)
  }

  if (is_null(table_name)) {
    # FIXME: table_name is not exposed so there is no option to pass a table
    # name to is_table_id from get_table_id
    return(grepl("^tbl[[:alnum:]]+$", x))
  }

  grepl(paste0("^", table_name, "$"), x)
}

#' Is x an Airtable field ID?
#'
#' Return TRUE if x is a string starting with "fld".
#'
#' @param x Object to test.
#' @noRd
is_field_id <- function(x) {
  if (is_null(x) || !is_string(x)) {
    return(FALSE)
  }

  grepl("^fld[[:alnum:]]+$", x)
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
                         call = caller_env()) {
  table <- table %||% airtable %||% url

  if (is_table_id(table, table_name)) {
    return(table)
  }

  if (is_airtable_obj(table)) {
    return(airtable[["table"]])
  }

  if (is_url(table)) {
    return(parse_url_table_id(url, table_name, call))
  }

  cli_abort(
    "{.arg table} must be a table ID string, an {.cls airtable} object,
    or an Airtable URL, not {.obj_type_friendly {table}}.",
    call = call
  )
}

#' Get an Airtable column/field ID from a string or URL
#'
#' Return the provided field ID or a field ID extracted from a URL.
#'
#' @noRd
get_field_id <- function(column = NULL,
                         url = NULL,
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
