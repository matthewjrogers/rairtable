#' Create a new airtable object
#'
#' Creates an S3 airtable object, which serves as a pointer for rairtable
#' functions
#'
#' @param table Airtable table ID or name. Table ID values start with "tbl". If
#'   table is an Airtable URL matching the pattern
#'   "https://airtable.com/{baseID}/{tableIdOrName}/{viewId}" the URL is parsed
#'   to supply the values for base and table.
#' @param base Airtable base ID. Base ID values start with "app". Optional if
#'   table is a URL containing a base ID or required otherwise.
#' @param view Optional Airtable view ID. View ID values starts with "viw".
#' @param fields An optional `airtable_fields_schema` object. Automatically
#'   populated in tables created by `airtable_base()`.
#' @inheritParams airtable_request
#' @param ... Additional parameters passed to [check_airtable_obj()]. Primarily
#'   intended for internal use.
#'
#' @return An `airtable` class list of table and fields stored as values of the
#'   list and the base, view, and request_url stored as attributes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' table <- airtable("Table 1", "appXXXXXXXXXXXXX")
#' }
airtable <- function(table = NULL,
                     base = NULL,
                     view = NULL,
                     fields = list(),
                     api_url = NULL,
                     api_version = NULL,
                     token = NULL,
                     ...) {
  atbl <- new_airtable_obj(
    base = base,
    table = table,
    view = view,
    fields = fields,
    api_url = api_url,
    api_version = api_version,
    token = token
  )

  check_airtable_obj(
    atbl,
    ...
  )

  atbl
}

#' Create a new airtable object
#'
#' @noRd
#' @importFrom vctrs new_vctr
new_airtable_obj <- function(base,
                             table,
                             view = NULL,
                             fields = NULL,
                             api_url = NULL,
                             api_version = NULL,
                             token = NULL,
                             call = caller_env()) {
  check_string(table, call = call)

  if (is_airtable_url(table)) {
    ids <- parse_airtable_url(table, call = call)
    base <- ids[["base"]]
    table <- ids[["table"]]
    view <- ids[["view"]]
  }

  check_string(base, call = call)
  check_character(view, allow_null = TRUE, call = call)

  req <- airtable_request(
    base = base,
    table = table,
    api_url = api_url,
    api_version = api_version,
    call = call
  )

  base_info <- list_airtable_bases(
    base = base,
    token = token,
    call = call
  )

  name <- base_info[["name"]]
  permissions <- base_info[["permissionLevel"]]
  base_url <- getOption("rairtable.base_url", "https://airtable.com/")
  table_url <- as.character(glue("{base_url}{base}/{table}"))
  request_url <- req[["url"]]

  vctrs::new_vctr(
    .data = vctrs::list_drop_empty(
      list(
        "name" = name,
        "base" = base,
        "table" = table,
        "view" = view %||% list(),
        "fields" = fields %||% list(),
        "permissions" = permissions,
        "table_url" = table_url,
        "request_url" = request_url
      )
    ),
    class = "airtable"
  )
}

#' @export
format.airtable <- function(x, ...) {
  formatC(vctrs::vec_data(x))
}

#' @export
vec_ptype_abbr.airtable <- function(x, ...) {
  "atbl"
}

#' @export
vec_ptype_full.airtable <- function(x, ...) {
  "airtable"
}

#' @export
print.airtable <- function(x, ...) {
  cli::cli_text("{.cls {class(x)}}")

  if (!is_empty(x$name)) {
    cli::cli_text("{cli::symbol$line} {.val {x$name}}")
  }

  cli::cli_rule("{.url {x$table_url}}")

  text <- "Base: {.field {x$base}}"

  if (!is_empty(x$table)) {
    text <- c(text, "Table: {.field {x$table}}")
  }

  if (!is_empty(x$view)) {
    text <- c(text, "View: {.field {x$view}}")
  }

  if (!is_empty(x$fields)) {
    text <- c(text, "Fields: {.field {x$fields}}")
  }

  cli::cli_bullets(text)
  invisible(x)
}
