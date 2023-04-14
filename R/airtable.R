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
#' @inheritParams check_airtable_obj
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
                     require_url = TRUE,
                     require_fields = FALSE) {
  atbl <- build_airtable_obj(
    table = table,
    base = base,
    view = view,
    fields = fields,
    api_url = api_url,
    api_version = api_version
  )

  check_airtable_obj(
    atbl,
    require_url = require_url,
    require_fields = require_fields
  )

  atbl
}

#' Create a new airtable object
#'
#' @noRd
build_airtable_obj <- function(table,
                               base,
                               view = NULL,
                               fields = list(),
                               api_url = NULL,
                               api_version = NULL,
                               call = caller_env()) {
  check_string(table, call = call)

  if (is_airtable_url(table)) {
    ids <- parse_airtable_url(table, call = call)
    table <- ids[["table"]]
    base <- ids[["base"]]
    view <- ids[["view"]]
  }

  check_string(base, call = call)
  check_string(view, allow_null = TRUE, call = call)

  atbl <- new.env()
  atbl$table <- table
  atbl$fields <- fields

  class(atbl) <- "airtable"
  attr(atbl, "base") <- base
  attr(atbl, "view") <- view %||% character()

  attr(atbl, "request_url") <-
    airtable_request(
      api_url = api_url,
      api_version = api_version,
      base = base,
      table = table,
      call = call
    )[["url"]]

  atbl
}

#' print method for an airtable object
#'
#' @keywords internal
#' @export
print.airtable <- function(x, ...) {
  cat("Table: ", x$table, "\n", sep = "")
  if (!is.null(attr(x, "view"))) {
    cat("   View: ", attr(x, "view"), "\n", sep = "")
  }
  cat("   Base: ", attr(x, "base"), "\n", sep = "")
}
