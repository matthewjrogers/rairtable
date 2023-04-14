#' Create a new airtable object
#'
#' Creates an S3 airtable object, which serves as a pointer for rairtable
#' functions
#'
#' @param table Table name in Airtable
#' @param base Airtable base containing table. A base functions like a schema in
#'   a traditional database. You can retrieve the base ID from the API
#'   documentation.
#' @param view Optional view of data to read
#' @param fields An optional `airtable_fields_schema` object. Automatically
#'   populated in tables created by `airtable_base()`
#' @param api_url API endpoint to connect to. Can be changed for API
#'   integrations that require custom endpoint
#' @param api_version Version of API to use. Defaults to 0 (the current version
#'   as of Fall 2021)
#'
#' @return An airtable object
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
                               api_version = NULL) {
  check_string(table)

  if (is_airtable_url(table)) {
    ids <- parse_airtable_url(table)
    table <- ids[["table"]]
    base <- ids[["base"]]
    view <- ids[["view"]]
  }

  check_string(base)
  check_string(view, allow_null = TRUE)
  stopifnot(is.list(fields))

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
      table = table
    )[["url"]]

  atbl
}

#' Does x inherit the airtable class?
#'
#' @noRd
is_airtable_obj <- function(x) {
  inherits(x, "airtable")
}

#' Validate a new airtable object
#'
#' @noRd
check_airtable_obj <- function(x,
                               require_url = TRUE,
                               require_table = TRUE,
                               require_view = FALSE,
                               require_fields = FALSE,
                               call = caller_env()) {
  if (!is_airtable_obj(x)) {
    cli_abort("The provided airtable object is not of class `airtable`")
  }

  check_string(attr(x, "base"), call = call)

  if (require_table) {
    check_string(x$table, call = call)
  }

  if (require_url) {
    url <- attr(x, "request_url")

    check_airtable_api_url(
      url,
      require_table = require_table,
      require_view = require_view
    )
  }

  if (require_view) {
    check_string(attr(x, "view"), call = call)
  }

  if (require_fields) {
    fields <- x$fields

    if (!is.list(fields)) {
      cli_abort("The fields of the provided Airtable object is not a list")
    }

    if (!inherits(fields, "airtable_fields_schema")) {
      cli_abort("The fields of the provided Airtable object must be of class `airtable_fields_schema`")
    }
  }
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


#' str method for an airtable object
#'
#' @keywords internal
#' @export
str.airtable <- function(object, ...) {
  view <- ifelse(is.null(attr(object, "view")), "", paste0('."', attr(object, "view"), '"'))
  cat(" Airtable: ", object$table, view, " @ ", attr(object, "base"), "\n", sep = "")
}
