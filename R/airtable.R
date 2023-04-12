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

airtable <- function(table,
                     base,
                     view = NULL,
                     fields = list(),
                     api_url = NULL,
                     api_version = NULL) {
  check_required(table)
  check_required(base)

  atbl <- new_airtable(
    table,
    base,
    view,
    fields,
    api_url = api_url,
    api_version = api_version
  )

  validate_airtable(atbl)

  atbl
}

#' Create a new airtable object
#'
#' @noRd
new_airtable <- function(table,
                         base,
                         view = NULL,
                         fields = list(),
                         api_url = NULL,
                         api_version = NULL) {

  check_string(table)
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
    req_airtable_url(
      api_url = api_url,
      api_version = api_version,
      base = base,
      table = table
      )[["url"]]

  validate_airtable(atbl)

  atbl
}

#' Validate a new airtable object
#'
#' @noRd
validate_airtable <- function(airtable_obj,
                              check_fields = FALSE,
                              call = caller_env()) {

  if (check_fields) {
    fields <- airtable_obj$fields

    if (!is.list(fields)) {
      cli_abort("The fields of the provided Airtable object is not a list")
    }

    if (!inherits(fields, "airtable_fields_schema")) {
      cli_abort("The fields of the provided Airtable object must be of class `airtable_fields_schema`")
    }
  }

  if (!inherits(airtable_obj, "airtable")) {
    cli_abort("The provided airtable object is not of class `airtable`")
  }

  check_string(attr(airtable_obj, "base"), call = call)
  check_string(airtable_obj$table, call = call)
  view <- attr(airtable_obj, "view")

  if (!is_empty(view)) {
    check_string(view, allow_null = TRUE, call = call)
  }

  if (!is_url(attr(airtable_obj, "request_url"))) {
    cli_abort("`request_url` cannot contain any spaces", call = call)
  }

}

#' print method for an airtable object
#'
#' @keywords internal
#' @export
print.airtable <- function(x, ...) {

  cat("Table: ", x$table, "\n", sep = "")
  if (!is.null(attr(x, 'view'))) {
    cat("   View: ", attr(x, 'view'), "\n", sep = "")
  }
  cat("   Base: ", attr(x, 'base'), "\n", sep = "")
}


#' str method for an airtable object
#'
#' @keywords internal
#' @export
str.airtable = function(object, ...) {

  view <- ifelse(is.null(attr(object, "view")), "", paste0('."', attr(object, "view"), '"'))
  cat(" Airtable: ", object$table, view, " @ ", attr(object, "base"), "\n", sep = "")

}

