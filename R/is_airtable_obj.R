#' Test or check if an object is an airtable object
#'
#' @param x Object to test or check.
#' @keywords internal
#' @export
is_airtable_obj <- function(x) {
  inherits(x, "airtable")
}

#' @name check_airtable_obj
#' @rdname is_airtable_obj
#' @param require_url If `TRUE` (default), check that x includes a "request_url"
#'   attribute that is a valid Airtable API url.
#' @inheritParams check_airtable_api_url
#' @param require_fields If `TRUE`, check that x includes a "fields" value that
#'   is a list with the `airtable_fields_schema` class. Defaults to `FALSE`.
#' @keywords internal
#' @export
check_airtable_obj <- function(x,
                               require_url = TRUE,
                               require_table = TRUE,
                               require_view = FALSE,
                               require_fields = FALSE,
                               allow_null = FALSE,
                               call = caller_env()) {
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  if (!is_airtable_obj(x)) {
    cli_abort(
      "{.arg airtable} must be an {.cls airtable} object, not a
    {.obj_type_friendly {class(airtable)}}."
    )
  }

  check_string(x[["base"]], call = call)

  if (require_table) {
    check_string(x[["table"]], call = call)
  }

  if (require_url) {
    url <- x[["request_url"]]

    check_airtable_api_url(
      url,
      require_table = require_table,
      require_view = require_view,
      call = call
    )
  }

  if (require_view) {
    check_string(x[["view"]], call = call)
  }

  if (require_fields) {
    fields <- x[["fields"]]

    if (!is.list(fields)) {
      cli_abort("{.arg airtable} must include a list of fields.")
    }

    if (!inherits(fields, "airtable_fields_schema")) {
      cli_abort(
        "{.arg airtable} fields must have the
        {.cls airtable_fields_schema} class."
      )
    }
  }
}
