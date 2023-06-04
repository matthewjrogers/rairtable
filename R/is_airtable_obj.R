#' Test or check if an object is an airtable object
#'
#' [is_airtable_obj()] returns TRUE for airtable class objects or FALSE
#' otherwise. [check_airtable_obj()] errors if the input is not an airtable
#' object with the option to ignore `NULL` inputs. This check can also
#' optionally require an airtable object to include a request URL, a table, and
#' a view. Note that this is an internal function primarily intended for
#' developer use.
#'
#' @param x Object to test or check.
#' @keywords internal
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
#' @param allow_null If `FALSE` and x is `NULL`, returns an error. If `TRUE`,
#'   invisibly returns NULL.
#' @keywords internal
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
    not <- "not {.obj_type_friendly {airtable}}."
    if (is_url(x)) {
      not <- "not a url."
    }

    cli_abort(
      "{.arg airtable} must be an {.cls airtable} object, {not}",
      call = call
    )
  }

  check_string(x[["base"]], call = call)

  if (is_true(require_table)) {
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
      cli_abort(
        "{.arg airtable} must include a list of fields.",
        call = call
      )
    }

    if (!is_airtable_fields_schema(fields)) {
      cli_abort(
        "{.arg airtable} fields must have the
        {.cls airtable_fields_schema} class.",
        call = call
      )
    }
  }
}
