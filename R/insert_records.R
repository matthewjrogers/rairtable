#' Insert records into an Airtable table
#'
#' Create new records in an Airtable table from a data.frame or list. The column
#' names and types for the input data must exactly match the column names and
#' types in the Airtable table. These fields are not validated before calling
#' the API but supplying any other names or types returns a 422 Unprocessable
#' Entity error. Supports batch insert.
#'
#' @param data A data.frame or list with records to insert.
#' @inheritParams airtable_request
#' @inheritParams req_create_record
#' @param return_json If `TRUE`, return the response from the Airtable API as
#'   list. If `FALSE` (default), return the input data.frame or list.
#' @param parallel If `TRUE`, use parallel processing for encoding large tables.
#'   Not currently supported in development version.
#' @param batch_size Deprecated. Set using the `rairtable.batch_size` option.
#' @return A data.frame (invisibly) of the input data, to be stored as an object
#'   or piped into additional functions or, if as_json is `TRUE`, the function
#'   returns the parsed API response from [httr2::resp_body_json()].
#'
#' @aliases create_records
#' @export
insert_records <- function(data,
                           airtable = NULL,
                           ...,
                           typecast = FALSE,
                           parallel = FALSE,
                           return_json = FALSE,
                           batch_size = deprecated()) {
  if (parallel) {
    cli::cli_warn(
      "{.arg parallel} is currently not supported in this development version."
    )
  }

  check_airtable_obj(airtable, allow_null = TRUE)

  resp <- req_create_record(
    airtable = airtable,
    ...,
    data = data,
    typecast = typecast
  )

  if (return_json) {
    return(resp)
  }

  invisible(data)
}
