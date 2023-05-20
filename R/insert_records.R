#' Insert records into an Airtable table
#'
#' Create new records in an Airtable table from a data.frame or list. The column
#' names and types for the input data must exactly match the column names and
#' types in the Airtable table. Fields are not validated before calling
#' the API so supplying any other names or types returns an error.
#' [create_records()] and [insert_records()] are identical.
#'
#' @param data A data.frame with records to insert.
#' @inheritParams airtable_request
#' @inheritParams req_create_record
#' @param return_json If `TRUE`, return the response from the Airtable API as
#'   list. If `FALSE` (default), return the input data.frame or list.
#' @param batch_size Deprecated. Set using the `rairtable.batch_size` option.
#' @return A data.frame (invisibly) of the input data, to be stored as an object
#'   or piped into additional functions or, if as_json is `TRUE`, the function
#'   returns the parsed API response from [httr2::resp_body_json()].
#'
#' @export
insert_records <- function(data,
                           airtable = NULL,
                           typecast = FALSE,
                           batch_size = deprecated(),
                           return_json = FALSE,
                           ...) {

  check_airtable_obj(airtable, allow_null = TRUE)
  check_data_frame(data)
  n_records <- nrow(data)

  cli::cli_alert_info("Creating {n_records} record{?s}.")

  resp <- req_create_records(
    airtable = airtable,
    ...,
    data = data,
    typecast = typecast
  )

  cli::cli_alert_success("{n_records} record{?s} created.")

  if (return_json) {
    if (!inherits(resp, "httr2_response")) {
      # FIXME: The body from the batched requests should be combined if possible
      return(lapply(resp, httr2::resp_body_json))
    }

    return(httr2::resp_body_json(resp))
  }

  invisible(data)
}

#' @rdname insert_records
#' @name create_records
#' @export
create_records <- function(data,
                           airtable = NULL,
                           typecast = FALSE,
                           batch_size = deprecated(),
                           return_json = FALSE,
                           ...) {
  insert_records(
    data = data,
    airtable = airtable,
    typecast = typecast,
    batch_size = batch_size,
    return_json = return_json,
    ...
  )
}
