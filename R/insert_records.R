#' Insert records into an Airtable table
#'
#' Insert or create new records in an existing Airtable table from a data frame
#' input. The column names and types for the input data must match the column
#' names and types in the Airtable table. Fields are not validated before
#' calling the API so supplying any other names or types returns an error.
#' [insert_records()] and [create_records()] are identical.
#'
#' @param data A data frame with records to insert or create.
#' @inheritParams request_airtable
#' @inheritParams req_create_records
#' @inheritParams return_data_resp
#' @param batch_size Deprecated. Set using the `rairtable.batch_size` option.
#' @return A data frame (invisibly) of the input data, to be stored as an object
#'   or piped into additional functions or, if as_json is `TRUE`, the function
#'   returns the parsed API response from [httr2::resp_body_json()].
#'
#' @export
insert_records <- function(data,
                           airtable = NULL,
                           typecast = FALSE,
                           batch_size = deprecated(),
                           return_data = TRUE,
                           ...) {
  # TODO: Add support for inserting records from a list
  check_data_frame(data)

  if (is_empty(data)) {
    cli_abort("{.arg data} can't be an empty data frame.")
  }

  n_records <- nrow(data)

  resp <- req_create_records(
    airtable = airtable,
    ...,
    data = data,
    typecast = typecast
  )

  cli::cli_progress_step(
    "{n_records} record{?s} created.",
    msg_failed = "Can't create records."
  )

  return_data_resp(data, resp, return_data)
}

#' @rdname insert_records
#' @name create_records
#' @export
create_records <- function(data,
                           airtable = NULL,
                           typecast = FALSE,
                           batch_size = deprecated(),
                           return_data = TRUE,
                           ...) {
  insert_records(
    data = data,
    airtable = airtable,
    typecast = typecast,
    batch_size = batch_size,
    return_data = return_data,
    ...
  )
}
