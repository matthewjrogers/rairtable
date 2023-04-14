#' Delete records from an Airtable table
#'
#' Delete records in an Airtable table based on their Airtable record ID.
#'
#' @param data A data.frame with a record ID column matching the value of
#'   airtable_id_col or rownames with record ID values.
#' @param airtable An airtable class object.
#' @param airtable_id_col Column name or tidyselect function identifying column
#'   containing Airtable record IDs. Optional if records is supplied or if
#'   record IDs are stored in row names (default option for [read_airtable()]).
#'   Defaults to `NULL` which is set to `getOption("rairtable.id_col",
#'   "airtable_record_id")`.
#' @param records Character vector with record IDs. Optional if data is
#'   provided.
#' @param safely If `TRUE`, ask for confirmation before deleting Airtable
#'   records and abort if confirmation is not provided.
#' @param batch_size Deprecated. Number of records to delete in a single batch.
#'   batch_size can now be set using the "rairtable.batch_size" option.
#'
#' @returns A vector of deleted record IDs returned invisibly.
#'
#' @export
#' @importFrom cli cli_alert_info
delete_records <- function(data = NULL,
                           airtable,
                           airtable_id_col = NULL,
                           records = NULL,
                           safely = TRUE,
                           batch_size = deprecated()) {
  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")
  check_airtable_obj(airtable)

  records <- get_records(data, airtable_id_col, records)
  n_records <- length(records)
  check_character(records)
  check_bool(safely)

  safety_check(
    safely,
    cancel_message = "DELETE request cancelled.",
    c(
      "You are about to delete {n_records} Airtable record{s}.",
      "Do you wish to proceed?"
    )
  )

  req_delete_records(
    airtable = airtable,
    records = records
  )

  cli::cli_alert_info("Deleted {length(records)} record{s}.")

  invisible(records)
}


#' Delete one or more records from an Airtable table
#'
#' Delete one record from an Airtable table with [req_delete_record()] or delete
#' multiple records with [req_delete_records()].
#'
#' More information on the Airtable API:
#' - <https://airtable.com/developers/web/api/delete-record>
#' - <https://airtable.com/developers/web/api/delete-multiple-records>
#'
#' @inheritParams airtable_request
#' @param records Required. Character vector with record IDs. If only one record
#'   is provided, the parameters are passed to [req_delete_record()]
#'   https://airtable.com/developers/web/api/delete-multiple-records.
#' @param batch If `TRUE` (default) and more than 10 records are provided, the
#'   records vector is split into batches of 10. If batch is `FALSE`, records
#'   must be length 10 or less. This default batch size is set by
#'   "rairtable.batch_size" option. The option default is 10 may be changed if
#'   the API limit is modified in the future.
#' @inheritDotParams airtable_request
#' @keywords internal
#' @export
req_delete_records <- function(url = NULL,
                               ...,
                               records,
                               batch = TRUE,
                               call = caller_env()) {
  if (has_length(records, 1)) {
    return(req_delete_record(url = url, ..., records = records, call = call))
  }

  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

  check_string(records, call = call)
  n_records <- length(records)

  if (n_records > batch_size) {
    if (!batch) {
      cli_abort(
        c("{.arg records} must be length {batch_size}, not length {n_records}.",
          "*" = "Set {.code batch = TRUE} to delete more than {batch_size} record{s}."
        ),
        call = call
      )
    }

    record_batches <- split_list(records, batch_size)
    batch_delete <- Vectorize(req_delete_records, vectorize.args = "records")
    return(batch_delete(url = url, ..., records = record_batches, call = call))
  }

  check_number_whole(
    n_records,
    max = batch_size,
    message = "{.arg records} must be provided in a batch of {batch_size}
    or less, not {n_records}.",
    call = call
  )

  req <- airtable_request(url = url, ..., call = call)

  req <- req_airtable_query(
    .req = req,
    method = "DELETE",
    call = call
  )

  req <- httr2::req_url_path_append(
    req,
    paste0("?", paste0("records=", records, collapse = "&"))
  )

  httr2::req_perform(req)
}

#' @rdname req_delete_records
#' @name req_delete_record
#' @noRd
req_delete_record <- function(url = NULL, ..., record, call = caller_env()) {
  req <- airtable_request(url = url, ..., call = call)

  req <- req_airtable_query(
    .req = req,
    records = records,
    template = "/{records}",
    method = "DELETE",
    call = call
  )

  httr2::req_perform(req)
}
