#' Delete records from an Airtable table
#'
#' Delete records in an Airtable table based on their Airtable record ID.
#'
#' @param data A data.frame with a record ID column matching the value of
#'   airtable_id_col or rownames with record ID values.
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
#' @inheritParams airtable_request
#' @param return_json If `TRUE`, return the JSON response from the Airtable API.
#'   If `FALSE` (default), return a vector of deleted record IDs.
#' @inheritDotParams airtable_request -api_url -api_version -call
#' @returns A vector of deleted record IDs returned invisibly.
#'
#' @export
#' @importFrom cli cli_alert_info
#' @importFrom httr2 resp_body_json
delete_records <- function(data = NULL,
                           airtable = NULL,
                           airtable_id_col = NULL,
                           records = NULL,
                           safely = NULL,
                           batch_size = deprecated(),
                           return_json = FALSE,
                           token = NULL,
                           ...) {
  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")
  check_airtable_obj(airtable, allow_null = TRUE)

  if (!is_null(data)) {
    records <- get_record_id_col(data, id_col = airtable_id_col)
  }

  n_records <- length(records)

  text <- "Deleting {n_records} record{?s} from Airtable."

  if (!is_null(airtable)) {
    text <-
      "Deleting {n_records} record{?s} from base: {.field {airtable$name}}"
  }

  safety_check(
    safely = safely,
    c(">" = text),
    message = "Record deletion cancelled."
  )

  resp <- req_delete_records(
    airtable = airtable,
    ...,
    records = records,
    token = token
  )

  cli::cli_alert_success("{n_records} record{?s} deleted.")

  if (return_json) {
    return(httr2::resp_body_json(resp))
  }

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
                               token = NULL,
                               call = caller_env()) {
  if (has_length(records, 1)) {
    return(req_delete_record(url = url, ..., record = records, call = call))
  }

  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

  check_string(records, call = call)
  n_records <- length(records)

  if (n_records > batch_size) {
    record_batches <- split_list(records, batch_size)
    batch_delete <- Vectorize(req_delete_records, vectorize.args = "records")
    return(batch_delete(url = url, ..., records = record_batches, call = call))
  }

  req <- airtable_request(url = url, ..., call = call)

  req <- req_query_airtable(
    .req = req,
    method = "DELETE",
    token = token,
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
#' @export
req_delete_record <- function(url = NULL,
                              ...,
                              record,
                              token = NULL,
                              call = caller_env()) {
  req <- airtable_request(url = url, ..., call = call)

  req <- req_query_airtable(
    .req = req,
    record = record,
    template = "/{record}",
    method = "DELETE",
    token = token,
    call = call
  )

  httr2::req_perform(req)
}
