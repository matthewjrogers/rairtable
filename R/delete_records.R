#' Delete records from an Airtable table
#'
#' Delete records in an Airtable table based on their Airtable record ID. Set
#' `safely = FALSE` to delete records outside of an interactive session.
#'
#' Find more information on the Airtable API methods to delete a record
#' <https://airtable.com/developers/web/api/delete-record> or multiple records
#' <https://airtable.com/developers/web/api/delete-multiple-records>.
#'
#' @param data A data frame with a record ID column matching the value of
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
#' @inheritParams request_airtable
#' @param return_records If `FALSE`, return the JSON response from the Airtable
#'   API. If `TRUE` (default), return a vector of deleted record IDs.
#' @inheritDotParams request_airtable -api_url -call
#' @returns A vector of deleted record IDs returned invisibly.
#'
#' @export
#' @importFrom cli cli_alert_warning
delete_records <- function(data = NULL,
                           airtable = NULL,
                           airtable_id_col = NULL,
                           records = NULL,
                           safely = NULL,
                           batch_size = deprecated(),
                           return_records = TRUE,
                           token = NULL,
                           ...) {
  if (is_null(data) && is_null(records)) {
    cli_abort(
      "{.arg data} or {.arg records} must be supplied."
    )
  }

  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")

  if (!is_null(data)) {
    if (!is_null(records)) {
      cli::cli_alert_warning(
        "{.arg records} is ignored when {.arg data} is supplied."
      )
    }

    if (has_name(data, airtable_id_col) || tibble::has_rownames(data)) {
      records <- get_record_id_col(data, id_col = airtable_id_col)
    } else {
      check_data_frame(data)
      cli_abort(
        "{.arg data} must be a data frame with a column named {airtable_id_col}
        or a data frame with rownames."
      )
    }
  }

  check_character(records)
  n_records <- length(records)

  text <- "Deleting {n_records} record{?s} from Airtable."

  if (!is_null(airtable[["name"]])) {
    text <- "Deleting {n_records} record{?s} from base:
    {.field {airtable[['name']]}}"
  }

  safety_check(
    safely = safely,
    text = c(">" = text),
    message = "Record deletion cancelled."
  )

  resp <- req_delete_records(
    airtable = airtable,
    ...,
    records = records,
    token = token
  )

  cli::cli_progress_step(
    "{n_records} record{?s} deleted.",
    msg_failed = "Can't delete records."
  )

  return_data_resp(records, resp, return_records)
}


#' Delete one or more records from an Airtable table
#'
#' Delete one record from an Airtable table with [req_delete_record()] or delete
#' multiple records with [req_delete_records()].
#'
#' @inheritParams request_airtable
#' @param req If req is supplied, url or any additional parameters passed to ...
#'   are ignored.
#' @param records Required. Character vector with record IDs. If only one record
#'   is provided, the parameters are passed to [req_delete_record()]
#'   https://airtable.com/developers/web/api/delete-multiple-records.
#' @param batch If `TRUE` (default) and more than 10 records are provided, the
#'   records vector is split into batches of 10. If batch is `FALSE`, records
#'   must be length 10 or less. This default batch size is set by
#'   "rairtable.batch_size" option. The option default is 10 may be changed if
#'   the API limit is modified in the future.
#' @inheritDotParams request_airtable
#' @keywords internal
#' @importFrom httr2 req_url_path_append req_perform
req_delete_records <- function(req = NULL,
                               ...,
                               records,
                               token = NULL,
                               call = caller_env()) {
  check_character(records, call = call)
  n_records <- length(records)

  if (n_records == 1) {
    resp <- req_delete_record(
      req = req,
      ...,
      record = records,
      token = token,
      call = call
    )

    return(resp)
  }

  req <- req_airtable(
    .req = req %||% request_airtable(..., call = call),
    method = "DELETE",
    token = token,
    call = call
  )

  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

  if (n_records > batch_size) {
    batched_records <- split_list(records, batch_size, call = call)

    resp <-
      map_along(
        batched_records,
        function(i) {
          req_delete_records(
            records = batched_records[[i]],
            req = req,
            call = call
          )
        },
        action = "Deleting records"
      )

    return(resp)
  }

  # FIXME: There is likely a better way to do this with httr2::req_url_query()
  req <- httr2::req_url_path_append(
    req,
    paste0("?", paste0("records=", records, collapse = "&"))
  )

  httr2::req_perform(req, error_call = call)
}

#' @rdname req_delete_records
#' @name req_delete_record
#' @keywords internal
#' @importFrom httr2 req_perform
req_delete_record <- function(req = NULL,
                              ...,
                              record,
                              token = NULL,
                              call = caller_env()) {
  req <- req_airtable(
    .req = req %||% request_airtable(..., call = call),
    record = record,
    template = "/{record}",
    method = "DELETE",
    token = token,
    call = call
  )

  httr2::req_perform(req, error_call = call)
}
