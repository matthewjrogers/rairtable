#' Delete records from an Airtable table
#'
#' Delete records in an Airtable table based on their Airtable record ID. Set
#' `safely = FALSE` to delete records outside of an interactive session.
#'
#' Find more information on the Airtable API methods to delete a record
#' <https://airtable.com/developers/web/api/delete-record> or multiple records
#' <https://airtable.com/developers/web/api/delete-multiple-records>.
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
#' @importFrom cli cli_alert_warning
delete_records <- function(data = NULL,
                           airtable = NULL,
                           airtable_id_col = NULL,
                           records = NULL,
                           safely = NULL,
                           batch_size = deprecated(),
                           return_json = FALSE,
                           token = NULL,
                           ...) {
  if (is_null(data) && is_null(records)) {
    cli_abort(
      "{.arg data} or {.arg records} must be supplied."
    )
  }

  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")

  data_records <- !is_null(data) &&
    (has_name(data, airtable_id_col) || tibble::has_rownames(data))

  if (data_records) {
    if (!is_null(records)) {
      cli::cli_alert_warning(
        "{.arg records} is ignored when {.arg data} is supplied."
      )
    }

    records <- get_record_id_col(data, id_col = airtable_id_col)
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
    c(">" = text),
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

  if (return_json) {
    if (!is_httr2_resp(resp)) {
      return(c(map(lapply, httr2::resp_body_json)))
    }

    return(httr2::resp_body_json(resp))
  }

  invisible(records)
}


#' Delete one or more records from an Airtable table
#'
#' Delete one record from an Airtable table with [req_delete_record()] or delete
#' multiple records with [req_delete_records()].
#'
#' @inheritParams airtable_request
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
#' @inheritDotParams airtable_request
#' @keywords internal
#' @importFrom httr2 req_url_path_append req_perform
req_delete_records <- function(req = NULL,
                               ...,
                               records,
                               token = NULL,
                               call = caller_env()) {
  check_character(records, call = call)
  n_records <- length(records)
  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

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

  req <- req %||% airtable_request(..., call = call)

  req <- req_query_airtable(
    .req = req,
    method = "DELETE",
    token = token,
    call = call
  )

  if (n_records > batch_size) {
    batched_records <- split_list(records, batch_size)

    resp <-
      map_action_along(
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

  req <- httr2::req_url_path_append(
    req,
    paste0("?", paste0("records=", records, collapse = "&"))
  )

  httr2::req_perform(req)
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
  req <- req %||% airtable_request(..., call = call)

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
