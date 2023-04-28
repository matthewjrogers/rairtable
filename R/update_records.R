#' Update records in an Airtable table
#'
#' Update one or more fields in an Airtable table.
#'
#' @param data A `data.frame` containing the fields to update. If records is
#'   `NULL`, data must include a column of record IDs (named by airtable_id_col)
#'   or rownames with record IDs.
#' @param airtable An `airtable` object. Optional if url or base and table are
#'   provided with additional parameters passed to [airtable_request()].
#' @param airtable_id_col Column containing Airtable record IDs. Not required if
#'   record IDs are stored in row names as returned from [read_airtable()]
#' @param columns Columns in the data to update on Airtable. Can be a vector of
#'   character strings, unquoted column names, or a tidyselect helper. Defaults
#'   to [tidyselect::everything()]. See
#'   <https://tidyselect.r-lib.org/reference/index.html#language-and-helpers>
#'   for documentation on helpers.
#' @param records A character vector of record IDs indicating which Airtable
#'   records to replace with the updated values from data. Optional if data has
#'   a record ID column (named by airtable_id_col) or rownames with record IDs.
#'   Defaults to `NULL`.
#' @param safely If `TRUE`, confirm number and names of columns to update and
#'   number of rows before executing update.
#' @param parallel If `TRUE` use parallel processing for encoding large tables.
#' @param return_json If `TRUE`, return JSON repsponse from the Airtable web API
#'   as a list. If `FALSE` (default), return input data.
#' @inheritDotParams airtable_request -api_url -api_version -call
#' @return A `data.frame` (invisibly) of the input data, to be stored as an object
#'   or piped into further additional functions.
#'
#' @export
#' @importFrom tidyselect everything
#' @importFrom cli cli_alert_success
#' @importFrom httr2 resp_body_json
update_records <- function(data,
                           airtable = NULL,
                           airtable_id_col = NULL,
                           columns = tidyselect::everything(),
                           records = NULL,
                           safely = TRUE,
                           return_json = FALSE,
                           parallel = FALSE,
                           ...) {
  check_data_frame(data)
  check_airtable_obj(airtable, allow_null = TRUE)

  if (is_null(records)) {
    airtable_id_col <- airtable_id_col %||%
      getOption("rairtable.id_col", "airtable_record_id")
    records <- get_record_id_col(data = data, id_col = airtable_id_col)
  }

  check_character(records)

  update_data <- get_data_columns(
    data = data,
    columns = columns,
    id_col = airtable_id_col
  )

  update_col_names <- get_data_colnames(columns, data = update_data)

  n_records <- nrow(update_data)

  safety_check(
    safely = safely,
    c(
      ">" = "Updating values for the {.field {update_col_names}} field{?s} in
      {n_records} record{?s}."
    ),
    message = "Record update cancelled."
  )

  resp <- req_update_records(
    airtable = airtable,
    ...,
    records = records,
    data = update_data,
    parallel = parallel
  )

  cli::cli_alert_success("{n_records} record{?s} updated.")

  if (return_json) {
    if (!inherits(resp, "httr2_response")) {
      # FIXME: The body from the batched requests should be combined if possible
      return(lapply(resp, httr2::resp_body_json))
    }

    return(httr2::resp_body_json(resp))
  }

  invisible(data)
}

#' Update one or more records in an Airtable base
#'
#' For more information on the Airtable API, see
#' <https://airtable.com/developers/web/api/update-record> or
#' <https://airtable.com/developers/web/api/update-multiple-records>.
#'
#' @param req A HTTP response created by [req_query_airtable()]. Optional if
#'   airttable, url, *or* base and table are passed to [airtable_request()].
#' @inheritDotParams airtable_request -api_url -api_version
#' @inheritParams req_query_airtable
#' @param records,record Record ID or IDs to update as a character vector.
#'   Required.
#' @param parallel If `TRUE`, use parallel processing for encoding large tables.
#' @keywords internal
#' @export
#' @importFrom httr2 req_body_json req_perform
req_update_records <- function(req = NULL,
                               ...,
                               records,
                               data,
                               typecast = FALSE,
                               method = NULL,
                               token = NULL,
                               parallel = FALSE,
                               call = caller_env()) {
  method <- match.arg(method, c("PATCH", "PUT"))

  if (is_null(req)) {
    req <- req_query_airtable(
      .req = airtable_request(..., call = call),
      method = method,
      token = token,
      call = call
    )
  }

  check_character(records, call = call)
  batch_size <- as.integer(getOption("rairtable.batch_size", 10))
  n_records <- length(records)

  if (n_records == 1) {
    resp <- req_update_record(
      req = req,
      record = records,
      data = data,
      typecast = typecast,
      method = method,
      call = call
    )

    return(resp)
  }

  data <- make_field_list(data, parallel = parallel, call = call)

  if (n_records > batch_size) {
    resp <-
      batch_update_records(
        data = data,
        records = records,
        batch_size = batch_size,
        req = req,
        typecast = typecast,
        parallel = parallel,
        call = call
      )

    return(resp)
  }

  req <- httr2::req_body_json(
    req,
    data = list(
      "fields" = make_field_list(data, 1, parallel = parallel, call = call),
      "typecast" = typecast
      )
  )

  httr2::req_perform(req)
}

#' @rdname req_update_records
#' @name req_update_record
#' @export
#' @importFrom httr2 req_perform
req_update_record <- function(req = NULL,
                              ...,
                              record,
                              data,
                              typecast = FALSE,
                              method = NULL,
                              parallel = FALSE,
                              call = caller_env()) {
  method <- match.arg(method, c("PATCH", "PUT"))

  req <- req %||% airtable_request(..., call = call)

  check_string(record, allow_empty = FALSE, call = call)

  data <- list(
    "fields" = make_field_list(
      data, max_rows = 1,
      parallel = parallel, call = call
      )[[1]]
    )

  req <- req_query_airtable(
    .req = req,
    template = "/{record}",
    record = record,
    method = method,
    data = data,
    call = call
  )

  httr2::req_perform(req)
}

#' Call req_update_records for each batch in multiple batches of records
#'
#' @noRd
#' @importFrom cli cli_progress_along
batch_update_records <- function(req,
                                 data,
                                 records,
                                 batch_size = NULL,
                                 typecast = FALSE,
                                 action = "Updating records",
                                 parallel = FALSE,
                                 call = caller_env()) {
  batch_size <- batch_size %||%
    as.integer(getOption("rairtable.batch_size", 10))

  batched_records <-
    split_list(records, batch_size, parallel = parallel, call = call)

  batched_data <-
    split_list(data, batch_size, parallel = parallel, call = call)

  format <-
    "{cli::symbol$arrow_right} {action}: {cli::pb_bar} | {cli::pb_percent}"

  map(
    cli::cli_progress_along(
      batched_records,
      format = format
    ),
    ~ req_update_records(
      req = req,
      records = batched_records[[.x]],
      data = batched_data[[.x]],
      typecast = typecast,
      parallel = parallel,
      call = call
    )
  )
}


#' Get record ID column (or rownames) from data
#'
#' @noRd
#' @importFrom tibble has_rownames
get_record_id_col <- function(data,
                              id_col = NULL,
                              id_col_arg = caller_arg(id_col),
                              id_from_col = TRUE,
                              call = caller_env()) {
  check_data_frame(data, call = call)

  if (!id_from_col && tibble::has_rownames(data)) {
    return(row.names(data))
  }

  id_col <- id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")

  if (is_string(id_col) && (id_col != "")) {
    check_data_frame(data, call = call)
    data <- select_cols(id_col, data = data)

    if (ncol(data) != 1) {
      cli_abort(
        "{.arg {id_col_arg}} must select a single record ID column,
        not {ncol(data)} columns.",
        call = call
      )
    }

    return(data[[1]])
  }

  cli_abort(
    "{.arg {id_col_arg}} can't be {.code NULL} when
    {.arg data} has no rownames.",
    call = call
  )
}

#' Get selected data columns (optionally dropping id column)
#'
#' @noRd
get_data_columns <- function(data,
                             columns = NULL,
                             id_col = NULL,
                             call = caller_env()) {
  if (!is_null(id_col)) {
    check_name(id_col, call = call)
    data <- data[, names(data) != id_col]
  }

  select_cols(columns, data = data)
}

#' Get names of selected columns
#'
#' @noRd
get_data_colnames <- function(..., data) {
  names(select_cols(..., data = data))
}

#' Use tidyselect to pull a column from a data.frame object
#'
#' @noRd
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
select_cols <- function(..., data) {
  data[, tidyselect::eval_select(expr(c(...)), data = data)]
}
