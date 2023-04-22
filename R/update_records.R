#' Update Airtable records
#'
#' Update one or more columns of data in an Airtable table. Supports batch
#' updates and parallel JSON encoding (recommended for large tables).
#'
#' @param data A data.frame containing the fields to update and, if records is
#'   `NULL`, a column or rownames with record IDs.
#' @param airtable An airtable object. Optional if url or base and table are
#'   provided with additional parameters passed to [airtable_request()].
#' @param columns Columns in the data to update on Airtable. Can be a vector of
#'   character strings, unquoted column names, or a tidyselect helper like
#'   [tidyselect::starts_with()], [tidyselect::ends_with()] or
#'   [tidyselect::everything()]. Defaults to [tidyselect::everything()]
#' @param airtable_id_col Column containing Airtable record IDs. Not required if
#'   record IDs are stored in row names as returned from [read_airtable()]
#' @param safely If `TRUE`, confirm number and names of columns to update and
#'   number of rows before executing update.
#' @param parallel If `TRUE` use parallel processing for encoding large tables.
#'   Currently not implemented in development version.
#' @param return_json If `TRUE`, return JSON repsponse from the Airtable web API
#'   as a list. If `FALSE` (default), return input data.
#' @inheritDotParams airtable_request -api_url -api_version -call
#' @return A data.frame (invisibly) of the input data, to be stored as an object
#'   or piped into further additional functions.
#'
#' @export
#' @importFrom tidyselect everything
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
  check_logical(safely)
  check_logical(parallel)

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

  update_col_names <- get_data_col_names(columns, data = update_data)

  n_records <- nrow(update_data)
  safety_check(
    safely = safely,
    c(
      ">" = "Updating values for the {.field {update_col_names}} field{?s} in
      {n_records} record{?s}."
    ),
    message = "Record update cancelled."
  )

  cli_alert_success("Updated {n_records} record{?s}.")

  body <- req_update_records(
    airtable = airtable,
    ...,
    records = records,
    data = update_data
  )

  if (return_json) {
    return(body)
  }

  invisible(data)
}

#' Update one or more records
#'
#' https://airtable.com/developers/web/api/update-record
#' https://airtable.com/developers/web/api/update-multiple-records
#'
#' @inheritParams req_query_airtable
#' @param records,record Record ID or IDs to update as a list or character
#'   vector.
#' @keywords internal
#' @export
#' @importFrom httr2 req_perform resp_body_json
req_update_records <- function(url = NULL,
                               ...,
                               records,
                               data,
                               typecast = FALSE,
                               method = NULL,
                               call = caller_env()) {
  if (has_length(records, 1)) {
    return(req_update_record(
      url = url,
      ...,
      record = records,
      data = data,
      typecast = typecast,
      method = method,
      call = call
    ))
  }

  req <- airtable_request(url = url, ..., call = call)

  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

  data <- make_field_list(data, call = call)

  method <- match.arg(method, c("PATCH", "PUT"))
  check_character(records, call = call)

  if (length(records) > batch_size) {
    batched_records <- split_list(records, batch_size)
    batched_data <- split_list(data, batch_size)

    batch_req_update <- Vectorize(
      req_update_records,
      vectorize.args = c("records", "data")
    )

    return(
      batch_req_update(
        req = req,
        records = batched_records,
        data = batched_data,
        typecast = typecast,
        method = method,
        call = call
      )
    )
  }

  req <- req_query_airtable(
    .req = req,
    method = method,
    data = list("fields" = make_field_list(data, 1), "typecast" = typecast),
    call = call
  )

  resp <- httr2::req_perform(req)

  invisible(httr2::resp_body_json(resp))
}


#' @rdname req_update_records
#' @name req_update_record
#' @export
#' @importFrom httr2 req_perform resp_body_json
req_update_record <- function(url = NULL,
                              ...,
                              record,
                              data,
                              typecast = FALSE,
                              method = NULL,
                              call = caller_env()) {

  method <- match.arg(method, c("PATCH", "PUT"))

  req <- airtable_request(url = url, ..., call = call)

  check_string(record, call = call)

  data <- list("fields" = make_field_list(data, max_rows = 1)[[1]])

  req <- req_query_airtable(
    .req = req,
    method = method,
    template = "/{record}",
    record = record,
    data = data,
    call = call
  )

  resp <- httr2::req_perform(req)

  invisible(httr2::resp_body_json(resp))
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
  if (!id_from_col && tibble::has_rownames(data)) {
    return(row.names(data))
  }

  if (!is_null(id_col)) {
    data <- select_cols(id_col, data = data)

    if (ncol(data) > 1) {
      cli_abort(
        "{.arg {id_col_arg}} must select only a single record ID column.",
        call = call
      )
    }

    return(data[[1]])
  }

  cli_abort(
    "{.arg {id_col_arg}} can't be {.code NULL} when {.arg data} has no rownames",
    call = call
  )
}

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

#' Use tidyselect to pull a column from a data.frame object
#'
#' @noRd
#' @importFrom tidyselect eval_select
#' @importFrom rlang is_character expr
select_cols <- function(..., data) {
  data[, tidyselect::eval_select(rlang::expr(c(...)), data = data)]
}

#' Use select_cols helper function to return names of columns
#'
#' @noRd
get_data_col_names <- function(..., data) {
  names(select_cols(..., data = data))
}
