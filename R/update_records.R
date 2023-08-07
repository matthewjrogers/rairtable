#' Update records in an Airtable table
#'
#' Update one or more fields in an Airtable table based on a data frame. Set
#' `safely = FALSE` to delete records outside of an interactive session.
#'
#' Find more information on the Airtable API methods for [updating a single
#' record](https://airtable.com/developers/web/api/update-record) or [multiple
#' records](https://airtable.com/developers/web/api/update-multiple-records).
#'
#' @param data A data frame containing the fields to update. If records is
#'   `NULL`, data must include a column of record IDs (named by airtable_id_col)
#'   or rownames with record IDs. If the data frame is originally created using
#'   `list_records()`, make sure to drop the "createdTime" and/or "commentCount"
#'   columns.
#' @param airtable An `airtable` object. Optional if url or base and table are
#'   provided with additional parameters passed to [request_airtable()].
#' @param airtable_id_col Column containing Airtable record IDs. Not required if
#'   record IDs are stored in row names as returned from [read_airtable()]
#' @param columns Columns in the data to update on Airtable. Can be a vector of
#'   character strings, unquoted column names, or a tidyselect helper. Defaults
#'   to [tidyselect::everything()]. See
#'   <https://tidyselect.r-lib.org/reference/index.html#language-and-helpers>
#'   for documentation on helpers.
#' @param records A character vector of record IDs indicating which Airtable
#'   records to replace with the updated values from data. A vector of record or
#'   cell URLs is also supported. Optional if data has a record ID column (named
#'   by airtable_id_col) or rownames with record IDs. Defaults to `NULL`.
#' @param safely If `TRUE`, confirm number and names of columns to update and
#'   number of rows before executing update. Defaults to `NULL` which sets
#'   safely based on the `rairtable.safely` option (which defaults to `TRUE`).
#' @inheritParams return_data_resp
#' @inheritDotParams request_airtable -api_url -call
#' @return A data frame of the input data, to be stored as an object or piped
#'   into further additional functions. The data frame is returned invisibly.
#'
#' @export
#' @importFrom tidyselect everything
update_records <- function(data,
                           airtable = NULL,
                           airtable_id_col = NULL,
                           columns = tidyselect::everything(),
                           records = NULL,
                           model = NULL,
                           fields_by_id = FALSE,
                           typecast = FALSE,
                           return_data = TRUE,
                           safely = NULL,
                           token = NULL,
                           ...) {
  if (is_null(records)) {
    airtable_id_col <- airtable_id_col %||%
      getOption("rairtable.id_col", "airtable_record_id")
    records <- get_record_id_col(data = data, id_col = airtable_id_col)
  } else if (all(is_url(records))) {
    records <- parse_url_record_id(records)
  }

  check_character(records)
  n_records <- length(records)

  update_data <- get_data_columns(
    data = data,
    columns = columns,
    id_col = airtable_id_col
  )

  update_fields <- get_data_colnames(columns, .data = update_data)

  if (!is_null(model)) {
    update_fields <- try_fetch(
      field_name_match(
        update_fields,
        model = model,
        error_arg = "fields"
      ),
      error = function(cnd) {
        cli_abort(
          c("{.arg data} has column names that can't be found
            in the fields of the supplied {.arg model}"),
          parent = cnd
        )
      }
    )
  }

  safety_check(
    safely = safely,
    c(
      ">" = "Updating values for the {.field {update_fields}} field{?s} in
      {n_records} record{?s}."
    ),
    message = "Record update cancelled."
  )

  resp <- req_update_records(
    airtable = airtable,
    ...,
    records = records,
    data = update_data,
    fields_by_id = fields_by_id,
    typecast = typecast,
    token = token
  )

  cli::cli_progress_step(
    "{n_records} record{?s} updated.",
    msg_failed = "Can't update records."
  )

  return_data_resp(data, resp, return_data)
}

#' Update one or more records in an Airtable base
#'
#' @param req A HTTP response created by [req_airtable()]. Optional if
#'   airtable, url, *or* base and table are passed to [request_airtable()].
#' @inheritDotParams request_airtable -api_url
#' @param fields_by_id If `TRUE`, the response to the API request is keyed by
#'   field IDs instead of field names. The response is only returned if
#'   `return_data = FALSE`.
#' @inheritParams req_airtable
#' @param records,record Record ID or IDs to update as a character vector.
#' @keywords internal
#' @importFrom httr2 req_body_json req_perform
req_update_records <- function(req = NULL,
                               ...,
                               records,
                               data,
                               fields_by_id = FALSE,
                               typecast = FALSE,
                               method = NULL,
                               token = NULL,
                               call = caller_env()) {
  method <- match.arg(method, c("PATCH", "PUT"))

  if (is_null(req)) {
    req <- req_airtable(
      .req = request_airtable(..., call = call),
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

  data <- make_list_of_lists(data, call = call)

  if (n_records > batch_size) {
    batched_records <- split_list(records, batch_size, call = call)

    batched_data <- split_list(data, batch_size, call = call)

    resp <- map_along(
      batched_records,
      function(i) {
        req_update_records(
          req = req,
          records = batched_records[[i]],
          data = batched_data[[i]],
          fields_by_id = fields_by_id,
          typecast = typecast,
          call = call
        )
      },
      action = "Updating records"
    )

    return(resp)
  }

  req <- httr2::req_body_json(
    req,
    data = list(
      "fields" = make_list_of_lists(data, 1, call = call),
      "returnFieldsByFieldId" = fields_by_id,
      "typecast" = typecast
    )
  )

  httr2::req_perform(req, error_call = call)
}

#' @rdname req_update_records
#' @name req_update_record
#' @keywords internal
#' @importFrom httr2 req_perform
req_update_record <- function(req = NULL,
                              ...,
                              record,
                              data,
                              typecast = TRUE,
                              method = NULL,
                              token = NULL,
                              call = caller_env()) {
  method <- match.arg(method, c("PATCH", "PUT"))

  check_string(record, allow_empty = FALSE, call = call)

  data <- list(
    "fields" = make_list_of_lists(
      data,
      max_rows = 1,
      call = call
    )[[1]],
    "typecast" = typecast
  )

  req <- req_airtable(
    .req = req %||% request_airtable(..., call = call),
    template = "/{record}",
    record = record,
    method = method,
    data = data,
    token = token,
    call = call
  )

  httr2::req_perform(req, error_call = call)
}

#' Get record ID column (or rownames) from data
#'
#' @noRd
#' @importFrom tibble has_rownames
#' @importFrom tidyselect all_of
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

    if (is_empty(data)) {
      cli_abort(
        "{.arg data} can't be empty.",
        call = call
      )
    }

    data <- select_cols(tidyselect::all_of(id_col), .data = data, call = call)
    n_cols <- ncol(data)

    if (n_cols != 1) {
      cli_abort(
        "{.arg {id_col_arg}} must select a single record ID column,
        not {n_cols} column{?s}.",
        call = call
      )
    }

    if (!is_character(data[[1]])) {
      cli_abort(
        "Records in {.val {id_col}} column must be a character vector,
          not {.obj_simple_type {data[[1]]}}.",
        call = call
      )
    }

    return(data[[1]])
  }

  cli_abort(
    "{.arg {id_col_arg}} can't be empty when
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
  check_data_frame(data, call = call)

  if (!is_null(id_col)) {
    check_name(id_col, call = call)
    data <- data[, names(data) != id_col, drop = FALSE]
  }

  select_cols(columns, .data = data, call = call)
}

#' Get names of selected columns
#'
#' @noRd
get_data_colnames <- function(..., .data, .drop = FALSE, call = caller_env()) {
  names(select_cols(..., .data = .data, .drop = .drop, call = call))
}

#' Use tidyselect to pull a column from a data frame
#'
#' @noRd
#' @importFrom tidyselect eval_select
#' @importFrom rlang expr
select_cols <- function(..., .data, .drop = FALSE, call = caller_env()) {
  .data[,
    tidyselect::eval_select(
      expr(c(...)),
      data = .data,
      error_call = call
    ),
    drop = .drop
  ]
}
