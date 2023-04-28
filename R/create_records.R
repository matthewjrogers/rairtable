#' Use httr2 to make a POST request to create records in an Airtable table
#'
#' @name req_create_records
#' @inheritParams airtable_request
#' @inheritDotParams airtable_request -api_url -api_version
#' @param data A data.frame or list of records to create in Airtable. Column
#'   names or the names of list values must match the field names used in the
#'   Airtable base. Required.
#' @param typecast If `TRUE` (default), values will be converted to match the
#'   base if possible. typecase must be set to `TRUE` to add new values to a
#'   multiselect field type.
#'   base. Required.
#' @param parallel If `TRUE`, use parallel processing for encoding large tables.
#' @inheritParams req_query_airtable
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @returns Invisibly return response from API call.
#' @export
#' @inheritParams rlang::args_error_context
#' @inheritParams airtable_request
req_create_records <- function(req = NULL,
                               ...,
                               data,
                               typecast = TRUE,
                               token = NULL,
                               parallel = FALSE,
                               call = caller_env()) {
  req <- req %||%
    req_auth_airtable(
      req = airtable_request(..., call = call),
      token = token
    )

  data <- make_field_list(data, parallel = parallel, call = call)
  n_records <- length(data)
  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

  if (n_records > batch_size) {
    resp <- batch_create_records(
      req = req,
      data = data,
      batch_size = batch_size,
      parallel = parallel,
      call = call
    )

    return(resp)
  }

  data <- set_names(data, rep_len("fields", length(data)))

  check_bool(typecast, call = call)

  req <- httr2::req_body_json(
    req,
    data = list("records" = split_list(data, 1), "typecast" = typecast)
  )

  httr2::req_perform(req)
}

#' @rdname req_create_records
#' @name req_create_record
#' @export
req_create_record <- function(req = NULL,
                              data,
                              ...,
                              typecast = TRUE,
                              call = caller_env(),
                              token = NULL) {
  check_bool(typecast, call = call)

  req <- req_query_airtable(
    .req = req %||% airtable_request(..., call = call),
    data = list(
      "fields" = make_field_list(data, call = call),
      "typecast" = typecast
    ),
    token = token,
    call = call
  )

  httr2::req_perform(req)
}

#' Create records in an Airtable table in batches
#'
#' @noRd
batch_create_records <- function(req,
                                 data,
                                 batch_size = NULL,
                                 action = "Creating records",
                                 parallel = FALSE,
                                 call = caller_env()) {
  batch_size <- batch_size %||%
    as.integer(getOption("rairtable.batch_size", 10))

  batched_data <- split_list(data, parallel = parallel, call = call)

  format <-
    "{cli::symbol$arrow_right} {action}: {cli::pb_bar} | {cli::pb_percent}"

  map(
    cli::cli_progress_along(batched_data, format = format),
    ~ req_create_records(
      req = req,
      data = batched_data[[.x]],
      parallel = parallel,
      call = call
    )
  )
}
