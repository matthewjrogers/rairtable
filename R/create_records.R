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
#' @inheritParams req_query_airtable
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @returns Invisibly return response from API call.
#' @export
#' @inheritParams rlang::args_error_context
#' @inheritParams airtable_request
req_create_records <- function(url = NULL,
                               ...,
                               data,
                               typecast = TRUE,
                               token = NULL,
                               call = caller_env()) {
  check_required(data, call = call)
  check_logical(typecast, call = call)
  data <- make_field_list(data, call = call)

  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

  if (length(data) > batch_size) {
    batched_data <- split_list(data, batch_size)
    batch_req_create <- Vectorize(
      req_create_records,
      vectorize.args = "data"
    )

    return(
      batch_req_create(
        url = url,
        data = batched_data,
        ...,
        typecast = typecast,
        token = token,
        call = call
      )
    )
  }

  req <- airtable_request(url = url, ..., call = call)

  data <- rlang::set_names(data, rep("fields", length(data)))

  req <- req_query_airtable(
    .req = req,
    data = list("records" = split_list(data, 1), "typecast" = typecast),
    token = token,
    call = call
  )

  resp <- httr2::req_perform(req)

  invisible(httr2::resp_body_json(resp))
}

#' @rdname req_create_records
#' @name req_create_record
#' @export
req_create_record <- function(url = NULL,
                              data,
                              ...,
                              typecast = TRUE,
                              call = caller_env(),
                              token = NULL) {
  check_logical(typecast, call = call)
  req <- airtable_request(url = url, ..., call = call)
  data <- make_field_list(data, call = call)

  req <- req_query_airtable(
    .req = req,
    token = token,
    data = list("fields" = data, "typecast" = typecast),
    call = call
  )

  resp <- httr2::req_perform(req)

  invisible(httr2::resp_body_json(resp))
}
