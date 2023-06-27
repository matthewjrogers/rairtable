#' Use httr2 to make a POST request to create records in an Airtable table
#'
#' @name req_create_records
#' @inheritParams request_airtable
#' @inheritDotParams request_airtable -api_url -api_version
#' @param data A data frame or list of records to create in Airtable. Column
#'   names or the names of list values must match the field names used in the
#'   Airtable base. Required.
#' @param typecast If `TRUE` (default), values will be converted to match the
#'   base if possible. typecase must be set to `TRUE` to add new values to a
#'   multiselect field type.
#'   base. Required.
#' @inheritParams req_airtable
#' @inheritParams rlang::args_error_context
#' @returns Invisibly return response from API call.
#' @keywords internal
#' @inheritParams rlang::args_error_context
#' @inheritParams request_airtable
req_create_records <- function(req = NULL,
                               ...,
                               data,
                               typecast = TRUE,
                               token = NULL,
                               call = caller_env()) {
  req <- req %||%
    req_auth_airtable(
      req = request_airtable(..., call = call),
      token = token
    )

  data <- make_list_of_lists(data, call = call)
  n_records <- length(data)
  batch_size <- as.integer(getOption("rairtable.batch_size", 10))

  if (n_records > batch_size) {
    batched_data <- split_list(data, batch_size, call = call)

    resp <-
      map_along(
        batched_data,
        function(i) {
          req_create_records(
            req = req,
            data = batched_data[[i]],
            call = call
          )
        },
        action = "Creating records"
      )

    return(resp)
  }

  data <- set_names(data, rep_len("fields", n_records))

  check_bool(typecast, call = call)

  req <- httr2::req_body_json(
    req,
    data = list("records" = split_list(data, 1), "typecast" = typecast)
  )

  httr2::req_perform(req, error_call = call)
}
