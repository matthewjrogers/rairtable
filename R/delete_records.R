#' Delete airtable records
#'
#' Delete records in an Airtable table based on their Airtable record ID.
#'
#' @param data A data frame containing records to delete
#' @param airtable An airtable object
#' @param airtable_id_col Column containing Airtable record IDs. Not required if
#'   record IDs are stored in row names as returned from `read_airtable`.
#' @param safely If `TRUE`, ask for confirmation before executing DELETE request
#' @param batch_size Number of requests to send at a time. Maximum of 10.
#'
#' @return A vector of IDs deleted
#'
#' @export
#'
#' @importFrom utils menu
#' @importFrom crayon green
#' @importFrom crayon red
#' @importFrom cli symbol
#' @importFrom cli cli_inform
#' @importFrom rlang enexpr
#' @importFrom dplyr filter

delete_records <- function(data = NULL,
                           airtable,
                           airtable_id_col = deprecated(),
                           id_col = getOption("rairtable.id_col", "airtable_record_id"),
                           records = NULL,
                           safely = TRUE,
                           batch_size = deprecated()) {
  check_airtable_obj(airtable)

  if (is.data.frame(data)) {
    records <- get_ids(df = data, id_col = rlang::enexpr(id_col))
  } else if (is.character(data)) {
    records <- records %||% data
  }

  check_character(records)
  check_bool(safely)

  safety_check(
    safely,
    cancel_message = "DELETE request cancelled.",
    paste0("You are about to delete ", length(records), " Airtable records.\n\nDo you wish to proceed?")
  )

  url <- attr(airtable, "request_url")

  req_delete_records(
    url = url,
    records = records
  )

  cli::cli_inform("Deleted {length(records)} record{s}.")

  invisible(records)
}

#' Delete an individual record
#'
#' @noRd
req_delete_record <- function(url = NULL, ..., record, call = caller_env()) {
  req <- airtable_request(url = url, ...)

  req <- req_airtable_query(
    .req = req,
    records = records,
    template = "/{records}",
    method = "DELETE"
  )

  httr2::req_perform(req)
}

#' Delete multiple records
#'
#' https://airtable.com/developers/web/api/delete-multiple-records
#'
#' @noRd
req_delete_records <- function(url = NULL, ..., records, batch = TRUE, call = caller_env()) {
  if (has_length(records, 1)) {
    return(req_delete_record(url = url, ..., records = records, call = call))
  }

  check_string(records, call = call)
  n_records <- length(records)

  if ((n_records > 10) && is_true(batch)) {
    record_batches <- split_list(records, 10)
    batch_delete <- Vectorize(req_delete_records, vectorize.args = "records")
    return(batch_delete(url = url, ..., records = record_batches, call = call))
  }

  check_number_whole(
    n_records,
    max = 10,
    message = "{.arg records} must be provided in a batch of 10 or less, not {n_records}.",
    call = call
  )

  req <- airtable_request(url = url, ...)

  req <- req_airtable_query(
    .req = req,
    method = "DELETE"
  )

  req <- httr2::req_url_path_append(
    req,
    paste0("?", paste0("records=", records, collapse = "&"))
  )

  httr2::req_perform(req)
}
