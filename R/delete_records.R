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

delete_records <- function(data,
                           airtable,
                           airtable_id_col = 'airtable_record_id',
                           safely = TRUE,
                           batch_size = 10) {

  validate_airtable(airtable)
  check_data_frame(data)
  check_number_whole(batch_size, max = 10)
  check_bool(safely)

  ids <- get_ids(df = data, id_col = rlang::enexpr(airtable_id_col))

  safety_check(safely,
               cancel_message = "DELETE request cancelled.",
               paste0("You are about to delete ", length(ids), " Airtable records.\n\nDo you wish to proceed?")
               )

  id_batches <- lapply(split_list(ids, batch_size), function(x) paste0("records[]=", x, collapse = "&"))

  vdelete(ids = id_batches, airtable_obj = airtable)

  cli::cli_inform("Deleted {length(ids)} record{s}.")

  invisible(ids)

}


