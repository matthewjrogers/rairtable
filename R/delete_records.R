#' Delete airtable records
#'
#' Delete records in an Airtable table based on their Airtable record ID.
#'
#' @param data A data frame containing records to delete
#' @param airtable An airtable object
#' @param airtable_id_col Column containing Airtable record IDs. Not required if record IDs are stored in row names as returned from \code{read_airtable}.
#' @param safely If \code{TRUE}, ask for confirmation before executing DELETE request
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
#' @importFrom rlang enexpr
#'

delete_records <- function(data, airtable, airtable_id_col = NULL, safely = TRUE, batch_size = 10){

  validate_airtable(airtable)
  stopifnot(is.data.frame(data))
  stopifnot(batch_size <= 10)
  stopifnot(is.logical(safely))

  ids <- get_ids(df = data, id_col = rlang::enexpr(airtable_id_col))

  safety_check(safely,
               cancel_message = "DELETE request cancelled.",
               paste0("You are about to delete ", length(ids), " Airtable records.\n\nDo you wish to proceed?")
               )

  id_batches <- lapply(split_list(ids, batch_size), function(x) paste0("records[]=", x, collapse = "&"))

  vdelete(ids = id_batches, airtable_obj = airtable)


  cat(adorn_text(paste0("Deleted ", length(ids), " records.")))

  return(invisible(ids))

}


