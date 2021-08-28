#' Update airtable records
#'
#' @param data A dataframe containing the records and fields to update
#' @param airtable An airtable object
#' @param columns Columns in the data to update on Airtable. Defaults to `dplyr::everything()`
#' @param airtable_id_col Column containing Airtable record IDs. Not required if record IDs are stored in row names as returned from `read_airtable`
#' @param safely If `TRUE`, confirm number and names of columns to update and number of rows befor executing update.
#' @param parallel If `TRUE` use parallel processing for encoding large tables
#' @param batch_size Number of records to update per request. Maximum of 10
#'
#' @export
#'
#' @importFrom httr PATCH
#' @importFrom httr add_headers
#' @importFrom dplyr `%>%`
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr row_number
#' @importFrom rlang enexpr
#' @importFrom rlang .data
#' @importFrom dplyr pull
#'

update_records <- function(data, airtable, columns = dplyr::everything(), airtable_id_col = NULL, safely = TRUE, parallel = FALSE, batch_size = 10){

  validate_airtable(airtable)
  stopifnot(is.data.frame(data))
  stopifnot(batch_size <= 10)
  stopifnot(is.logical(safely))
  stopifnot(is.logical(parallel))

  if (!tibble::has_rownames(data) & is.null(rlang::enexpr(airtable_id_col))){
    stop("Data must either have Airtable IDs in row names or a provided ID column", call. = FALSE)
  }

  if (is.null(rlang::enexpr(airtable_id_col))){
    update_data <- dplyr::select(data, {{ columns }})
  } else {
    update_data <- dplyr::select(data, c({{ airtable_id_col }}, {{ columns }}))
  }

  safety_check(safely,
               cancel_message = "PATCH request cancelled.",
               paste0("You are about to update ", nrow(update_data), " records of the following variables:\n    ", paste0(names(update_data), collapse = ", "), "\nDo you wish to proceed?")
               )


  batch_json_requests <- batch_encode_patch(data, id_col = rlang::enexpr(airtable_id_col), batch_size = batch_size, parallel = parallel)



  pb <- progress::progress_bar$new(total = length(batch_json_requests),
                                   format = "  Sending PATCH requests [:bar] :percent eta: :eta"
  )

  vpatch(batch_json_requests, airtable, pb)

}
