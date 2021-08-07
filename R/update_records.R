#' Update airtable records
#'
#' @param data A dataframe containing the records and fields to update
#' @param airtable An airtable object
#' @param columns Columns in the data to update on Airtable. Defaults to `dplyr::everything()`
#' @param airtable_id_col Column containing Airtable record IDs. Not required if record IDs are stored in row names as returned from `read_airtable`
#' @param safely If `TRUE`, confirm number and names of columns to update and number of rows befor executing update.
#' @param batch_size Number of records to update per request. Maximum of 10
#'
#' @export
#'
#' @importFrom httr PATCH
#' @importFrom httr add_headers
#' @importFrom dplyr `%>%`
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom rlang enexpr
#'

update_records <- function(data, airtable, columns = dplyr::everything(), airtable_id_col = NULL, safely = TRUE, batch_size = 10){

  validate_airtable(airtable)
  stopifnot(is.data.frame(data))
  stopifnot(batch_size <= 10)
  stopifnot(is.logical(safely))

  if (!tibble::has_rownames(data) & is.null(rlang::enexpr(airtable_id_col))){
    stop("Data must either have Airtable IDs in row names or a provided ID column", .call = FALSE)
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


  batch_json_requests <- split_rows(update_data, batch_size) %>%
    lapply(function(x) batch_encode_patch(x, id_col = rlang::enexpr(airtable_id_col)))

  for (request in batch_json_requests){
    httr::PATCH(attr(airtable, 'request_url'),
                config = httr::add_headers(
                  Authorization = paste("Bearer", get_airtable_api_key()),
                  `Content-type` = "application/json"
                ),
                body = request
    )
  }

}
