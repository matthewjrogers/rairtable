#' Insert records into an Airtable table
#'
#' @param data A dataframe containing records to insert
#' @param airtable An airtable object
#' @param batch_size Number of records per request to insert. Maximum of 10
#'
#' @export
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr status_code
#' @importFrom dplyr `%>%`
#'

insert_records <- function(data, airtable, batch_size = 10){

  validate_airtable(airtable)
  stopifnot(is.data.frame(data))
  stopifnot(batch_size <= 10)

  # current_table <-  api_get(airtable, 15)

  # compare_names(data, current_table)

  batch_json_requests <- batch_encode_post(data, batch_size)
  # batch_json_requests <- split_rows(data, batch_size) %>%
  #   lapply(batch_encode_post)



  vpost(records = batch_json_requests, airtable_obj = airtable)

}
