#' Insert records into an Airtable table
#'
#' @param data A dataframe containing records to insert
#' @param airtable An airtable object
#' @param parallel Use parallel processing for encoding large tables
#' @param batch_size Number of records per request to insert. Maximum of 10
#'
#' @export
#'
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr status_code
#' @importFrom dplyr `%>%`
#' @importFrom crayon green
#' @importFrom cli symbol
#' @importFrom progress progress_bar
#'

insert_records <- function(data, airtable, parallel = FALSE, batch_size = 10){

  validate_airtable(airtable)
  stopifnot(is.data.frame(data))
  stopifnot(batch_size <= 10)

  batch_json_requests <- batch_encode_post(data, batch_size = batch_size, parallel = parallel)


  pb <- progress::progress_bar$new(total = length(batch_json_requests),
                                   format = "  Creating records: [:bar] :percent eta: :eta"
                                   )

  vpost(records = batch_json_requests, airtable_obj = airtable, pb = pb)

}
