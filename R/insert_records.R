#' Insert records into an Airtable table
#'
#' Insert rows into an Airtable table. Requires that data names and types exactly match column names and types in Airtable. Violating this assumption will return a 422 Unprocessable Entity error. Supports batch insert and parallel JSON encoding (recommended for large tables).
#'
#' @param data A dataframe containing records to insert
#' @param airtable An airtable object
#' @param parallel If  \code{TRUE}, use parallel processing for encoding large tables
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
#' @importFrom jsonlite toJSON
#'

insert_records <- function(data, airtable, parallel = FALSE, batch_size = 10){

  validate_airtable(airtable)
  stopifnot(is.data.frame(data))
  stopifnot(batch_size <= 10)

  batch_json_requests <- batch_encode_post(data, batch_size = batch_size, parallel = parallel)


  pb <- progress::progress_bar$new(total = length(batch_json_requests),
                                   format = "  Creating records: [:bar] :percent eta: :eta"
                                   )

  vpost(records = batch_json_requests, airtable_obj = airtable, prog_bar = pb)

  return(invisible(data))
}
