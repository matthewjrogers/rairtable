#' Create a new airtable object
#'
#' @param table Table name
#' @param base Airtable base containing table
#' @param view Optional view of data to read
#' @param api_url API endpoint to connect to. Can be changed for integrations that require custom endpoint
#' @param api_version Version of API to use
#'
#' @return An airtable object
#' @export
#'
#' @examples
#' \dontrun{
#' table <- airtable("Table 1", "appXXXXXXXXXXXXX")
#' }

airtable <- function(table, base, view = NULL, api_url = 'https://api.airtable.com', api_version = 0){

  atbl <- new_airtable(table, base, view, api_url, as.integer(api_version))

  validate_airtable(atbl)

  atbl
}
