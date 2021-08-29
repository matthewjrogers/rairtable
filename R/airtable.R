#' Create a new airtable object
#'
#' Creates an S3 airtable object, which serves as a pointer for rairtable functions
#'
#' @param table Table name in Airtable
#' @param base Airtable base containing table. A base functions like a schema in a traditional database. You can retrieve the base ID from the API documentation.
#' @param view Optional view of data to read
#' @param api_url API endpoint to connect to. Can be changed for API integrations that require custom endpoint
#' @param api_version Version of API to use. Defaults to 0 (the current version as of Fall 2021)
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
