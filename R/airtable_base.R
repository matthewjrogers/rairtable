#' Title
#'
#' @param base_id A valid Airtable base ID
#' @param metadata_api_url_pattern URL for GET request. By default 'https://api.airtable.com/v0/meta/bases/%s/tables'
#'
#' @return an `airtable_base_schema` object
#' @export
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr stop_for_status
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' base <- airtable_base("appXXXXXXXXXXXXX")
#' 
#' # read 'table1' from the base
#' tbl_data <- read_airtable(base$table1)
#' 
#' # view base schema
#' print(base$schema)
#' }

airtable_base <- function(base_id, metadata_api_url_pattern = 'https://api.airtable.com/v0/meta/bases/%s/tables'){
  url <- sprintf(metadata_api_url_pattern, base_id)
  result <- httr::GET(url,
                      config = httr::add_headers(
                        Authorization = paste("Bearer", get_airtable_pat())
                      )
  )
  httr::stop_for_status(result, "retrieve base metadata")
  string_content <- rawToChar(result$content)
  
  schema <- jsonlite::fromJSON(string_content, simplifyDataFrame = FALSE)[[1]]
  class(schema) <- "airtable_base_schema"
  
  for (i in seq_along(schema)){
    class(schema[[i]]) <- "airtable_table_schema"
    class(schema[[i]]$fields) <- "airtable_fields_schema"
  }
  base <- new.env()
  base$schema <- schema
  for (table in schema){
    base[[table$name]] <- airtable(table = table$name, base = base_id, fields = table$fields)
  }
  
  return(base)
}