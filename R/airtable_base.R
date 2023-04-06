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