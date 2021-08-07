#' Read records from an Airtable object
#'
#' @param airtable An airtable object
#' @param id_to_col If TRUE, store airtable ID as a column rather than as row names
#' @param base_max_rows Record cap for the base,
#'
#' @export
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom data.table rbindlist
#' @importFrom tibble column_to_rownames
#'

read_airtable <- function(airtable, id_to_col = FALSE, base_max_rows = 50000){

  validate_airtable(airtable)
  stopifnot(is.logical(id_to_col))

  dta <- vector(ceiling(base_max_rows/100), mode = 'list')

  query_body <- list()

  if (!is.null(attr(airtable, 'view'))){
    query_body['view'] <- attr(airtable, 'view')
  }

  for (idx in 1:length(dta)){
    response <- httr::GET(attr(airtable, "request_url"),
                          config = httr::add_headers(
                            Authorization = paste("Bearer", get_airtable_api_key())
                          ),
                          query = query_body
    )

    parsed_json_response <- jsonlite::fromJSON(httr::content(response, as = "text"))

    if (!is.null(parsed_json_response$error)){
      stop(paste0('Error in JSON Response: ', parsed_json_response$error, collapse = " "))
    }

    dta[[idx]] <- cbind(airtable_id = parsed_json_response$records$id, parsed_json_response$records$fields)

    if (is.null(parsed_json_response$offset)){
      break
    }

    query_body['offset'] <- parsed_json_response$offset

    Sys.sleep(.2)

  }

  table_data <- data.table::rbindlist(dta, use.names = TRUE)

  if(!id_to_col){
    table_data <- tibble::column_to_rownames(table_data, 'airtable_id')
  }

  return(table_data)

}
