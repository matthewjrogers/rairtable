#' Read records from an Airtable object
#'
#' @param airtable An airtable object
#' @param id_to_col If TRUE, store airtable ID as a column rather than as row names
#' @param max_rows Optional maximum number of rows to read
#'
#' @export
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom httr stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom data.table rbindlist
#' @importFrom tibble column_to_rownames
#'

read_airtable <- function(airtable, id_to_col = FALSE, max_rows = 50000){

  validate_airtable(airtable)
  stopifnot(is.logical(id_to_col))
  stopifnot(max_rows <= 50000)

  # pre-allocate space for data
  dta <- vector(ceiling(max_rows/100), mode = 'list')

  # create empty query body
  query_body <- list()

  if (!is.null(attr(airtable, 'view'))){
    # add view to query if present
    query_body['view'] <- attr(airtable, 'view')
  }

  for (idx in 1:length(dta)){

    response <- httr::GET(attr(airtable, "request_url"),
                          config = httr::add_headers(
                            Authorization = paste("Bearer", get_airtable_api_key())
                          ),
                          query = query_body
    )

    # stop for errors
    httr::stop_for_status(response, "Fetch Airtable records")

    # parse response
    parsed_json_response <- jsonlite::fromJSON(httr::content(response, as = "text"))

    # bind record IDs and fields into a dataframe
    dta[[idx]] <- cbind(airtable_id = parsed_json_response$records$id, parsed_json_response$records$fields)

    if (is.null(parsed_json_response$offset)){
      # end loop if no offset returned
      break
    }

    query_body['offset'] <- parsed_json_response$offset

    # Sys.sleep(.2)

  }

  # collapse list to dataframe
  table_data <- data.table::rbindlist(dta, use.names = TRUE, fill = TRUE)

  if(!id_to_col){
    # set ids to rownames if not instructed to do otherwise
    table_data <- tibble::column_to_rownames(table_data, 'airtable_id')
  }

  return(table_data)

}
