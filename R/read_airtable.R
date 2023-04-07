#' Read table from Airtable
#'
#' Connect to and read values from an Airtable table.
#'
#' @param airtable An airtable object
#' @param fields An optional list of fields to select.
#' @param id_to_col If TRUE, store airtable ID as a column rather than as row names
#' @param max_rows Optional maximum number of rows to read
#' 
#' @return A dataframe containing the data read from the specified 'Airtable' table
#' 
#' @export
#'
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom httr stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tibble column_to_rownames
#'

read_airtable <- function(airtable, fields = NULL, id_to_col = TRUE, max_rows = 50000){

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
  
  url <- attr(airtable, "request_url")
  
  if (!is.null(fields)){
    url_params <- paste0("fields%5B%5D=", fields, collapse = "&")
    url <- paste(url, url_params, sep = "?")
  }
  
  for (idx in 1:length(dta)){

    response <- httr::GET(url,
                          config = httr::add_headers(
                            Authorization = paste("Bearer", get_airtable_pat_or_key())
                          ),
                          query = query_body
    )

    # stop for errors
    httr::stop_for_status(response, "Fetch Airtable records")

    # parse response
    parsed_json_response <- jsonlite::fromJSON(httr::content(response, as = "text"))

    # bind record IDs and fields into a dataframe
    dta[[idx]] <- cbind(airtable_record_id = parsed_json_response$records$id, parsed_json_response$records$fields)

    if (is.null(parsed_json_response$offset)){
      # end loop if no offset returned
      break
    }

    query_body['offset'] <- parsed_json_response$offset

    # Sys.sleep(.2)

  }

  
  if (any(unlist(lapply(dta[[1]], class)) == "data.frame")){
    warning("This data may contain 'user' field types. This type is currently unsupported in `insert_records` and `update_records`", call. = FALSE)
  }
  
  # collapse list to dataframe
  table_data <- dplyr::bind_rows(dta)

  if(!id_to_col){
    # set ids to rownames if not instructed to do otherwise
    table_data <- tibble::column_to_rownames(table_data, 'airtable_record_id')
  }

  return(table_data)

}
