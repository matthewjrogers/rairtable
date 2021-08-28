patch <- function(records, airtable_obj, pb){

  response <- httr::PATCH(attr(airtable_obj, 'request_url'),
                          config = httr::add_headers(
                            Authorization = paste("Bearer", get_airtable_api_key()),
                            `Content-type` = "application/json"
                          ),
                          body = records
  )

  if (!httr::status_code(response) %in% c(200)){
    stop(paste0("Error in POST. ", process_error(httr::status_code(response))), call. = FALSE)
  }

  Sys.sleep(.2)

  pb$tick()

}

vpatch <- Vectorize(patch, vectorize.args = 'records')

batch_encode_patch <- function(df, id_col = NULL, batch_size = 10, pb = NULL, parallel = TRUE){
  # get ids

  ids <- get_ids(df = df, id_col = id_col)

  # remove id col if present
  if (!is.null(id_col)){
    df <- select(df, -{{ id_col }})
  }


  # split data into single rows
  records <- df %>%
    dplyr::mutate(rowid = row_number()) %>%
    dplyr::group_by(.data$rowid) %>%
    dplyr::group_split(.keep = FALSE)


  # convert to list of lists
  records <- lapply(records, as.list)

  record_batches <- split_list(records, batch_size)
  id_batches     <- split_list(ids, batch_size)

  encode <- vencode_batch_patch(record_batches, id_batches, pb)

  encode
  # split <- lapply(split_rows(df, 1),
  #                 function(x) jsonlite::toJSON(list(fields = jsonlite::unbox(x)))
  # ) %>%
  #   unlist()
  #
  # names(split) <- NULL
  #
  # records <- sprintf('{"id":"%s",%s', ids, gsub("^\\{", "", split))
  #
  # encode <- sprintf('{"records":[%s]}', paste(records, collapse = ","))
  # encode

}

encode_batch_patch <- function(record_batch, id_batch, pb = NULL){

  stopifnot(length(record_batch) == length(id_batch))

  lol <- vector(mode = 'list', length = length(record_batch))

  for (idx in 1:length(lol)){
    lol[[idx]] <- list(id = id_batch[[idx]], fields = record_batch[[idx]])
  }

  fields <- list(records = lol)

  jsonout <- jsonlite::toJSON(fields, auto_unbox = TRUE,
                              pretty = TRUE,
                              na = "null")

  cln <- gsub("fields\\.\\d?\\d", "fields", jsonout)
  if(!is.null(pb)){  pb$tick() }

  cln
}

vencode_batch_patch <- Vectorize(encode_batch_patch, vectorize.args = c('record_batch', 'id_batch'))

