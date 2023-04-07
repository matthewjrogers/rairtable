batch_encode_patch <- function(df, id_col = NULL, batch_size = 10, parallel = TRUE){

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

  batches <- mapply(function(x, y){ list(ids = x, records = y) },
                    split_list(ids, batch_size),
                    split_list(records, batch_size),
                    SIMPLIFY = FALSE
  )

  if (parallel){
    message("JSON encoding data for PATCH\n")

    cl <- snow::makeCluster(parallel::detectCores(), type = 'SOCK')

    # snow::clusterExport(cl, "encode_batch_patch")

    encoded_batches <- snow::parLapply(cl, x = batches, fun = function(x){ encode_batch_patch(x, prog_bar = NULL) })

    snow::stopCluster(cl)

    message(adorn_text("Data JSON Encoded. Beginning PATCH requests.\n"))

  } else {

    pb <- progress::progress_bar$new(total = length(batches),
                                     format = "  JSON Encoding Data for PATCH [:bar] :percent eta: :eta"
    )
    # pb$tick(0)

    encoded_batches <- vencode_batch_patch(batches, prog_bar = pb)

  }

  encoded_batches

}

encode_batch_patch <- function(record_batch, prog_bar = NULL){
  # browser()
  stopifnot(length(record_batch[['records']]) == length(record_batch[['ids']]))

  lol <- vector(mode = 'list', length = length(record_batch[['records']]))

  for (idx in 1:length(lol)){
    lol[[idx]] <- list(id = record_batch[['ids']][[idx]], fields = lapply(record_batch[['records']][[idx]], function(r) if (is.list(r) & length(r[[1]]) > 1) unlist(r) else r))
  }

  fields <- list(records = lol)

  jsonout <- jsonlite::toJSON(fields, 
                              auto_unbox = TRUE,
                              # pretty = TRUE,
                              na = "null")

  cln <- gsub("fields\\.\\d?\\d", "fields", jsonout)

  if(!is.null(prog_bar)){  invisible(prog_bar$tick()) }

  cln
}

vencode_batch_patch <- Vectorize(encode_batch_patch, vectorize.args = c('record_batch'))

patch <- function(records, airtable_obj, prog_bar){

  response <- httr::PATCH(attr(airtable_obj, 'request_url'),
                          config = httr::add_headers(
                            Authorization = paste("Bearer", get_airtable_pat_or_key()),
                            `Content-type` = "application/json"
                          ),
                          body = records
  )

  if (!httr::status_code(response) %in% c(200)){
    stop(paste0("Error in PATCH ", process_error(httr::status_code(response))), call. = FALSE)
  }

  Sys.sleep(.21)

  invisible(prog_bar$tick())

}

vpatch <- Vectorize(patch, vectorize.args = 'records')





