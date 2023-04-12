
delete <- function(ids, airtable_obj, call = caller_env()) {
  response <- httr::DELETE(sprintf("%s?%s", attr(airtable_obj, 'request_url'), ids),
                           config = httr::add_headers(
                             Authorization = paste("Bearer", get_airtable_pat_or_key())
                           )
  )

  if (!httr::status_code(response) %in% c(200)) {
    cli_abort(paste0("Error in DELETE ", process_error(httr::status_code(response))), call = call)
  }

  Sys.sleep(.2)
}

vdelete <- Vectorize(delete, vectorize.args = "ids")
