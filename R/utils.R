#' Airtable constructor
#'
#' @param table Table name
#' @param base base name
#' @param view view name
#' @param api_url api url
#' @param api_version api version
#'
#' @return Airtable object
#'
#' @importFrom utils URLencode
#'

new_airtable <- function(table = character(), base = character(), view = character(), api_url = character(), api_version = integer()) {

  stopifnot(is.character(table))
  stopifnot(is.character(base))
  stopifnot(is.character(view) | is.null(view))
  stopifnot(is.character(api_url))
  stopifnot(is.integer(api_version))
  stopifnot(length(api_version) == 1)

  atbl <- new.env()
  atbl$table <- table

  class(atbl) <- "airtable"

  attr(atbl, "base") <- base
  attr(atbl, "view") <- view
  attr(atbl, "request_url") <- paste(api_url, sprintf("v%s", api_version), base, utils::URLencode(table), sep = "/")

  validate_airtable(atbl)

  atbl

}

# validate a new airtable object

validate_airtable <- function(airtable_obj){

  table <- airtable_obj$table
  base <- attr(airtable_obj, "base")
  view <- attr(airtable_obj, "view")
  request_url <- attr(airtable_obj, "request_url")

  if (class(airtable_obj) != 'airtable'){
    stop("The provided airtable object is not of class `airtable`")
  }

  if (length(table) > 1){
    stop("You can only connect to one Airtable table at a time. `table` should be a single character value", call. = FALSE)
  }

  if (length(table) < 1){
    stop("You muist provide an Airtable table name. `table` should be a single character value", call. = FALSE)
  }

  if (length(base) > 1){
    stop("Tables appear in a single Airtable base. `base` should be a single character value", call. = FALSE)
  }

  if (length(base) < 1){
    stop("You muist provide an Airtable base name. `base` should be a single character value", call. = FALSE)
  }

  if (length(view) > 1){
    stop("You can only connect to one Airtable view at a time. `view` must be either NULL or a single character value", call. = FALSE)
  }

  if (length(view) < 1 & !is.null(view)){
    stop("`view` must be either NULL or a single character value", call. = FALSE)
  }

  if (grepl("\\s", request_url)){
    stop("`request_url` cannot contain any spaces", call. = FALSE)
  }

}

#' @keywords internal
#' @export
#'

print.airtable <- function(x, ...){

  cat("Table: ", x$table, "\n", sep = "")
  if (!is.null(attr(x, 'view'))){
    cat("   View: ", attr(x, 'view'), "\n", sep = "")
  }
  cat("   Base: ", attr(x, 'base'), "\n", sep = "")
}

#' @keywords internal
#' @export
#'

str.airtable = function(object, ...) {

  view <- ifelse(is.null(attr(object, "view")), "", paste0('."', attr(object, "view"), '"'))
  cat(" Airtable: ", object$table, view, " @ ", attr(object, "base"), "\n", sep = "")

}

# Retrieve API Key

get_airtable_api_key <- function(){

  key <- Sys.getenv("AIRTABLE_API_KEY")

  if (key == ""){
    stop("No Airtable API key set. Use `airtable_api_key()` to set your API key.")
  }

  key
}

# Split a dataframe by row

split_rows <- function(df, chunk_size){
  n_rows <- nrow(df)
  split_vec  <- rep(1:ceiling(n_rows / chunk_size), each = chunk_size)[1:n_rows]
  res <- split(df, split_vec)

  res
}

encode_batch_post <- function(list_of_lists, pb){

  lol <- vector(mode = 'list', length = length(list_of_lists))

  for (idx in 1:length(lol)){
    lol[[idx]] <- list(fields = list_of_lists[[idx]])
  }

  fields <- list(records = lol)

  jsonout <- jsonlite::toJSON(fields, auto_unbox = TRUE,
                              # pretty = TRUE,
                              na = "null")

  cln <- gsub("fields\\.\\d?\\d", "fields", jsonout)
  if(!is.null(pb)){  pb$tick() }

  cln
}

#' JSON encode a dataframe for POST
#'
#' @param df Dataframe
#' @param batch_size Size of json batches to create. Max 10.
#' @param parallel Use parallel processing for encoding large tables
#'
#' @return JSON object
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite unbox
#' @importFrom dplyr `%>%`
#' @importFrom dplyr group_by
#' @importFrom dplyr group_split
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom parallel detectCores
#' @importFrom snow makeCluster
#' @importFrom snow clusterApply
#' @importFrom snow stopCluster
#' @importFrom rlang .data
#' @importFrom progress progress_bar
#'

batch_encode_post <- function(df, batch_size = 10, parallel = TRUE){

  records <- df %>%
    dplyr::mutate(rowid = row_number()) %>%
    dplyr::group_by(.data$rowid) %>%
    dplyr::group_split(.keep = FALSE)

  records_lst <- lapply(records, as.list)

  batches <- split_list(records_lst, batch_size)


  if (parallel){
    cat("JSON encoding data for POST")

    cl <- snow::makeCluster(parallel::detectCores(), type = 'SOCK')
    snow::clusterExport(cl, "encode_batch_post")

    encoded_batches <- snow::parLapply(cl, x = batches, fun = function(x){ encode_batch_post(x, pb = NULL)})

    snow::stopCluster(cl)
    cat(adorn_text("Data JSON Encoded. Beginning POST requests."))

  } else {
    pb <- progress::progress_bar$new(total = length(batches),
                                     format = "  JSON Encoding Data for POST [:bar] :percent eta: :eta"
    )
    pb$tick(0)

    encoded_batches <- lapply(batches, function(x) encode_batch_post(x, pb))
  }

  encoded_batches

}


adorn_text <- function(text, mode = 'success'){
  md <- match.arg(mode, c('success', 'failure'))

  if (md == 'success'){
    res <- paste(crayon::green(cli::symbol$tick), text, sep = " ")
  } else {
    res <- paste(crayon::red(cli::symbol$cross), text, sep = " ")
  }

  res
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

batch_encode_patch <- function(df, id_col = NULL, batch_size = 10, pb = NULL){
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

#' Get ids from data frame
#'
#' @param df A dataframe
#' @param id_col Optional name of a column containing Airtable record IDs
#'
#' @return Vector of Airtable IDs
#'
#' @importFrom tibble has_rownames
#' @importFrom dplyr select
#' @importFrom dplyr pull
#'

get_ids <- function(df, id_col){
  if (is.null(id_col)){

    if (!tibble::has_rownames(df)){
      stop("Data must either have Airtable IDs in row names or a provided ID column", call. = FALSE)
    }

    ids <- row.names(df)

  } else {

    ids <- df %>% select(!!id_col) %>% pull()

  }

  ids

}

# Split list or vector into equal size pieces

split_list <- function(lst, chunk_size = 10){
  mapply(
    function(a, b) (lst[a:b]),
    seq.int(from = 1, to = length(lst), by = chunk_size),
    pmin(seq.int(from = 1, to = length(lst), by = chunk_size) + (chunk_size - 1), length(lst)),
    SIMPLIFY = FALSE
  )
}

stop_quietly <- function(...) {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  cat(paste(..., collapse = " "))
  stop()
}

process_error <- function(response_status){

  statuses <- data.frame(
    code = c(400,  401, 403,  404, 408, 413, 422, 503),
    description = c("The server could not understand the request due to invalid syntax",
                    "Unauthorized: Invalid authentication",
                    "Forbidden: Invalid authentication",
                    "Resource not found",
                    "Request timeout",
                    "Payload too large",
                    "Unprocessable entity. The request was well-formed but was unable to be followed due to semantic errors.\n\nEnsure that the column types in R are compatible with the column types of your Airtable table.",
                    "The server is currently unable to handle the request due to temporary overloading or maintenance of the server."

    )
  )

  if (response_status %in% statuses$code){
    return(sprintf("Error Code %s: %s", statuses[statuses$code == response_status, 'code'], statuses[statuses$code == response_status, 'description']))
  }

  sprintf("Error Code %s", response_status)
}

#' Send batch delete request
#'
#' @param ids Vector of IDs
#' @param airtable_obj Object of class `airtable`
#'
#' @importFrom httr DELETE
#' @importFrom httr add_headers
#'

delete <- function(ids, airtable_obj){
  response <- httr::DELETE(sprintf("%s?%s", attr(airtable_obj, 'request_url'), ids),
                           config = httr::add_headers(
                             Authorization = paste("Bearer", get_airtable_api_key())
                           )
  )

  if (!httr::status_code(response) %in% c(200)){
    stop(paste0("Error in DELETE ", process_error(httr::status_code(response))), call. = FALSE)
  }

  Sys.sleep(.2)
}

vdelete <- Vectorize(delete, vectorize.args = "ids")

#' Send batch post request
#'
#' @param records JSON records to post
#' @param airtable_obj An object of class `airtable`
#' @param pb A progress_bar object
#'

post <- function(records, airtable_obj, pb){

  response <- httr::POST(attr(airtable_obj, 'request_url'),
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

vpost <- Vectorize(post, vectorize.args = "records")


patch <- function(records, airtable_obj, pb){

  response <- httr::PATCH(attr(airtable_obj, 'request_url'),
                          config = httr::add_headers(
                            Authorization = paste("Bearer", get_airtable_api_key()),
                            `Content-type` = "application/json"
                          ),
                          body = request
  )

  if (!httr::status_code(response) %in% c(200)){
    stop(paste0("Error in POST. ", process_error(httr::status_code(response))), call. = FALSE)
  }

  Sys.sleep(.2)

  pb$tick()

}

vpatch <- Vectorize(patch, vectorize.args = 'records')
# Get a subset of records via the API

api_get <- function(airtable, max_records){
  response <- httr::GET(paste0(attr(airtable, "request_url"), "?"),
                        config = httr::add_headers(
                          Authorization = paste("Bearer", get_airtable_api_key())
                        ),
                        query = list(maxRecords = max_records)
  )

  parsed_json_response <- jsonlite::fromJSON(httr::content(response, as = "text"))

  if (!is.null(parsed_json_response$error)){
    stop(paste0('Error in JSON Response: ', parsed_json_response$error, collapse = " "), call. = FALSE)
  }

  dta <-  parsed_json_response$records$fields

  dta

}

# Compare the names of two data frames and stop execution if they do not match

compare_names <- function(df1, df2){

  names1 <- sort(names(df1))
  names2 <- sort(names(df2))

  same_length <- length(names1) == length(names2)

  if(!same_length){
    stop(
      sprintf(
        "The provided data and the data on the Airtable base have an inconsistent number of columns.\n\nThe provided data has %s columns, while the Airtable base has %s columns",
        length(names1),
        length(names2)
      )
    )
  }

  all_names_match <- all(names1 == names2)

  if (!all_names_match){

    names1_unique <- paste0(names1[!names1 %in% names2], collapse = ", ")
    names2_unique <- paste0(names2[!names2 %in% names1], collapse = ", ")

    stop(
      sprintf(
        "The provided data and the data on the Airtable base do not have the same variable names.\n\n
        Names in provided data not on Airtable: %s \n
        Names in Airtable not in provided data: %s
        ",
        names1_unique,
        names2_unique
      ))
  }

}

# If the user chooses to execute safely, confirm action before proceeding

safety_check <- function(safely, cancel_message,  ...){

  stopifnot(is.logical(safely))

  if (safely){

    ans <- menu(c(paste(crayon::green(cli::symbol$tick), 'Yes'),
                  paste(crayon::red(cli::symbol$cross), 'No')),
                title = paste0(..., collapse = ""))

    if (ans != 1){
      stop_quietly(crayon::red(cli::symbol$cross), cancel_message)
    }
  }

}
