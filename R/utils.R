

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

  if (!inherits(airtable_obj, 'airtable')){
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




adorn_text <- function(text, mode = 'success'){
  md <- match.arg(mode, c('success', 'failure'))

  if (md == 'success'){
    res <- paste(crayon::green(cli::symbol$tick), text, sep = " ")
  } else {
    res <- paste(crayon::red(cli::symbol$cross), text, sep = " ")
  }

  res
}


get_ids <- function(df, id_col){
  if (is.null(id_col)){

    if (!tibble::has_rownames(df)){
      stop("Data must either have Airtable IDs in row names or a provided ID column", call. = FALSE)
    }

    ids <- row.names(df)

  } else {

    ids <- df %>% select(!!id_col) %>% dplyr::pull()

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


# If the user chooses to execute safely, confirm action before proceeding

safety_check <- function(safely, cancel_message, ...){

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
