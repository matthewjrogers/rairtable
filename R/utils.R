#' @noRd
is_url <- function(x) {
  if (is_null(x)) {
    return(FALSE)
  }

  grepl(
    "http[s]?://(?:[[:alnum:]]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

# Split a dataframe by row

split_rows <- function(df, chunk_size) {
  n_rows <- nrow(df)
  split_vec  <- rep(1:ceiling(n_rows / chunk_size), each = chunk_size)[1:n_rows]
  res <- split(df, split_vec)

  res
}

adorn_text <- function(text, mode = 'success') {
  md <- match.arg(mode, c('success', 'failure'))

  if (md == 'success') {
    res <- paste(crayon::green(cli::symbol$tick), text, sep = " ")
  } else {
    res <- paste(crayon::red(cli::symbol$cross), text, sep = " ")
  }

  res
}


get_ids <- function(df, id_col, call = caller_env()) {
  if (is.null(id_col)) {

    if (!tibble::has_rownames(df)) {
      cli_abort(
        "Data must either have Airtable IDs in row names or a provided ID column",
        call = call
        )
    }

    return(row.names(df))
  }

  df %>% select(!!id_col) %>% dplyr::pull()
}

# Split list or vector into equal size pieces

split_list <- function(lst, chunk_size = 10) {
  mapply(
    function(a, b) (lst[a:b]),
    seq.int(from = 1, to = length(lst), by = chunk_size),
    pmin(seq.int(from = 1, to = length(lst), by = chunk_size) + (chunk_size - 1), length(lst)),
    SIMPLIFY = FALSE
  )
}

stop_quietly <- function(..., call = caller_env()) {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  cli::cli_alert(paste(..., collapse = " "))
  abort(call = call)
}

process_error <- function(response_status) {

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

  if (response_status %in% statuses$code) {
    return(sprintf("Error Code %s: %s", statuses[statuses$code == response_status, 'code'], statuses[statuses$code == response_status, 'description']))
  }

  sprintf("Error Code %s", response_status)
}


# If the user chooses to execute safely, confirm action before proceeding

safety_check <- function(safely, cancel_message, ...) {

  check_logical(safely)

  if (safely) {

    ans <- menu(c(paste(crayon::green(cli::symbol$tick), 'Yes'),
                  paste(crayon::red(cli::symbol$cross), 'No')),
                title = paste0(..., collapse = ""))

    if (ans != 1) {
      stop_quietly(crayon::red(cli::symbol$cross), cancel_message)
    }
  }

}
