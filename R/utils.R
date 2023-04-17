#' Check if user, confirm action before proceeding
#'
#' @param yes Character vector of acceptable "yes" response options.
#' @inheritParams cli_ask
#' @noRd
safety_check <- function(safely = NULL,
                         ...,
                         prompt = "Do you want to continue?",
                         yes = c("", "Y", "Yes", "Yup", "Yep", "Yeah"),
                         message = "Aborted. A yes is required to continue.",
                         .envir = parent.frame(),
                         call = rlang::caller_env()) {
  safely <- safely %||% getOption("rairtable.safely", TRUE)
  check_bool(safely, call = call)

  if (is_false(safely)) {
    return(invisible(NULL))
  }

  check_logical(safely)

  resp <- cli_ask(..., prompt = paste0("?\u00a0", prompt, "\u00a0(Y/n)"), .envir = .envir)

  if (!all(tolower(resp) %in% tolower(yes))) {
    cli::cli_abort(
      message = message,
      .envir = .envir,
      call = call
    )
  }
}

#' Adapted from cliExtras::cli_ask()
#'
#' @noRd
cli_ask <- function(prompt = "?",
                    ...,
                    .envir = rlang::caller_env(),
                    call = .envir) {
  if (!rlang::is_interactive()) {
    cli::cli_abort(
      "User interaction is required.",
      call = call
    )
  }

  if (!rlang::is_empty(rlang::list2(...))) {
    cli::cli_bullets(..., .envir = .envir)
  }
  readline(paste0(prompt, "\u00a0"))
}

#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_rbind
list_rbind <- function(x, names_to = rlang::zap(), ptype = NULL) {
  vctrs::vec_rbind(
    !!!x,
    .names_to = names_to,
    .ptype = ptype,
    .error_call = rlang::current_env()
  )
}

#' Extract pattern from a length 1 string
#'
#' @param string Passed to x parameter of [regmatches()]
#' @inheritParams base::regexpr
#' @noRd
string_extract <- function(string, pattern, perl = TRUE) {
  if (is.na(string)) {
    return(NA_character_)
  }

  match <-
    regmatches(
      x = string,
      m = regexpr(
        pattern = pattern,
        text = string,
        perl = perl
      )
    )

  if (is_empty(match)) {
    return(NULL)
  }

  match
}

#' Split a data.frame by row
#'
#' @noRd
split_rows <- function(df, chunk_size) {
  n_rows <- nrow(df)
  split_vec <- rep(1:ceiling(n_rows / chunk_size), each = chunk_size)[1:n_rows]
  res <- split(df, split_vec)

  res
}

#' Split list or vector into equal size pieces
#'
#' @noRd
split_list <- function(x, chunk_size = 10) {
  mapply(
    function(a, b) (x[a:b]),
    seq.int(from = 1, to = length(x), by = chunk_size),
    pmin(seq.int(from = 1, to = length(x), by = chunk_size) + (chunk_size - 1), length(x)),
    SIMPLIFY = FALSE
  )
}

#' @noRd
process_error <- function(response_status) {
  statuses <- data.frame(
    code = c(400, 401, 403, 404, 408, 413, 422, 503),
    description = c(
      "The server could not understand the request due to invalid syntax",
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
    return(sprintf("Error Code %s: %s", statuses[statuses$code == response_status, "code"], statuses[statuses$code == response_status, "description"]))
  }

  sprintf("Error Code %s", response_status)
}

#' @export
#' @importFrom utils str
str.airtable <- function(object, ...) {
  view <- ifelse(is.null(attr(object, "view")), "", paste0('."', attr(object, "view"), '"'))
  cat(" Airtable: ", object$table, view, " @ ", attr(object, "base"), "\n", sep = "")
  utils::str(object)
}
