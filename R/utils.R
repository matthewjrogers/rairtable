# ---
# repo: r-lib/rlang
# file: standalone-purrr.R
# last-updated: 2023-02-23
# license: https://unlicense.org
# imports: rlang
# ---
#' map (from standalone-purrr.R)
#'
#' @noRd
map <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  lapply(.x, .f, ...)
}

#' Split list or vector into equal size pieces
#'
#' @noRd
split_list <- function(x,
                       batch_size = NULL,
                       arg = caller_arg(x),
                       call = caller_env()) {
  check_required(x, arg = arg)

  batch_size <- batch_size %||%
    as.integer(getOption("rairtable.batch_size", 10))

  check_number_whole(batch_size, call = call)

  mapply(
    FUN = function(a, b) {
      x[a:b]
    },
    seq.int(from = 1, to = length(x), by = batch_size),
    pmin(
      seq.int(from = 1, to = length(x), by = batch_size) + (batch_size - 1),
      length(x)
    ),
    SIMPLIFY = FALSE
  )
}

#' Check if object is a list
#'
#' @noRd
check_list <- function(x,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!is_list(x)) {
    stop_input_type(
      x,
      what = "a list",
      allow_na = allow_na, allow_null = allow_null,
      arg = arg, call = call
    )
  }
}

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

  answer <- cli_ask(
    ...,
    prompt = paste0("?\u00a0", prompt, "\u00a0(Y/n)"),
    .envir = .envir
  )

  if (all(tolower(answer) %in% tolower(yes))) {
    return(invisible(NULL))
  }

  check_character(message, call = call)

  cli::cli_abort(
    message = message,
    .envir = .envir,
    call = call
  )
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

#' @noRd
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_rbind
list_rbind <- function(x, names_to = zap(), ptype = NULL) {
  vctrs::vec_rbind(
    !!!x,
    .names_to = names_to,
    .ptype = ptype,
    .error_call = current_env()
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
