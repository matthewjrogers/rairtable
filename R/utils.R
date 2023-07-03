.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("field_types"),
    package = pkg,
    envir = parent.env(environment())
  )
}

# ---
# repo: r-lib/rlang
# file: standalone-purrr.R
# last-updated: 2023-02-23
# license: https://unlicense.org
# imports: rlang
# ---
#' map and keep (from standalone-purrr.R)
#'
#' @noRd
map <- function(.x, .f, ...) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  lapply(.x, .f, ...)
}

#' @noRd
map_lgl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}

#' @noRd
.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = global_env())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

#' @noRd
keep <- function(.x, .f, ...) {
  .x[.rlang_purrr_probe(.x, .f, ...)]
}

#' @noRd
.rlang_purrr_probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = global_env())
    map_lgl(.x, .p, ...)
  }
}

#' Wrapper for cli::cli_progress_along
#'
#' @param action String with the action to display in the progress message.
#' @noRd
#' @importFrom cli symbol pb_bar pb_percent cli_progress_along
#' @importFrom rlang with_options
map_along <- function(x,
                      .f,
                      ...,
                      action = NULL,
                      format = NULL) {
  format <- format %||%
    "{cli::symbol$arrow_right} {action}: {cli::pb_bar} | {cli::pb_percent}"

  rlang::with_options(
    list("cli.progress_show_after" = 0.75),
    map(
      cli::cli_progress_along(x, format = format),
      .f = .f,
      ...
    )
  )
}

#' Is x a list of lists?
#'
#' @noRd
is_list_of_lists <- function(x) {
  if (!is_list(x)) {
    return(FALSE)
  }

  all(vapply(x, is_list, TRUE))
}

#' Split list or vector into equal size pieces
#'
#' @noRd
split_list <- function(x,
                       batch_size = NULL,
                       arg = caller_arg(x),
                       call = caller_env()) {
  check_required(x, arg = arg, call = call)

  batch_size <- batch_size %||%
    as.integer(getOption("rairtable.batch_size", 10))

  check_number_whole(batch_size, call = call)

  len <- length(x)

  mapply(
    FUN = function(a, b) {
      x[a:b]
    },
    seq.int(from = 1, to = len, by = batch_size),
    pmin(
      seq.int(from = 1, to = len, by = batch_size) + (batch_size - 1),
      len
    ),
    SIMPLIFY = FALSE
  )
}

#' Set list names optionally using an attribute from each item in the list
#'
#' @noRd
set_list_names <- function(x, nm = NULL, at = "name") {
  nm <- nm %||% names_at(x, at)
  set_names(x, nm)
}

#' @noRd
names_at <- function(x, at = "name") {
  vapply(x, function(x) {
    x[[at]]
  }, NA_character_)
}

#' Return data, response body, or combined body from list of response objects
#'
#' An internal helper function to optionally return data, response body as a
#' list, or combined list of httr2_res response objects.
#'
#' @param data Input data frame or list
#' @param return_data If `FALSE`, return JSON response from the Airtable web API
#'   as a list. If `TRUE` (default) and data is not `NULL`, return input data
#'   frame or list.
#' @inheritParams httr2::resp_body_json
#' @keywords internal
return_data_resp <- function(data = NULL, resp = NULL, return_data = TRUE) {
  if (return_data && !is_null(data)) {
    return(invisible(data))
  }

  if (!is_httr2_resp(resp)) {
    return(c(lapply(resp, httr2::resp_body_json)))
  }

  httr2::resp_body_json(resp)
}

#' Check if object is a list
#'
#' @noRd
#' @importFrom vctrs obj_check_list
check_list <- function(x,
                       allow_na = FALSE,
                       allow_empty = FALSE,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (allow_na && is.na(x)) {
    return(invisible(NULL))
  }

  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  if (is_empty(x)) {
    if (allow_empty) {
      return(invisible(NULL))
    }

    cli_abort(
      "{.arg {arg}} can't be empty.",
      call = call
    )
  }

  if (is_list(x)) {
    return(invisible(NULL))
  }

  vctrs::obj_check_list(
    x = x,
    arg = arg,
    call = call
  )
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

  cli_abort(
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
    cli_abort(
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
