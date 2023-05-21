#' Set or get an Airtable personal access token
#'
#' Set an Airtable personal access token as an environment variable, and
#' optionally install the personal access token to your .Renviron file for
#' future use.
#'
#' @param pat A valid Airtable personal access token. Setup a token at
#'   <https://airtable.com/create/tokens>
#' @inheritParams set_airtable_token
#' @return Invisibly return personal access token value
#'
#' @seealso [set_airtable_api_key()]
#' @examples
#' \dontrun{
#' set_airtable_pat("<personal access token>", install = TRUE)
#'
#' get_airtable_pat()
#' }
#' @export
set_airtable_pat <- function(pat,
                             install = FALSE,
                             overwrite = FALSE,
                             default = "AIRTABLE_PAT",
                             call = caller_env()) {
  set_airtable_token(pat, install, overwrite, default, call)
}

#' @rdname set_airtable_pat
#' @name get_airtable_pat
#' @export
get_airtable_pat <- function(pat = NULL,
                             default = "AIRTABLE_PAT",
                             call = caller_env()) {
  get_airtable_token(
    pat,
    message = c("Personal access token can't be found at {.envvar {default}}.",
      "*" = "Use {.fn set_airtable_pat} to set {.envvar {default}}."
    ),
    default = default, call = call
  )
}

#' Get an Airtable Personal Access Token or an API Key (if PAT is not available)
#'
#' @keywords internal
#' @export
get_airtable_pat_or_key <- function(token = NULL,
                                    default = c("AIRTABLE_PAT", "AIRTABLE_API_KEY"),
                                    call = caller_env()) {
  try_fetch(
    get_airtable_pat(token, call = call, default = default[[1]]),
    error = function(cnd) {
      get_airtable_api_key(token, call = call, default = default[[2]])
    }
  )
}

#' Set or get an Airtable Personal Access Token or API Key
#'
#' @param token Airtable personal access token or API key.
#' @param install Add your personal access token to your `.Renviron` for future
#'   sessions.
#' @param default Default name used for environmental variable where the
#'   personal access token is stored.
#' @param overwrite If `TRUE`, overwrite any existing Airtable personal access
#'   token.
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @export
#' @importFrom rlang caller_call call_name
#' @importFrom cli cli_bullets cli_alert_success
#' @importFrom utils read.table write.table
set_airtable_token <- function(token,
                               install = FALSE,
                               overwrite = FALSE,
                               default = "AIRTABLE_PAT",
                               call = caller_env()) {
  check_bool(install, call = call)

  if (!install) {
    caller_name <- "set_airtable_token"
    caller <- caller_call()
    if (!is.null(caller)) {
      caller_name <- call_name(caller)
    }

    cli::cli_bullets(
      c(
        "v" = "{.envvar {default}} set to {.val {token}} with {.fn Sys.setenv}.",
        "*" = "To use this token in future sessions, run
        {.fn {caller_name}} using {.arg install = TRUE}."
      )
    )
    Sys.setenv(default = token)
    return(invisible(token))
  }

  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (file.exists(renv)) {
    has_default <- any(grepl(default, readLines(renv)))

    if (has_default && !overwrite) {
      cli_abort(
        c("{.envvar {default}} already exists in your {.file .Renviron}.",
          "*" = "Set {.arg overwrite = TRUE} to replace this token."
        ),
        call = call
      )
    }
    backup <- file.path(home, ".Renviron_backup")
    file.copy(renv, backup)
    cli::cli_alert_success("{.file .Renviron} backed up to {.path {backup}}.")

    if (has_default) {
      oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
      newenv <- oldenv[!grepl(default, readLines(renv)), ]
      utils::write.table(newenv, renv,
        quote = FALSE,
        sep = "\n", col.names = FALSE, row.names = FALSE
      )
    }
  } else {
    file.create(renv)
  }

  write(glue('{default}="{token}"'), renv, sep = "\n", append = TRUE)

  cli::cli_bullets(
    c(
      "v" = "{.val {token}} saved to {.file .Renviron} variable {.envvar {default}}.",
      "*" = "Restart R or run {.code readRenviron(\"~/.Renviron\")} then use
      {.code Sys.getenv(\"{default}\")} to access the token."
    )
  )

  invisible(token)
}

#' @rdname set_airtable_token
#' @name get_airtable_token
#' @keywords internal
#' @export
get_airtable_token <- function(token = NULL,
                               default = "AIRTABLE_PAT",
                               call = caller_env(),
                               ...) {
  check_string(default)
  token <- token %||% Sys.getenv(default)

  if (!is.null(token) && token != "") {
    return(token)
  }

  cli_abort(
    ...,
    call = call
  )
}
