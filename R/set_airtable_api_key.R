#' Set or get an Airtable API key
#'
#' Set Airtable API key as an environment variable, and optionally install the
#' API key to your `.Renviron` file for future use.
#'
#' @name set_airtable_api_key
#' @param key A valid Airtable API key
#' @param install Add your API key to .Renviron for future sessions.
#' @param overwrite If `TRUE`, overwrite any existing Airtable API key.
#' @param default Default name to use for environment variable.
#' @inheritParams rlang::args_error_context
#'
#' @return Invisibly return API key value
#'
#' @seealso [set_airtable_pat()]
#' @export
#'
#' @importFrom cli cli_warn
#' @examples
#' \dontrun{
#' set_airtable_api_key("XXXXXXXXXX", install = TRUE)
#' }
#'
set_airtable_api_key <- function(key,
                                 install = FALSE,
                                 overwrite = FALSE,
                                 default = "AIRTABLE_API_KEY",
                                 call = caller_env()) {
  cli_warn(
    c("Airtable API keys will no longer be supported starting in early 2024.",
      "i" = "Moving forward, using {.fn set_airtable_pat} with a personal
      access token is strongly recommended."
    ),
    .frequency = "once",
    .frequency_id = "api_deprecate",
    call = call
  )

  set_airtable_token(key, install, overwrite, default, call)
}

#' @rdname set_airtable_api_key
#' @name get_airtable_api_key
#' @export
get_airtable_api_key <- function(key = NULL,
                                 default = "AIRTABLE_API_KEY",
                                 call = caller_env()) {
  get_airtable_token(
    key,
    message = c("API key can't be found at {.envvar {default}}.",
      "*" = "Use {.fn set_airtable_api_key} to set {.envvar {default}}."
    ),
    default = default, call = call
  )
}
