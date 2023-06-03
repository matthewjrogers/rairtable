#' Check is a URL is valid
#'
#' @noRd
check_url <- function(url,
                      allow_null = FALSE,
                      call = caller_env()) {
  check_string(url, allow_null = allow_null, call = call)

  if (is_url(url) || (allow_null && is_null(url))) {
    return(invisible(NULL))
  }

  cli_abort(
    "{.arg url} must be a valid url.",
    call = call
  )
}

#' Check Airtable URLs and API URLs
#'
#' Utility functions for checking and validating Airtable URLs and API URLs.
#'
#' - [check_airtable_url()] errors if url does not start with the base_url value.
#' - [check_airtable_api_url()] errors if url is not a valid API url.
#'
#' For more information on the expected URL format for different Airtable API
#' end points, see the web API documentation at
#' <https://airtable.com/developers/web/api/introduction>
#'
#' @name check_airtable_url
#' @inheritParams request_airtable
#' @param allow_null If `FALSE` (default) and url is `NULL`,
#'   [check_airtable_url()] errors. If `TRUE`, a `NULL` value for url is
#'   allowed.
#' @inheritParams rlang::args_error_context
#' @keywords internal
check_airtable_url <- function(url,
                               base_url = NULL,
                               allow_null = FALSE,
                               allow_shared = FALSE,
                               call = caller_env()) {
  check_url(url, allow_null = allow_null, call = call)

  if (is_airtable_url(url, base_url, allow_shared) || (allow_null && is_null(url))) {
    return(invisible(NULL))
  }

  message <- "{.arg url} must start with the {.arg base_url}: {.url {base_url}}"

  if (!is_null(url) && any(grepl("/shr", url))) {
    message <- "{.arg url} can't be an Airtable shared url."
  }

  base_url <- base_url %||%
    getOption("rairtable.base_url", "https://airtable.com")

  cli_abort(
    message,
    call = call
  )
}

#' @name check_airtable_api_url
#' @rdname check_airtable_url
#' @param require_base If `TRUE` (default), the string "app" must be included in
#'   the supplied API request URL.
#' @param require_table If `TRUE`, the string "tbl" must be included in the
#'   supplied url. If require_table is a string, it is treated as the required
#'   table name which must be part of url. If `FALSE` (default), there is no
#'   check for a table being present in the API request URL.
#' @param require_view If `TRUE`, the string "viw" must be included in the
#'   string supplied to url. If `FALSE` (default), there is no check
#'   for a view being present in the API request URL.
#' @param require_view If `TRUE`, the string "viw" must be included in the
#'   string supplied to url. If `FALSE` (default), there is no check
#'   for a view being present in the API request URL.
#' @keywords internal
check_airtable_api_url <- function(url,
                                   require_base = TRUE,
                                   require_table = FALSE,
                                   require_view = FALSE,
                                   api_url = NULL,
                                   call = caller_env()) {
  check_string(url, call = call)
  missing <- NULL

  if (require_base && !grepl("app", url)) {
    missing <- c(missing, "a {.arg base} name starting with {.val app}")
  }

  if (is_true(require_table) && !grepl("tbl", url)) {
    missing <- c(missing, "a {.arg table} name starting with {.val tbl}")
  }

  if (is_string(require_table) && !grepl(require_table, url)) {
    check_string(require_table, call = call)
    missing <- c(missing, "a {.arg table} named {.val {require_table}}")
  }

  if (require_view && !grepl("viw", url)) {
    missing <- c(missing, "a {.arg view} name starting with {.val viw}")
  }

  if (is_airtable_api_url(url, api_url) && is_null(missing)) {
    return(invisible(NULL))
  }

  message <- "{.arg url} is not a valid Airtable API url."

  if (!is.null(missing)) {
    missing <- paste0(
      "{.arg url} is missing ",
      oxford_comma(missing, final = "and"), "."
    )

    message <- c(
      message,
      "i" = missing
    )
  }

  cli::cli_abort(
    message = message,
    call = call
  )
}

#' Does the parsed values from a URL included required IDs?
#'
#' @noRd
check_parsed_airtable_url <- function(ids,
                                      url,
                                      require_table = FALSE,
                                      require_view = FALSE,
                                      require_field = FALSE,
                                      call = caller_env()) {
  if (is_empty(ids[["base"]])) {
    cli_abort(
      c("{.arg url} is not valid.",
        "i" = "{.arg base} can't be found in {.url {url}}."
      ),
      call = call
    )
  }

  if (require_table && is_empty(ids[["table"]])) {
    cli_abort(
      c("{.arg url} is not valid.",
        "i" = "{.arg require_table} is `TRUE` and
        {.arg table} can't be found in {.url {url}}."
      ),
      call = call
    )
  }

  if (require_view && is_empty(ids[["view"]])) {
    cli_abort(
      c("{.arg url} is not valid.",
        "i" = "{.arg require_view} is `TRUE` and
        {.arg view} can't be found in {.url {url}}."
      ),
      call = call
    )
  }

  if (require_field && is_empty(ids[["field"]])) {
    cli_abort(
      c("{.arg url} is not valid.",
        "i" = "{.arg require_field} is `TRUE` and
        {.arg field} or {.arg column} can't be found in {.url {url}}."
      ),
      call = call
    )
  }
}
