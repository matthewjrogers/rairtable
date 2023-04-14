#' Test, check, and parse Airtable URLs and API URLs
#'
#' Utility functions for checking and validating Airtable URLs and API URLs:
#'
#' - [is_airtable_url()] returns `TRUE` if a url starts with the base_url value.
#' - [is_airtable_api_url()] returns `TRUE` if a url starts with the api_url value.
#' - [check_airtable_url()] errors if url does not start with the base_url value.
#' - [check_airtable_api_url()] errors if url is not a valid API url.
#' - [parse_airtable_url()] accepts a base or API url as an input and returns a
#' named list with the base, table, and view ID values.
#'
#' For more information on the expected URL format for different Airtable API
#' end points, see the web API documentation at
#' <https://airtable.com/developers/web/api/introduction>
#'
#' @name airtable_url
NULL

#' @rdname airtable_url
#' @name is_airtable_url
#' @param url Airtable API or view URL, typically in the form
#'   "https://api.airtable.com/v0/{baseId}/{tableIdOrName}" or
#'   "https://airtable.com/{baseID}/{tableIdOrName}/{viewId}"
#' @param base_url Base URL for an Airtable base or view. Defaults `NULL` and
#'   set to `getOption("rairtable.api_url", "https://airtable.com/")`.
#' @export
is_airtable_url <- function(url, base_url = NULL) {
  if (is_null(url)) {
    return(FALSE)
  }

  base_url <- base_url %||%
    getOption("rairtable.base_url", "https://airtable.com/")

  is_url(url) & grepl(glue("^{base_url}"), url)
}


#' @rdname airtable_url
#' @name is_airtable_api_url
#' @param api_url Expected base URL for an API url. Defaults to `NULL` and set
#'   to `getOption("rairtable.api_url", "https://api.airtable.com")`.
#' @export
is_airtable_api_url <- function(url,
                                api_url = NULL) {
  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com")
  is_url(url) & grepl(paste0("^", api_url), url)
}

#' @rdname airtable_url
#' @name check_airtable_url
#' @inheritParams rlang::args_error_context
#' @export
check_airtable_url <- function(url,
                               base_url = NULL,
                               call = caller_env()) {
  check_string(url, call = call)

  if (is_airtable_url(url, base_url)) {
    return(invisible(NULL))
  }

  base_url <- base_url %||%
    getOption("rairtable.base_url", "https://airtable.com")

  cli_abort(
    "{.arg url} must start with the {.arg base_url}: {.url {base_url}}",
    call = call
  )
}

#' @rdname airtable_url
#' @name check_airtable_api_url
#' @param require_base If `TRUE`, the string "tbl" must be included in the
#'   supplied url.
#' @param require_table If `TRUE`, the string "tbl" must be included in the
#'   supplied url. If a string with a table name is provided, the string must be
#'   part of the string supplied to url. If `FALSE` (default), there is no check
#'   for a table being present in the URL.
#' @param require_view If `TRUE`, the string "viw" must be included in the
#'   string supplied to url.
#' @export
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

  if (is_logical(require_table) && require_table && !grepl("tbl", url)) {
    missing <- c(missing, "a {.arg table} name starting with {.val tbl}")
  }

  if (is_character(require_table) && !grepl(require_table, url)) {
    check_string(require_table)
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

#' @rdname airtable_url
#' @name parse_airtable_url
#' @param table_name,view_name Pattern to use when parsing a table name or view
#'   name. Defaults to `NULL` which extracts strings starting with "tbl" for
#'   tables IDs and "viw" for view IDs.
#' @export
parse_airtable_url <- function(url,
                               base_url = NULL,
                               api_url = NULL,
                               table_name = NULL,
                               view_name = NULL,
                               call = caller_env()) {
  base_url <- base_url %||%
    getOption("rairtable.base_url", "https://airtable.com/")

  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com")

  table_name <- table_name %||% "tbl[[:alnum:]]+"
  view_name <- view_name %||% "viw[[:alnum:]]+"

  list(
    "base" = string_extract(
      url,
      glue(
        "((?<={base_url})app[[:alnum:]]+(?=/))|",
        "((?<={api_url})app[[:alnum:]]+(?=/))"
      )
    ),
    "table" = string_extract(
      url,
      glue("(?<=/){table_name}+(?=/)")
    ),
    "view" = string_extract(
      url,
      glue("(?<=/){view_name}(?=\\?|/)")
    )
  )
}

#' Does x match the pattern of a URL?
#'
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
