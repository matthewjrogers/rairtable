#' Test, check, and parse Airtable URLs and API URLs
#'
#' Utility functions for checking and validating Airtable URLs and API URLs.
#' These functions are primarily intended for use by developers. See details for
#' a description of each function.
#'
#' @details
#'
#' - [is_airtable_url()] returns `TRUE` if a url starts with the base_url value.
#' - [is_airtable_api_url()] returns `TRUE` if a url starts with the api_url value.
#' - [check_airtable_url()] errors if url does not start with the base_url value.
#' - [check_airtable_api_url()] errors if url is not a valid API url.
#' - [parse_airtable_url()] accepts a base or API url as an input and returns a
#' named list with the base, table, view, and field ID values found in the URL.
#'
#' For more information on the expected URL format for different Airtable API
#' end points, see the web API documentation at
#' <https://airtable.com/developers/web/api/introduction>
#'
#' @name airtable_url
#' @inheritParams request_airtable
NULL

#' @rdname airtable_url
#' @name is_airtable_url
#' @param url Airtable API or view URL, typically in the form
#'   "https://api.airtable.com/v0/{baseId}/{tableIdOrName}" or
#'   "https://airtable.com/{baseID}/{tableIdOrName}/{viewId}"
#' @param base_url Base URL for an Airtable base or view. Defaults `NULL` and
#'   set to `getOption("rairtable.base_url", "https://airtable.com")`.
#' @param allow_shared If `FALSE` (default), [is_airtable_url()] returns `FALSE`
#'   for any shared base or view URL that starts with
#'   "https://airtable.com/shr".
#' @export
is_airtable_url <- function(url, base_url = NULL, allow_shared = FALSE) {
  if (is_null(url)) {
    return(FALSE)
  }

  base_url <- base_url %||%
    getOption("rairtable.base_url", "https://airtable.com")

  if (!allow_shared) {
    is_url(url) & grepl(paste0("^", base_url, "(?!/shr)"), url, perl = TRUE)
  } else {
    is_url(url) & grepl(paste0("^", base_url), url)
  }
}

#' @rdname airtable_url
#' @name is_airtable_api_url
#' @param api_url Expected base URL for an API url. Defaults to `NULL` and set
#'   to `getOption("rairtable.api_url", "https://api.airtable.com")`.
#' @export
is_airtable_api_url <- function(url,
                                api_url = NULL) {
  if (is.null(url)) {
    return(FALSE)
  }

  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com")

  is_url(url) & grepl(paste0("^", api_url), url)
}

#' @rdname airtable_url
#' @name check_airtable_url
#' @param allow_null If `FALSE` (default) and url is `NULL`,
#'   [check_airtable_url()] errors. If `TRUE`, a `NULL` value for url is
#'   allowed.
#' @inheritParams rlang::args_error_context
#' @export
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

#' @rdname airtable_url
#' @name check_airtable_api_url
#' @param require_base If `TRUE` (default), the string "app" must be included in
#'   the supplied url.
#' @param require_table If `TRUE`, the string "tbl" must be included in the
#'   supplied url. If a string with a table name is provided, the string must be
#'   part of the string supplied to url. If `FALSE` (default), there is no check
#'   for a table being present in the URL.
#' @param require_view If `TRUE`, the string "viw" must be included in the
#'   string supplied to url. If `FALSE` (default), there is no check
#'   for a view being present in the URL.
#' @param require_view If `TRUE`, the string "viw" must be included in the
#'   string supplied to url. If `FALSE` (default), there is no check
#'   for a view being present in the URL.
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

  if (is_true(require_table) && !grepl("tbl", url)) {
    missing <- c(missing, "a {.arg table} name starting with {.val tbl}")
  }

  if (!is_logical(require_table) && !grepl(require_table, url)) {
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

#' @rdname airtable_url
#' @name parse_airtable_url
#' @param table_name,view_name Pattern to use when parsing a table name or view
#'   name. Defaults to `NULL` which extracts strings starting with "tbl" for
#'   tables IDs and "viw" for view IDs.
#' @param require_field If `TRUE`, the url supplied to [parse_airtable_url()]
#'   must include a string starting with "fld" to use as the field ID .
#' @export
#' @importFrom vctrs list_drop_empty
parse_airtable_url <- function(url,
                               base_url = NULL,
                               api_url = NULL,
                               api_version = NULL,
                               table_name = NULL,
                               view_name = NULL,
                               require_table = FALSE,
                               require_view = FALSE,
                               require_field = FALSE,
                               call = caller_env()) {
  ids <-
    vctrs::list_drop_empty(
      list(
        "base" = parse_url_base_id(url, base_url, api_url, api_version, call),
        "table" = parse_url_table_id(url, table_name, call),
        "view" = parse_url_view_id(url, view_name, call),
        "field" = parse_url_field_id(url)
      )
    )

  check_parsed_airtable_url(
    ids,
    url,
    require_table,
    require_view,
    require_field,
    call = call
  )

  ids
}

#' Parse base ID from URL and error if URL is not valid
#'
#' Parse url and checks the input url for parse_airtable_url.
#'
#' @noRd
parse_url_base_id <- function(url,
                              base_url = NULL,
                              api_url = NULL,
                              api_version = NULL,
                              call = caller_env()) {
  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com")

  if (is_airtable_api_url(url, api_url)) {
    api_version <- api_version %||%
      getOption("rairtable.api_version", "0")

    base_pattern <- glue(
      "(?<={api_url}/v{api_version}/)",
      "app[[:alnum:]]+(?=/)"
    )
  } else {
    base_url <- base_url %||%
      getOption("rairtable.base_url", "https://airtable.com")

    base_pattern <- glue("(?<={base_url}/)app[[:alnum:]]+(?=/)")

    check_airtable_url(url, base_url, call = call)
  }

  string_extract(url, base_pattern)
}

#' Parse table ID or name from URL
#'
#' @noRd
parse_url_table_id <- function(url, table_name = NULL, call = caller_env()) {
  table_name <- table_name %||% "tbl[[:alnum:]]+"
  check_string(table_name, call = call)
  table_pattern <-
    glue(
      "((?<=/){table_name}(?=/))|((?<=/){table_name}$)|((?<=/){table_name}(?=\\?))"
    )

  string_extract(url, table_pattern)
}

#' Parse view ID or name from URL
#'
#' @noRd
parse_url_view_id <- function(url, view_name = NULL, call = caller_env()) {
  view_name <- view_name %||% "viw[[:alnum:]]+"
  check_string(view_name, call = call)
  view_pattern <-
    glue(
      "((?<=/){view_name}(?=/|\\?))|((?<=/){view_name}$)|((?<=view\\=){view_name})"
    )

  string_extract(url, view_pattern)
}

#' Parse field ID from URL
#'
#' @noRd
parse_url_field_id <- function(url) {
  field_name <- "fld[[:alnum:]]+"
  field_pattern <-
    glue(
      "((?<=/){field_name}(?=/|\\?))|((?<=/){field_name}$)"
    )

  string_extract(url, field_pattern)
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
