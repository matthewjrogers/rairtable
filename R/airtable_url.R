#' Test and parse Airtable URLs
#'
#' Utility functions primarily intended for use by developers for checking and
#' validating Airtable URLs, [is_airtable_url()] returns `TRUE` if a url starts
#' with the base_url value. [parse_airtable_url()] accepts a base or API url as
#' an input and returns a named list with the base, table, view, and field ID
#' values found in the URL.
#'
#' For more information on the expected URL format for different Airtable API
#' end points, see the web API documentation at
#' <https://airtable.com/developers/web/api/introduction>
#'
#' @name airtable_url
#' @inheritParams request_airtable
#' @returns
#'  - [is_airtable_url()] returns `TRUE` if the input matching an Airtable URL or `FALSE` otherwise.
#'  - [parse_airtable_url()] returns a list of values named "base", "table", "view", or "field".
NULL

#' @rdname airtable_url
#' @name is_airtable_url
#' @param url Airtable URL or API URL, typically in the form
#'   "https://airtable.com/{baseID}/{tableIdOrName}/{viewId}" or
#'   "https://api.airtable.com/v0/{baseId}/{tableIdOrName}". Note that, by
#'   default, shared URLs of the form "https://airtable.com/shr..." are not
#'   supported.
#' @param base_url Base URL for an Airtable base or view. Defaults `NULL` and
#'   set to `getOption("rairtable.base_url", "https://airtable.com")`.
#' @param allow_shared If `FALSE` (default), [is_airtable_url()] returns `FALSE`
#'   for any shared base or view URL that starts with
#'   "https://airtable.com/shr".
#' @export
is_airtable_url <- function(url, base_url = NULL, allow_shared = FALSE) {
  if (!all(is_character(url))) {
    url <- as.character(url)
  }

  base_url <- base_url %||%
    getOption("rairtable.base_url", "https://airtable.com")

  if (!allow_shared) {
    is_url(url) & grepl(paste0("^", base_url, "(?!/shr)"), url, perl = TRUE)
  } else {
    is_url(url) & grepl(paste0("^", base_url), url)
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

#' Does url match the pattern of an Airtable API url?
#'
#' [is_airtable_api_url()] returns `TRUE` if a url starts with the api_url
#' value.
#'
#' @inheritParams is_airtable_url
#' @param api_url Expected base URL for an API url. Defaults to `NULL` and set
#'   to `getOption("rairtable.api_url", "https://api.airtable.com/v0")`.
#' @keywords internal
is_airtable_api_url <- function(url,
                                api_url = NULL) {
  if (!all(is_character(url))) {
    url <- as.character(url)
  }

  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com/v0")

  is_url(url) & grepl(paste0("^", api_url), url)
}

#' @rdname airtable_url
#' @name parse_airtable_url
#' @inheritParams is_airtable_api_url
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
                               table_name = NULL,
                               view_name = NULL,
                               require_table = FALSE,
                               require_view = FALSE,
                               require_field = FALSE,
                               call = caller_env()) {
  check_url(url, call = call)

  ids <-
    vctrs::list_drop_empty(
      list(
        "base" = parse_url_base_id(url, base_url, api_url, call),
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
                              call = caller_env()) {
  # api_url <- api_url %||%
  #   getOption("rairtable.api_url", "https://api.airtable.com/v0")
  base_name <- "app[[:alnum:]]+"

  if (is_airtable_api_url(url, api_url)) {
    # FIXME: api_url is not required if base ID matches pattern
    base_pattern <- glue(
      "((?<=/){base_name}(?=/))|((?<=/){base_name}$)|((?<=/){base_name}(?=\\?))"
    )
  } else {
    base_url <- base_url %||%
      getOption("rairtable.base_url", "https://airtable.com")

    base_pattern <- glue("(?<={base_url}/){base_name}(?=/)")

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

#' Parse record ID from URL
#'
#' @noRd
parse_url_record_id <- function(url) {
  record_name <- "rec[[:alnum:]]+"

  record_pattern <-
    glue(
      "((?<=/){record_name}(?=/|\\?))|((?<=/){record_name}$)|((?<=view\\=){record_name})"
    )

  string_extract(url, record_pattern)
}

#' Parse workspace ID from URL
#'
#' @noRd
parse_url_workspace_id <- function(url) {
  workspace_name <- "wsp[[:alnum:]]+"

  workspace_pattern <-
    glue(
      "((?<=/){workspace_name}(?=/|\\?))|((?<=/){workspace_name}$)"
    )

  string_extract(url, workspace_pattern)
}

#' Extract pattern from a length 1 string
#'
#' @param string Passed to x parameter of [regmatches()]
#' @inheritParams base::regexpr
#' @noRd
string_extract <- function(string, pattern, perl = TRUE) {
  if (length(string) > 1) {
    match <- vapply(
      string,
      string_extract,
      FUN.VALUE = NA_character_,
      pattern = pattern,
      perl = perl
    )

    return(match)
  }

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
