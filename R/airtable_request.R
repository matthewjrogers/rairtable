#' Use httr2 to create an Airtable API request
#'
#' These functions are designed primarily for developers interested in
#' interacting directly with the Airtable API without using the airtable class
#' implemented in this package.
#'
#' [airtable_request()] creates an initial request object based on an airtable
#' object, API url, Airtable view URL, or base and table IDs.
#'
#' [req_auth_airtable()] set the rate limit, user agent, and authenticates the
#' Airtable API request with an API key or personal access token.
#'
#' [req_query_airtable()] builds an API request optionally using a template or
#' custom HTTP method (other than GET or POST). This function can create an
#' initial request using a URL or airtable object or use an existing request
#' object. The function also calls [req_auth_airtable()] before returning the
#' request. Most of the other data access functions in this package wrap
#' [req_auth_airtable()].
#'
#' @param url A URL for an API call or an Airtable view. If url is provided and
#'   is a valid URL, [airtable_request()] returns  `httr2::request(url)`. url
#'   is optional if airtable *or* base and table are supplied. If url is an
#'   Airtable view, any additional base or table values provided are ignored. If
#'   airtable is provided, any additional value provided to url is ignored. For
#'   the metadata API (where only a base ID is required) include `require_base =
#'   TRUE` to pass to [check_airtable_api_url()] when creating a request.
#' @param base Airtable base id starting with with "app". Optional if url or
#'   airtable are supplied or if require_base is `FALSE`. base and table are
#'   both required if url and airtable are `NULL` and require_base is `TRUE`.
#' @param table Airtable table id or name. Table ID values are strings starting
#'   with "tbl". Optional if require_table is `FALSE`.
#' @param view Airtable view ID. View ID values starts with "viw". Optional if
#'   require_view is `FALSE`.
#' @param airtable An airtable class object created with [airtable()]. airtable
#'   must include a request_url attribute. url is ignored if airtable is
#'   provided.  Optional if url or base *and* table are supplied.
#' @param api_url Airtable API URL, If `NULL` (default), the api_url is set to
#'   `getOption("rairtable.api_url", "https://api.airtable.com")`.
#' @param api_version Airtable API version number, If `NULL` (default), the
#'   api_version is set to `getOption("rairtable.api_version", 0))`.
#' @param ... For [airtable_request()], additional parameters passed to
#'   [parse_airtable_url()] if an Airtable url is provided. For
#'   [req_query_airtable()], additional parameters passed to
#'   [httr2::req_template()] if template is not `NULL` or
#'   [httr2::req_url_query()] if template is `NULL`.
#' @inheritParams check_airtable_api_url
#' @inheritDotParams parse_airtable_url
#' @inheritParams rlang::args_error_context
#'
#' @returns [airtable_request()] returns an HTTP response: an S3 list with class
#'   httr2_request. Other functions return modified modified HTTP requests.
#'
#' @keywords internal
#' @importFrom cli cli_alert_warning
#' @importFrom httr2 request req_url_path_append
airtable_request <- function(url = NULL,
                             base = NULL,
                             table = NULL,
                             view = NULL,
                             airtable = NULL,
                             api_url = NULL,
                             api_version = NULL,
                             require_base = TRUE,
                             require_table = TRUE,
                             require_view = FALSE,
                             ...,
                             call = caller_env()) {
  if (!is_null(airtable)) {
    check_airtable_obj(
      airtable,
      require_table = require_table,
      require_view = require_view,
      call = call
    )

    if (!is_null(url)) {
      cli::cli_alert_warning(
        "{.arg url} is ignored when {.arg airtable} is supplied."
      )
    }

    return(httr2::request(airtable[["request_url"]]))
  }

  if (is_airtable_api_url(url)) {
    check_airtable_api_url(
      url,
      require_base = require_base,
      require_table = require_table,
      require_view = require_view,
      api_url = api_url,
      call = call
    )

    return(httr2::request(url))
  }

  if (is_airtable_url(url)) {
    if (!is_empty(c(base, table, view))) {
      cli::cli_alert_warning(
        "Any {.arg base}, {.arg table}, or {.arg view} values supplied are
        ignored when {.arg url} is an Airtable URL."
      )
    }

    ids <- parse_airtable_url(
      url,
      ...,
      api_url = api_url,
      require_table = require_table,
      require_view = require_view,
      call = call
    )

    base <- ids[["base"]]
    table <- ids[["table"]]
    view <- ids[["view"]]
    url <- NULL
  }

  if (!is_null(url)) {
    check_url(url, call = call)

    api_url <- api_url %||%
      getOption("rairtable.api_url", "https://api.airtable.com")
    base_url <-
      getOption("rairtable.base_url", "https://airtable.com")

    cli_abort(
      "{.arg url} must be a valid url starting with
      {.url {base_url}} or {.url {api_url}}",
      call = call
    )
  }

  airtable_api_url_request(
    base = base,
    table = table,
    view = view,
    api_url = api_url,
    api_version = api_version,
    call = call
  )
}

#' Build an Airtable API request when a base and table are supplied
#'
#' @noRd
airtable_api_url_request <- function(base = NULL,
                                     table = NULL,
                                     view = NULL,
                                     api_url = NULL,
                                     api_version = NULL,
                                     call = caller_env()) {
  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com")
  check_string(api_url, call = call)

  req <- httr2::request(api_url)

  api_version <- api_version %||%
    getOption("rairtable.api_version", 0)
  check_number_whole(api_version, call = call)

  req <- httr2::req_url_path_append(req, paste0("v", api_version))

  if (!is_empty(base)) {
    check_string(base, call = call)
    req <- httr2::req_url_path_append(req, base)
  }

  if (!is_empty(table)) {
    check_string(table, call = call)
    req <- httr2::req_url_path_append(req, table)
  }

  req_airtable_view(req, view = view, call = call)
}

#' @rdname airtable_request
#' @name req_query_airtable
#' @param .req,req A request object created by [httr2::request()] or
#'   [airtable_request()]. If .req is provided to [req_query_airtable()], url,
#'   api_url, api_version, and airtable parameters are ignored. req is required
#'   for [req_auth_airtable()].
#' @param template Template for query parameters passed to
#'   [httr2::req_template()], Default: `NULL`.
#' @inheritParams httr2::req_method
#' @inheritParams httr2::req_body_json
#' @param allow_key If `TRUE`, allow use of an Airtable API key or an Airtable
#'   personal access token (PAT) to authenticate the request. The metadata API
#'   does not support the API key so allow_key is set to `FALSE` for any
#'   functions that call that API.
#' @keywords internal
#' @importFrom httr2 req_template req_url_query
req_query_airtable <- function(.req = NULL,
                               ...,
                               template = NULL,
                               method = NULL,
                               data = NULL,
                               url = NULL,
                               api_url = NULL,
                               api_version = NULL,
                               airtable = NULL,
                               token = NULL,
                               string = NULL,
                               allow_key = TRUE,
                               call = caller_env()) {
  .req <-
    .req %||% airtable_request(
      url = url,
      api_url = api_url,
      api_version = api_version,
      airtable = airtable,
      call = call
    )

  if (!is_null(template)) {
    .req <- httr2::req_template(.req, template = template, ...)
  } else {
    .req <- httr2::req_url_query(.req, ...)
  }

  if (!is_null(method)) {
    .req <- httr2::req_method(.req, method)
  }

  if (!is_null(data)) {
    .req <- httr2::req_body_json(.req, data = data)
  }

  req_auth_airtable(
    req = .req,
    token = token,
    string = string,
    allow_key = allow_key
  )
}

#' Add view query to URL if view is not already part of the request URL
#'
#' @noRd
req_airtable_view <- function(req,
                              view = NULL,
                              allow_empty = TRUE,
                              allow_null = TRUE,
                              call = caller_env()) {
  if ((allow_empty && is_empty(view)) || grepl("?view=viw", req[["url"]])) {
    return(req)
  }

  check_string(view, allow_empty = FALSE, allow_null = allow_null, call = call)
  httr2::req_url_query(req, view = view)
}

#' @rdname airtable_request
#' @name req_auth_airtable
#' @param token Airtable personal access token, Defaults to `NULL` which is set
#'   to environmental variable for "AIRTABLE_PAT" (from [get_airtable_pat()]) or
#'   "AIRTABLE_API_KEY" (from [get_airtable_api_key()]).
#' @param default Default name for environmental variable for token.
#' @param string Passed to [httr2::req_user_agent()], Default: `NULL` (set to
#'   `getOption("rairtable.useragent", default = "rairtable
#'   (https://github.com/matthewjrogers/rairtable)")`)
#' @param rate Rate passed to [httr2::req_throttle()]. Defaults to `5 / 1`.
#' @param realm Passed to [httr2::req_throttle()]. Defaults to `NULL`.
#' @keywords internal
#' @importFrom httr2 req_auth_bearer_token req_user_agent req_throttle
req_auth_airtable <- function(req,
                              token = NULL,
                              default = "AIRTABLE_PAT",
                              string = NULL,
                              rate = 5 / 1,
                              realm = NULL,
                              allow_key = TRUE) {
  if (allow_key) {
    token <- token %||% get_airtable_pat_or_key(token)
  } else {
    token <- token %||% get_airtable_pat(token, default = default)
  }
  # FIXME: Sys.getenv() had some odd issues with the period in pat so escaping
  # the token then removing escaping characters may be necessary in some cases
  token <- sub("\\\\", "", token)

  req <-
    httr2::req_auth_bearer_token(
      req = req,
      token = token
    )

  string <-
    string %||%
    getOption(
      "rairtable.useragent",
      default = "rairtable (https://github.com/matthewjrogers/rairtable)"
    )

  req <-
    httr2::req_user_agent(
      req = req,
      string = string
    )

  req <- httr2::req_error(req, body = airtable_error_body)

  httr2::req_throttle(
    req = req,
    rate = rate,
    realm = realm
  )
}

#' Format Airtable API error messages
#'
#' @noRd
airtable_error_body <- function(resp) {
  error <- httr2::resp_body_json(resp)[["error"]]
  if (has_name(error, "message")) {
    c(
      error[["message"]],
      "More information: <https://airtable.com/developers/web/api/errors>"
    )
  } else if (has_name(error, "type")) {
    error[["type"]]
  } else {
    as.character(error)
  }
}
