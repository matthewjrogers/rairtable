#' Use httr2 to create an Airtable API request
#'
#' [airtable_request()] creates an initial request object based on an airtable
#' object, API url, Airtable view URL, or base and table IDs.
#'
#' [req_auth_airtable()] set the rate limit, user agent, and authenticates the
#' Airtable API request with an API key or personal access token.
#'
#' [req_airtable_query()] builds an API request optionally using a template or
#' custom HTTP method (other than GET or POST). This function can create an
#' initial request using a URL or airtable object or use an existing request
#' object. The function also calls [req_auth_airtable()] before returning the
#' request. Most of the other data access functions in this package wrap
#' [req_auth_airtable()].
#'
#'
#' @param url A URL for an API call or an Airtable view. If url is provided and
#'   is a valid URL, [airtable_request() ] returns  `httr2::request(url)`. url
#'   is optional if airtable *or* base and table are supplied. If url is an
#'   Airtable view, any additional base or table values provided are ignored. If
#'   airtable is provided, any additional value provided to url is ignored. For
#'   the metadata API where only a base ID is required include `require_base =
#'   TRUE` to pass to `check_airtable_api_url()`.
#' @param api_url Airtable API URL, If `NULL` (default), the api_url is set to
#'   getOption("rairtable.api_url", "https://api.airtable.com").
#' @param api_version Airtable API version number, If `NULL` (default), the
#'   api_version is set to getOption("rairtable.api_version", 0)).
#' @param base Airtable base id starting with with "app".
#' @param table Airtable table id or name. If table is a table ID it is a string
#'   starting with "viw".
#' @inheritParams is_airtable_obj
#' @param ... For [airtable_request()], additional parameters passed to
#'   [check_airtable_api_url()]. For [req_airtable_query()], additional
#'   parameters passed to [httr2::req_template()] if template is not `NULL` or
#'   [httr2::req_url_query()] if template is `NULL`.
#' @inheritDotParams check_airtable_api_url
#' @keywords internal
#' @export
#' @importFrom cli cli_alert_warning
#' @importFrom httr2 request req_url_path_append
airtable_request <- function(url = NULL,
                             api_url = NULL,
                             api_version = NULL,
                             base = NULL,
                             table = NULL,
                             airtable = NULL,
                             ...,
                             call = caller_env()) {
  if (!is_null(airtable) && is_airtable_obj(airtable)) {
    if (!is_null(url)) {
      cli::cli_alert_warning(
        "{.arg url} is ignored when {.arg airtable} is supplied."
      )
    }

    url <- attr(airtable, "request_url")
  }

  if (is_airtable_api_url(url)) {
    check_airtable_api_url(url, ..., api_url = api_url, call = call)
    return(httr2::request(url))
  }

  if (is_airtable_url(url)) {
    ids <- parse_airtable_url(url, ...)
    if (!is_null(base) | !is_null(table)) {
      cli::cli_alert_warning(
        "Any supplied {.arg base} and {.arg table} values are ignored when
        {.arg url} is an Airtable view."
      )
    }

    base <- ids[["base"]]
    table <- ids[["table"]]
  }

  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com")
  check_string(api_url, call = call)

  req <- httr2::request(api_url)

  api_version <- api_version %||% getOption("rairtable.api_version", 0)
  check_number_whole(api_version, call = call)

  req <- httr2::req_url_path_append(req, paste0("v", api_version))

  if (!is_null(base)) {
    check_string(base, call = call)
    req <- httr2::req_url_path_append(req, base)
  }

  if (!is_null(table)) {
    check_string(table, call = call)
    req <- httr2::req_url_path_append(req, table)
  }

  req
}

#' @rdname airtable_request
#' @name req_airtable_query
#' @param template Template for query parameters passed to
#'   [httr2::req_template()], Default: `NULL`.
#' @inheritParams httr2::req_method
#' @inheritParams httr2::req_body_json
#' @export
#' @importFrom httr2 req_template req_url_query
req_airtable_query <- function(.req = NULL,
                               ...,
                               template = NULL,
                               method = NULL,
                               data = NULL,
                               url = NULL,
                               api_url = NULL,
                               api_version = NULL,
                               airtable = NULL,
                               token = NULL,
                               string = NULL) {
  .req <-
    .req %||% airtable_request(
      url = url,
      api_url = api_url,
      api_version = api_version,
      airtable = airtable
    )

  if (!is.null(template)) {
    .req <-
      httr2::req_template(
        .req,
        template = template,
        ...
      )
  } else {
    .req <-
      httr2::req_url_query(
        .req,
        ...
      )
  }

  if (!is_null(method)) {
    .req <- httr2::req_method(.req, method)
  }

  if (!is_null(data)) {
    .req <-
      httr2::req_body_json(
        .req,
        data = data
      )
  }

  req_auth_airtable(
    req = .req,
    token = token,
    string = string
  )
}


#' @rdname airtable_request
#' @name req_auth_airtable
#' @param token Airtable personal access token, Default: `NULL` (set to output
#'   from `get_airtable_pat_or_key()`)
#' @param default Default name for environmental variable for token.
#' @param string Passed to [httr2::req_user_agent()], Default: `NULL` (set to
#'   `getOption("rairtable.useragent", default = "rairtable
#'   (https://github.com/matthewjrogers/rairtable)")`)
#' @param rate Rate passed to [httr2::req_throttle()]. Defaults to `5 / 1`
#' @param realm Passed to [httr2::req_throttle()]. Defaults to `NULL`.
#' @export
#' @importFrom httr2 req_auth_bearer_token req_user_agent req_throttle
req_auth_airtable <- function(req,
                              token = NULL,
                              default = "AIRTABLE_PAT",
                              string = NULL,
                              rate = 5 / 1,
                              realm = NULL) {
  token <- token %||% get_airtable_pat(token, default = default)
  # FIXME: Sys.getenv() had some odd issues with the period in pat so escaping
  # the token then removing escaping characters may be necessary in some cases
  token <- sub("\\\\", "", token)

  req <-
    httr2::req_auth_bearer_token(
      req = req,
      token = token %||% get_airtable_pat(token, default = default)
    )

  req <-
    httr2::req_user_agent(
      req = req,
      string = string %||% getOption(
        "rairtable.useragent",
        default = "rairtable (https://github.com/matthewjrogers/rairtable)"
      )
    )

  httr2::req_throttle(
    req = req,
    rate = rate,
    realm = realm
  )
}
