#' Use httr2 to create or modify an Airtable API request
#'
#' These internal functions are designed for developers interested in
#' interacting directly with the Airtable API with or without using the airtable
#' class implemented in this package.
#'
#' [request_airtable()] creates an initial request object based on an airtable
#' object, API url, Airtable view URL, or base and table IDs.
#'
#' [req_airtable()] builds an API request optionally using a template or custom
#' HTTP method (other than GET or POST). This function can create an initial
#' request using a URL or airtable object or use an existing request object. The
#' function also set the rate limit, user agent, and authenticate an Airtable
#' API request.
#'
#' @param airtable An airtable class object created with [airtable()]. airtable
#'   must include a request_url attribute. url is ignored if airtable is
#'   provided. Optional if url or base *and* table are supplied.
#' @param url A URL for an API call or an Airtable view. If url is provided and
#'   is a valid URL, [request_airtable()] returns  `httr2::request(url)`. url
#'   is optional if airtable *or* base and table are supplied. If url is an
#'   Airtable view, any additional base or table values provided are ignored. If
#'   airtable is provided, any supplied url is ignored.
#' @param base Airtable base id starting with with "app". Optional if url or
#'   airtable are supplied or if require_base is `FALSE`. base and table are
#'   both required if url and airtable are `NULL` and require_base is `TRUE`.
#' @param table Airtable table id or name. Table ID values are strings starting
#'   with "tbl". Optional if require_table is `FALSE`.
#' @param view Airtable view ID. View ID values starts with "viw". Optional if
#'   require_view is `FALSE`.
#' @param api_url Airtable API URL, If `NULL` (default), the api_url is set to
#'   `getOption("rairtable.api_url", "https://api.airtable.com/v0")`.
#' @param ... For [request_airtable()], additional parameters passed to
#'   [parse_airtable_url()] if an Airtable url is provided. For
#'   [req_airtable()], additional parameters passed to
#'   [httr2::req_template()] if template is not `NULL` or
#'   [httr2::req_url_query()] if template is `NULL`.
#' @inheritParams check_airtable_api_url
#' @inheritDotParams parse_airtable_url
#' @inheritParams rlang::args_error_context
#'
#' @returns [request_airtable()] returns an HTTP response: an S3 list with class
#'   httr2_request. [req_airtable()] returns a modified HTTP request.
#'
#' @keywords internal
#' @export
#' @importFrom cli cli_alert_warning
#' @importFrom httr2 request
request_airtable <- function(airtable = NULL,
                             url = NULL,
                             base = NULL,
                             table = NULL,
                             view = NULL,
                             api_url = NULL,
                             require_base = TRUE,
                             require_table = TRUE,
                             require_view = FALSE,
                             ...,
                             call = caller_env()) {
  check_request_airtable_args(
    airtable = airtable,
    url = url,
    base = base,
    table = table,
    require_base = require_base,
    require_table = require_table
  )

  alert_airtable_args(
    base = base,
    table = table,
    view = view,
    airtable = airtable,
    url = url
  )

  if (!is_null(airtable)) {
    check_airtable_obj(
      airtable,
      require_table = require_table,
      require_view = require_view,
      call = call
    )

    return(httr2::request(airtable[["request_url"]]))
  }

  if (!is_null(url)) {
    req <- request_airtable_url(
      url,
      ...,
      api_url = api_url,
      require_base = require_base,
      require_table = require_table,
      require_view = require_view,
      call = call
    )

    return(req)
  }

  request_airtable_api_url(
    base = base,
    table = table,
    view = view,
    api_url = api_url,
    require_table = require_table,
    require_view = require_view,
    call = call
  )
}

#' Build a request from an Airtable URL or error if a URL is not valid
#'
#' @noRd
#' @importFrom httr2 request
request_airtable_url <- function(url = NULL,
                                 base = NULL,
                                 table = NULL,
                                 view = NULL,
                                 api_url = NULL,
                                 require_base = TRUE,
                                 require_table = TRUE,
                                 require_view = FALSE,
                                 ...,
                                 call = caller_env()) {
  check_url(url, call = call)

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
    ids <- parse_airtable_url(
      url = url,
      ...,
      api_url = api_url,
      require_table = require_table,
      require_view = require_view,
      call = call
    )

    req <- request_airtable_api_url(
      base = ids[["base"]],
      table = ids[["table"]],
      view = ids[["view"]],
      api_url = api_url,
      require_table = require_table,
      require_view = require_view,
      call = call
    )

    return(req)
  }

  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com/v0")
  base_url <- getOption("rairtable.base_url", "https://airtable.com")

  cli_abort(
    "{.arg url} must be a valid url starting with
      {.url {base_url}} or {.url {api_url}}",
    call = call
  )
}

#' Build an Airtable API request when a base and table are supplied
#'
#' @keywords internal
#' @importFrom httr2 request req_url_path_append
request_airtable_api_url <- function(base = NULL,
                                     table = NULL,
                                     view = NULL,
                                     api_url = NULL,
                                     require_table = FALSE,
                                     require_view = FALSE,
                                     call = caller_env()) {
  api_url <- api_url %||%
    getOption("rairtable.api_url", "https://api.airtable.com/v0")

  check_string(api_url, allow_empty = FALSE, call = call)

  req <- httr2::request(api_url)

  if (!is_empty(base)) {
    check_string(base, allow_empty = FALSE, call = call)
    req <- httr2::req_url_path_append(req, base)
  }

  check_string(table, allow_empty = FALSE, allow_null = !require_table)

  if (!is_empty(table)) {
    check_string(table, allow_empty = FALSE, call = call)
    req <- httr2::req_url_path_append(req, table)
  }

  check_string(view, allow_empty = FALSE, allow_null = !require_view)

  req_airtable_view(req, view = view, call = call)
}

#' @rdname request_airtable
#' @name req_airtable
#' @param .req A request object created by [httr2::request()] or
#'   [request_airtable()]. If .req is provided, any supplied url, api_url, or
#'   airtable parameters are ignored.
#' @param template Template for query parameters passed to
#'   [httr2::req_template()], Default: `NULL`.
#' @param remove_view If `TRUE` (default), remove view from request (only using
#'   view if it is explicitly passed as a query or template parameter). If
#'   `FALSE`, the request url for the supplied .req or created .req object may
#'   contain a view query parameter parsed from the url or view supplied to
#'   [airtable()] when creating an airtable object.
#' @inheritParams httr2::req_method
#' @inheritParams httr2::req_body_json
#' @inheritParams req_auth_airtable
#' @keywords internal
#' @export
#' @importFrom httr2 req_template req_url_query
req_airtable <- function(.req = NULL,
                         airtable = NULL,
                         url = NULL,
                         api_url = NULL,
                         ...,
                         template = NULL,
                         method = NULL,
                         data = NULL,
                         token = NULL,
                         string = NULL,
                         allow_key = TRUE,
                         require_base = TRUE,
                         require_table = FALSE,
                         remove_view = TRUE,
                         call = caller_env()) {
  .req <- .req %||% request_airtable(
    airtable = airtable,
    url = url,
    api_url = api_url,
    require_base = require_base,
    require_table = require_table,
    call = call
  )

  if (!is_httr2_req(.req)) {
    cli_abort(
      "{.arg .req} must be a {.cls httr2_request} object.",
      call = call
    )
  }

  .req <- req_remove_airtable_view(.req, remove_view = remove_view)

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

#' Set the rate limit, user agent, and authenticate an Airtable API request
#'
#' Set the rate limit, user agent, and authenticates the Airtable API request
#' with an API key or personal access token.
#'
#' @name req_auth_airtable
#' @param req A request object created by [httr2::request()] or
#'   [request_airtable()]. Required.
#' @param token Airtable personal access token, Defaults to `NULL` which is set
#'   to environmental variable for "AIRTABLE_PAT" (from [get_airtable_pat()]) or
#'   "AIRTABLE_API_KEY" (from [get_airtable_api_key()]).
#' @param default Default name for environmental variable for token.
#' @param string Passed to [httr2::req_user_agent()], Default: `NULL` (set to
#'   `getOption("rairtable.useragent", default = "rairtable
#'   (https://github.com/matthewjrogers/rairtable)")`)
#' @param rate Rate passed to [httr2::req_throttle()]. Defaults to `5 / 1`.
#' @param realm Passed to [httr2::req_throttle()]. Defaults to `NULL`.
#' @param allow_key If `TRUE`, allow use of an Airtable API key or an Airtable
#'   personal access token (PAT) to authenticate the request. The metadata API
#'   does not support the API key so allow_key is set to `FALSE` for any
#'   functions that call that API.
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

  req <- httr2::req_auth_bearer_token(
    req = req,
    token = token
  )

  string <- string %||%
    getOption(
      "rairtable.useragent",
      default = "rairtable (https://github.com/matthewjrogers/rairtable)"
    )

  req <- httr2::req_user_agent(
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
  status <- httr2::resp_status(resp)
  error <- httr2::resp_body_json(resp)[["error"]]

  if (!is_null(status)) {
    err_url <- paste0(
      "https://airtable.com/developers/web/api/errors#anchor-", status
    )

    message <- cli::cli_fmt(
      cli::cli_bullets(c("i" = "More information: {.url {err_url}}"))
    )
  }

  if (has_name(error, "message")) {
    return(c("x" = error[["message"]], message))
  }

  if (has_name(error, "type")) {
    return(c("x" = error[["type"]], message))
  }

  c(as.character(error), message)
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

#' Remove a view query parameter from a request
#'
#' @noRd
req_remove_airtable_view <- function(req,
                                     remove_view = FALSE) {
  if (remove_view) {
    return(httr2::req_url_query(req, view = NULL))
  }

  req
}

#' Does x have the class httr2_request?
#'
#' @noRd
is_httr2_req <- function(x) {
  inherits(x, "httr2_request")
}

#' Does x have the class httr2_response?
#'
#' @noRd
is_httr2_resp <- function(x) {
  inherits(x, "httr2_response")
}

#' Check if request_airtable has minimum required arguments
#'
#' @noRd
check_request_airtable_args <- function(airtable = NULL,
                                        url = NULL,
                                        base = NULL,
                                        table = NULL,
                                        require_base = TRUE,
                                        require_table = TRUE,
                                        call = caller_env()) {
  if (is_empty(airtable) && is_empty(url) && require_base && is_empty(base)) {
    message <- "{.arg airtable}, {.arg url}, or {.arg base}"

    if (require_table && is_null(table)) {
      message <- "{.arg airtable}, {.arg url},
      or {.arg base} {.emph and} {.arg table}"
    }

    cli_abort(
      paste0(message, " must be supplied."),
      call = call
    )
  }
}

#' Alert if request_airtable has arguments that will be ignored
#'
#' @noRd
alert_airtable_args <- function(base = NULL,
                                table = NULL,
                                view = NULL,
                                airtable = NULL,
                                url = NULL) {
  what <- NULL

  if (!is_empty(c(base, table))) {
    what <- "{.arg base} and {.arg table} are ignored"
  } else if (!is_empty(view)) {
    what <- "{.arg view} is ignored"
  }

  if (!is_null(airtable) && !is_null(what)) {
    cli::cli_alert_warning(
      paste0(what, " when {.arg airtable} is supplied.")
    )
  } else if (!is_null(url) && !is_null(what)) {
    cli::cli_alert_warning(
      paste0(what, " when {.arg url} is supplied.")
    )
  }

  if (!is_null(airtable) && !is_null(url)) {
    cli::cli_alert_warning(
      "{.arg url} is ignored when {.arg airtable} is supplied."
    )
  }
}
