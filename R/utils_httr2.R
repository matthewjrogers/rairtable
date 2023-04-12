#' Use httr2 to create and perform an Airtable API request
#'
#' [req_auth_airtable()] supports the API request by setting rate limit, user
#' agent, and authenticate Airtable request with key/token
#'
#' @param url If url is provided and is a URL, return `httr2::request(url)`.
#' @param api_url Airtable API URL, Default: `NULL` (set to
#'   getOption("rairtable.api_url", "https://api.airtable.com"))
#' @param api_version Airtable API version number, Default: `NULL`  (set to
#'   getOption("rairtable.api_version", 0))
#' @param base Base id
#' @param table Table id
#' @keywords internal
#' @export
#' @importFrom httr2 request req_url_path_append
airtable_request <- function(url = NULL,
                             api_url = NULL,
                             api_version = NULL,
                             base = NULL,
                             table = NULL,
                             airtable = NULL) {
  if (!is_null(airtable) && is_airtable_obj(airtable)) {
    url <- url %||% attr(airtable, "request_url")
  }

  if (is_url(url)) {
    check_airtable_url(url)
    return(httr2::request(url))
  }

  api_url <- api_url %||% getOption("rairtable.api_hostname", "https://api.airtable.com")
  check_string(api_url)

  api_version <- api_version %||% getOption("rairtable.api_version", 0)
  check_number_whole(api_version)

  req <-
    httr2::req_url_path_append(
      httr2::request(api_url),
      sprintf("v%s", api_version)
    )

  if (!is_null(base)) {
    check_string(base)
    req <- httr2::req_url_path_append(req, base)
  }

  if (!is_null(table)) {
    check_string(table)
    req <- httr2::req_url_path_append(req, table)
  }

  req
}

#' @rdname airtable_request
#' @name req_airtable_query
#' @param template Template for query parameters passed to
#'   [httr2::req_template()], Default: `NULL`.
#' @param ... Additional parameters passed to [httr2::req_template()] if
#'   template is not `NULL` or [httr2::req_url_query()] if template is `NULL`.
#' @export
#' @importFrom httr2 req_template req_url_query
req_airtable_query <- function(.req = NULL,
                               ...,
                               template = NULL,
                               method = NULL,
                               url = NULL,
                               api_url = NULL,
                               api_version = NULL,
                               airtable = NULL,
                               token = NULL,
                               string = NULL) {
  .req <- .req %||% airtable_request(
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

  if (!is.null(method)) {
    .req <- httr2::req_method(.req, method)
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

#' @rdname airtable_request
#' @name req_airtable_records
#' @param base Airtable base identifier. Required. If base is an Airtable URL,
#'   the base, table, and view identifiers are extracted from the URL and
#'   assigned to any `NULL` values for table and view. Required if req is
#'   `NULL`. Ignored if req is provided.
#' @param table Airtable table name or identifier. Required if req is `NULL`.
#'   Ignored if req is provided.
#' @param view Airtable view identifier, Default: `NULL`
#' @param record Airtable record identifier, Default: `NULL`
#' @param fields Fields to return from Airtable base, Default: `NULL`
#' @param filter Filter to apply to records, Note: This parameter is a
#'   placeholder and is not currently implemented. Default: `NULL`
#' @param sort Field to sort by, Default: `NULL`
#' @param desc If `TRUE`, sort in descending order, Default: `FALSE`
#' @param max_records Maximum number of records to return, Default: `NULL`. Must
#'   be 100 or less.
#' @param per_page Max records to return per page, Default: `NULL`
#' @param cell_format Cell format for "Link to another record" fields (either
#'   "json" (unique ID) or "string" (displayed character string)), Default:
#'   'json'
#' @param tz,locale Time zone and locale, Defaults: `NULL`
#' @param fields_by_id If `TRUE`, return fields by id, Default: `FALSE`
#' @export
#' @importFrom httr2 req_url_path_append req_url_query
req_airtable_records <- function(req = NULL,
                                 url = NULL,
                                 api_url = NULL,
                                 api_version = NULL,
                                 base = NULL,
                                 table = NULL,
                                 record = NULL,
                                 view = NULL,
                                 sort = NULL,
                                 max_records = 100,
                                 page_size = NULL,
                                 tz = NULL,
                                 locale = NULL,
                                 fields_by_id = FALSE,
                                 fields = NULL,
                                 filter = NULL,
                                 desc = FALSE,
                                 cell_format = NULL,
                                 token = NULL,
                                 default = "AIRTABLE_PAT") {
  req <- req %||% airtable_request(url, api_url, api_version, base, table)

  if (!is.null(record)) {
    req <- httr2::req_url_path_append(req, record)
    return(req_auth_airtable(req, token = token, default = default))
  }

  if (!is.null(sort)) {
    sort <- glue("field: \"{sort}\"")
    if (desc) {
      sort <- glue("{sort}, direction: \"desc\"")
    }
    sort <- glue("[{{sort}}]")
  }

  cell_format <- match.arg(cell_format, c("json", "string"))

  if (cell_format == "string") {
    tz <- Sys.timezone()
    locale <- Sys.getlocale("LC_TIME")
  }

  if (!fields_by_id) {
    fields_by_id <- NULL
  }

  req <- req_airtable_query(
    req,
    view = view,
    sort = sort,
    cellFormat = cell_format,
    timeZone = tz,
    userLocale = locale,
    maxRecords = max_records,
    pageSize = page_size,
    returnFieldsByFieldId = fields_by_id,
    token = token,
    default = default
  )

  if (!is.null(fields)) {
    for (field in fields) {
      req <- httr2::req_url_query(req, field = glue("[{field}]"))
    }
  }

  req
}
