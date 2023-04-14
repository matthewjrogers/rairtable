#' Read table from Airtable
#'
#' Connect to and read values from an Airtable table.
#'
#' @param airtable An airtable object
#' @param fields An optional list of fields to select.
#' @param id_to_col If `TRUE`, store airtable ID as a column rather than as row
#'   names
#' @param max_rows Optional maximum number of rows to read
#' @param ... Additional parameters passed to [req_airtable_query()]
#' @inheritParams rlang::args_error_context
#'
#' @return A data.frame with the records from the specified Airtable base and
#'   table.
#'
#' @export
#'
#' @importFrom cli cli_bullets
#' @importFrom dplyr bind_rows
#' @importFrom tibble column_to_rownames

read_airtable <- function(airtable,
                          fields = NULL,
                          id_to_col = TRUE,
                          airtable_id_col = NULL,
                          max_rows = NULL,
                          ...,
                          call = caller_env()) {
  check_airtable_obj(airtable)
  check_logical(id_to_col)
  max_rows <- max_rows %||% 50000
  check_number_whole(max_rows, max = 50000)

  data <- get_airtable_data(
    airtable = airtable,
    fields = fields,
    airtable_id_col = airtable_id_col,
    max_rows = max_rows,
    ...
  )

  if ("data.frame" %in% unlist(lapply(data[[1]], class))) {
    cli::cli_bullets(
      c(
        "!" = "This data may contain {.val user} field types.",
        "i" = "This field type is not supported by
        {.fn insert_records} or {.fn update_records}.",
        call = call
      )
    )
  }

  # collapse list to data.frame
  table_data <- dplyr::bind_rows(data)

  if (!id_to_col) {
    airtable_id_col <- airtable_id_col %||%
      getOption("rairtable.id_col", "airtable_record_id")
    # set ids to rownames if not instructed to do otherwise
    table_data <-
      tibble::column_to_rownames(
        table_data,
        airtable_id_col
        )
  }

  table_data
}

#' @noRd
get_airtable_data <- function(airtable,
                              fields = NULL,
                              airtable_id_col = NULL,
                              token = NULL,
                              max_rows = 50000,
                              ...) {

  url <- attr(airtable, "request_url")
  view <- attr(airtable, "view")

  if (is_empty(view) || view == "") {
    view <- NULL
  }

  .req <- req_airtable_query(
    url = url,
    fields = fields,
    view = view,
    token = token,
    ...
  )

  # pre-allocate space for data
  data <- vector(ceiling(max_rows / 100), mode = "list")
  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")
  offset <- NULL

  for (idx in seq_along(data)) {
    req <-
      httr2::req_url_query(
        .req = .req,
        offset = offset
      )

    resp <- httr2::req_perform(req)

    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # bind record IDs and fields into a data.frame
    data[[idx]] <- cbind(
      rlang::set_names(list(body[["records"]][["id"]]), airtable_id_col),
      body[["records"]][["fields"]]
    )

    if (is_null(body[["offset"]])) {
      # end loop if no offset returned
      break
    }

    offset <- body[["offset"]]
  }

  dplyr::bind_rows(data)
}



#' @noRd
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

