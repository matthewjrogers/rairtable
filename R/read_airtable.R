#' Read records from an Airtable table
#'
#' Read records from a table in an Airtable base. [read_airtable()] supports
#' basic access to data based on an airtable object. [list_records()]
#' and [get_record()] allows you to pass url or base and table directly
#' and supports more options from the Airtable API.
#'
#' Find more information on the list records API method:
#' <https://airtable.com/developers/web/api/list-records> or get record API
#' method: <https://airtable.com/developers/web/api/get-record>
#'
#' @param airtable An `airtable` class object. Optional for [read_airtable()] if
#'   url is supplied. For [list_records()] and
#'   [get_record()], support the airtable, url, or a base *and* table
#'   parameter.
#' @param fields Character vector with field names or field IDs to return.
#'   Optional.
#' @param id_to_col If `TRUE` (default), the airtable record IDs will be added
#'   to the returned data frame as a new column. If `FALSE`, the airtable record
#'   IDs are used as row names.
#' @param airtable_id_col Airtable record ID column name assigned to returned
#'   data.frame. Ignored if id_to_col is `TRUE`. Defaults to `NULL` which is set
#'   to `getOption("rairtable.id_col", "airtable_record_id")`.
#' @param max_rows Optional maximum number of rows to read. Defaults to `NULL`
#' @param ... For [read_airtable()], additional query parameters can be passed
#'   to [req_query_airtable()]. For [list_records()] and
#'   [get_record()], additional parameters, such as url, can be passed
#'   to [airtable_request()].
#' @inheritParams rlang::args_error_context
#'
#' @return A data.frame with the records from the Airtable base and table
#'   provided by the airtable parameter.
#'
#' @export
#'
#' @importFrom cli cli_bullets
#' @importFrom tibble column_to_rownames

read_airtable <- function(airtable = NULL,
                          fields = NULL,
                          id_to_col = TRUE,
                          airtable_id_col = NULL,
                          max_rows = deprecated(),
                          token = NULL,
                          ...) {
  check_airtable_obj(airtable, allow_null = TRUE)

  .req <- req_query_airtable(
    airtable = airtable,
    ...,
    fields = fields,
    token = token
  )

  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")

  data <-
    req_perform_offset(
      req = .req,
      record_cols = "id",
      record_nm = airtable_id_col
    )

  if ("data.frame" %in% lapply(data, class)) {
    cli::cli_bullets(
      c(
        "!" = "This data may contain {.val user} field types.",
        "i" = "This field type is not supported by
        {.fn insert_records} or {.fn update_records}."
      )
    )
  }

  check_bool(id_to_col)

  if (id_to_col) {
    return(data)
  }

  # set ids to rownames if not instructed to do otherwise
  tibble::column_to_rownames(
    data,
    airtable_id_col
  )
}


#' @rdname read_airtable
#' @name list_records
#' @inheritParams airtable_request
#' @param view Airtable view ID or name, Default: `NULL`. If the supplied url or
#'   airtable object includes a view, the view provided to this parameter is
#'   ignored.
#' @param fields Fields to return from Airtable base, Default: `NULL`
#' @param sort Field to sort by, Default: `NULL`
#' @param desc If `TRUE`, sort in descending order, Default: `FALSE`
#' @param max_records Maximum number of records to return, Default: `NULL`. Must
#'   be 100 or less.
#' @param page_size Max records to return per page, Default: `NULL`
#' @param cell_format Cell format for "Link to another record" fields. Defaults
#'   to "json" which returns a unique record ID. A "string" cell_format returns
#'   the displayed character string.
#' @param tz,locale Time zone and locale, Defaults to `NULL`. If cell_format is
#'   "string", tz defaults to `Sys.timezone()` and locale defaults to
#'   `Sys.getlocale("LC_TIME")`.
#' @param fields_by_id If `TRUE`, return fields by id, Default: `FALSE`
#' @param offset Offset value. Primarily intended for internal use.
#' @export
#' @importFrom httr2 req_url_path_append req_url_query
list_records <- function(airtable = NULL,
                         view = NULL,
                         sort = NULL,
                         max_records = 100,
                         page_size = NULL,
                         tz = NULL,
                         locale = NULL,
                         fields_by_id = FALSE,
                         fields = NULL,
                         desc = FALSE,
                         cell_format = NULL,
                         token = NULL,
                         offset = NULL,
                         ...) {
  req <- req_airtable_list_records(
    airtable = airtable,
    view = view,
    ...,
    sort = sort,
    max_records = max_records,
    page_size = page_size,
    tz = tz,
    locale = locale,
    fields_by_id = fields_by_id,
    fields = fields,
    desc = desc,
    cell_format = cell_format,
    token = token
  )

  req_perform_offset(req = req, offset = offset)
}

#' Build a request for the Airtable list records API method
#'
#' @noRd
req_airtable_list_records <- function(req = NULL,
                                      airtable = NULL,
                                      view = NULL,
                                      sort = NULL,
                                      max_records = 100,
                                      page_size = NULL,
                                      tz = NULL,
                                      locale = NULL,
                                      fields_by_id = FALSE,
                                      fields = NULL,
                                      desc = FALSE,
                                      cell_format = NULL,
                                      token = NULL,
                                      ...,
                                      call = caller_env()) {
  req <- req %||% airtable_request(
    airtable = airtable,
    view = view,
    ...,
    call = call
  )

  if (!is.null(sort)) {
    sort <- glue("field: \"{sort}\"")
    if (desc) {
      sort <- glue("{sort}, direction: \"desc\"")
    }
    sort <- glue("[{{sort}}]")
  }

  cell_format <- cell_format %||% "json"
  cell_format <- arg_match0(cell_format, c("json", "string"), error_call = call)

  if (cell_format == "string") {
    tz <- tz %||% Sys.timezone()
    locale <- locale %||% Sys.getlocale("LC_TIME")
  }

  if (!fields_by_id) {
    fields_by_id <- NULL
  }

  req <- req_query_airtable(
    .req = req,
    sort = sort,
    cellFormat = cell_format,
    timeZone = tz,
    userLocale = locale,
    maxRecords = max_records,
    pageSize = page_size,
    returnFieldsByFieldId = fields_by_id,
    token = token,
    call = call
  )

  if (!is_null(fields)) {
    for (f in fields) {
      req <- httr2::req_url_query(req, field = glue("[{f}]"))
    }
  }

  req
}

#' @rdname read_airtable
#' @name get_record
#' @param record Record ID number. Required for [get_record()].
#' @export
#' @importFrom httr2 req_url_path_append req_perform
get_record <- function(airtable = NULL,
                       record,
                       airtable_id_col = NULL,
                       token = NULL,
                       ...) {
  req <- airtable_request(
    airtable = airtable,
    ...
  )

  check_string(record)

  req <- req_query_airtable(
    .req = req,
    template = "/{record}",
    record = record,
    token = token
  )

  resp <- httr2::req_perform(req)

  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")

  resp_body_records(
    resp,
    record_nm = c(airtable_id_col, "createdTime")
  )
}

#' Get the body from API requests with a series of offset values
#'
#' @param req A modified HTTP request from [airtable_request()] or a httr2
#'   function.
#' @param offset Offset value to passed to [httr2::req_url_query()] as part of
#'   the API call. See
#'   <https://airtable.com/developers/web/api/list-records#response-offset> for
#'   more information on how the offset value is used by the Airtable API.
#' @keywords internal
#' @importFrom httr2 req_url_query req_perform
#' @importFrom tibble as_tibble
req_perform_offset <- function(req,
                               offset = NULL,
                               type = "combine",
                               record_cols = c("id", "createdTime"),
                               record_nm = record_cols,
                               max_rows = NULL,
                               call = caller_env()) {
  max_rows <- max_rows %||% 50000
  check_number_whole(max_rows, max = 50000, call = call)

  # pre-allocate space for data
  body_list <- vector(ceiling(max_rows / 100), mode = "list")

  # pre-allocate space for data
  for (i in seq_along(body_list)) {
    req <-
      httr2::req_url_query(
        .req = req,
        offset = offset
      )

    resp <- httr2::req_perform(req)

    offset <- httr2::resp_body_json(resp)[["offset"]]

    body_list[[i]] <-
      resp_body_records(
        resp,
        type = type,
        record_cols = record_cols,
        record_nm = record_nm,
        call = call
      )

    if (is_null(offset)) {
      # End loop if no offset returned
      break
    }
  }

  tibble::as_tibble(
    list_rbind(
      body_list[lengths(body_list) != 0]
    )
  )
}

#' @rdname req_perform_offset
#' @name resp_body_records
#' @param type "combine" (default) combines the fields and records data.frames
#'   in the response. Additional supported options are "records" and "fields".
#' @param id_col An alternate name to use for the id column of the response
#'   data.frame.
#' @param record_cols Column names for columns to retain from records data.frame
#'   when type is "records" or "combine".
#' @param record_nm Names to use for additional columns indicated by
#'   record_cols. Defaults to `NULL`.
#' @keywords internal
#' @importFrom vctrs vec_cbind
resp_body_records <- function(resp,
                              simplifyVector = TRUE,
                              type = "combine",
                              record_cols = c("id", "createdTime"),
                              record_nm = NULL,
                              call = caller_env(),
                              ...) {
  body <- httr2::resp_body_json(resp, simplifyVector = simplifyVector)

  type <-
    arg_match0(type, c("records", "fields", "combine"), error_call = call)

  if (has_name(body, "records")) {
    # Used by read_airtable and list_records
    records <- body[["records"]]
    fields <- records[["fields"]]

    if (has_length(record_cols, 1)) {
      records <- records[record_cols]
    } else {
      records <- records[, record_cols]
    }
  } else if (has_name(body, c("fields"))) {
    # Used by get_record
    fields <- body[["fields"]]
    records <- data.frame(
      "id" = body[["id"]],
      "createdTime" = body[["createdTime"]]
    )

    if (is_empty(fields)) {
      fields <- NULL
      cli::cli_warn(
        "The supplied {.arg record} is empty.",
        call = call
      )
    }
  }

  record_nm <- record_nm %||% names(records)

  switch(type,
    "records" = records,
    "fields" = fields,
    "combine" = vctrs::vec_cbind(
      set_names(records, record_nm),
      as.data.frame(fields),
      ...,
      .error_call = call
    )
  )
}
