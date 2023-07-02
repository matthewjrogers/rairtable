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
#'   data frame. Defaults to `NULL` which is sets record ID column name to
#'   `getOption("rairtable.id_col", "airtable_record_id")`. For [list_records()]
#'   and [get_record()], airtable_id_col is not used if metadata is `NULL` or
#'   does not include "id". The record ID column is dropped and converted to
#'   rownames if id_to_col is `FALSE`.
#' @param model A table model from [get_table_model()]. Optionally used to
#'   validate fields and sort parameters.
#' @param max_rows Deprecated. Maximum number of rows to read.
#' @param ... For [read_airtable()], additional query parameters, such as url,
#'   can be passed to [req_airtable()]. For [list_records()] and [get_record()],
#'   additional parameters are passed to [request_airtable()] and can also
#'   include base and table.
#' @inheritParams rlang::args_error_context
#'
#' @return A data frame with the records from the Airtable base and table
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
                          model = NULL,
                          .name_repair = "unique",
                          token = NULL,
                          ...) {
  req <- req_airtable(
    .req = request_airtable(
      airtable = airtable,
      ...
    ),
    remove_view = FALSE,
    token = token
  )

  if (!is_null(fields)) {
    req <- req_records_fields(req, fields = fields, model = model)
  }

  data <-
    req_perform_offset(
      req = req,
      airtable_id_col = airtable_id_col,
      metadata = "id",
      .name_repair = .name_repair
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

  airtable_id_col <- airtable_id_col %||%
    getOption("rairtable.id_col", "airtable_record_id")

  # set ids to rownames if not instructed to do otherwise
  tibble::column_to_rownames(
    data,
    airtable_id_col
  )
}


#' @rdname read_airtable
#' @name list_records
#' @inheritParams request_airtable
#' @inheritParams req_list_records
#' @inheritParams req_perform_offset
#' @export
list_records <- function(airtable = NULL,
                         view = NULL,
                         airtable_id_col = NULL,
                         fields = NULL,
                         sort = NULL,
                         direction = "asc",
                         fields_by_id = FALSE,
                         cell_format = NULL,
                         tz = NULL,
                         locale = NULL,
                         max_records = NULL,
                         page_size = NULL,
                         metadata = c("id", "createdTime"),
                         offset = NULL,
                         model = NULL,
                         .name_repair = "unique",
                         token = NULL,
                         ...) {
  req <- req_list_records(
    airtable = airtable,
    view = view,
    ...,
    fields = fields,
    sort = sort,
    direction = direction,
    fields_by_id = fields_by_id,
    cell_format = cell_format,
    tz = tz,
    locale = locale,
    max_records = max_records,
    page_size = page_size,
    metadata = metadata,
    model = model,
    token = token
  )

  req_perform_offset(
    req = req,
    airtable_id_col = airtable_id_col,
    metadata = metadata,
    .name_repair = .name_repair,
    offset = offset
  )
}

#' @rdname read_airtable
#' @name get_record
#' @param record Record ID (a string starting with "rec"). Required for
#'   [get_record()].
#' @export
#' @importFrom httr2 req_perform
get_record <- function(airtable = NULL,
                       record,
                       airtable_id_col = NULL,
                       cell_format = NULL,
                       tz = NULL,
                       locale = NULL,
                       metadata = c("id", "createdTime"),
                       .name_repair = "unique",
                       token = NULL,
                       ...) {
  req <- request_airtable(
    airtable = airtable,
    ...
  )

  check_string(record)

  req <- req_airtable(
    .req = req,
    template = "/{record}",
    record = record,
    token = token
  )

  req <- req_record_cell_format(req, cell_format, tz, locale)

  resp <- httr2::req_perform(req)

  if (!is_null(metadata)) {
    metadata <- record_metadata_match(metadata, values = c("id", "createdTime"))
  }

  resp_body_records(
    resp,
    airtable_id_col = airtable_id_col,
    metadata = metadata,
    .name_repair = .name_repair
  )
}

#' Build a request for the Airtable list records API method
#'
#' @name req_list_records
#' @param view Airtable view ID or name, Default: `NULL`. If the supplied url or
#'   airtable object includes a view, the view provided to this parameter is
#'   ignored.
#' @param fields Fields to return from Airtable base, Default: `NULL`
#' @param sort Field to sort by, Default: `NULL`
#' @param direction A string ("asc" for ascending (default) or "desc" for
#'   descending) or character vector matching length of sort parameter. Ignored
#'   if sort is `NULL`.
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
#' @param metadata Record metadata columns to include with returned data frame.
#'   Options including "id", "createdTime", and "commentCount". Defaults to
#'   `c("id", "createdTime")`. If metadata is `NULL`, no additional fields are
#'   added to the returned data frame.
#' @keywords internal
#' @importFrom httr2 req_url_query
#' @importFrom rlang exec
req_list_records <- function(req = NULL,
                             airtable = NULL,
                             view = NULL,
                             fields = NULL,
                             sort = NULL,
                             direction = "asc",
                             filter = NULL,
                             fields_by_id = FALSE,
                             cell_format = NULL,
                             tz = NULL,
                             locale = NULL,
                             metadata = c("id", "createdTime"),
                             max_records = NULL,
                             page_size = NULL,
                             model = NULL,
                             token = NULL,
                             ...,
                             call = caller_env()) {
  req <- req %||% request_airtable(
    airtable = airtable,
    view = view,
    ...,
    call = call
  )

  if (!is_null(sort)) {
    req <- req_records_sort(
      req,
      sort = sort,
      direction = direction,
      call = call
    )
  }

  if (!is_null(fields)) {
    req <- req_records_fields(req, fields = fields, model = model, call = call)
  }

  if (!is_null(metadata)) {
    req <- req_record_metadata(req, metadata = metadata, call = call)
  }

  req <- req_record_cell_format(
    req,
    cell_format = cell_format,
    tz = tz,
    locale = locale,
    call = call
  )

  if (!is_null(filter)) {
    check_string(filter, allow_empty = FALSE, call = call)

    req <- httr2::req_url_query(
      req,
      filterByFormula = filter
    )

    if (nchar(req[["url"]]) > 16000) {
      # TODO: To avoid this limit, make a POST request to
      # /v0/{base}/{table}/listRecords while passing the parameters within the
      # body of the request instead of the query parameters
      cli_abort(
        "Request URLs can't be longer than 16,000 characters.",
        call = call
      )
    }
  }

  if (!fields_by_id) {
    fields_by_id <- NULL
  }

  req_airtable(
    .req = req,
    maxRecords = max_records,
    pageSize = page_size,
    returnFieldsByFieldId = fields_by_id,
    remove_view = FALSE,
    token = token,
    call = call
  )
}

#' Add fields to list records request with optional validation
#'
#' @noRd
req_records_fields <- function(req,
                               fields,
                               model = NULL,
                               call = caller_env()) {
  if (!is_null(model)) {
    fields <- field_name_match(
      fields,
      model = model,
      error_arg = "fields",
      error_call = call
    )
  }

  req <- req_remove_airtable_view(req, remove_view = TRUE)
  fields <- set_names(fields, "fields[]")
  rlang::exec(httr2::req_url_query, req, !!!fields)
}

#' Add sort to list records request with optional validation
#'
#' @noRd
#' @importFrom vctrs vec_recycle
req_records_sort <- function(req,
                             sort,
                             direction = "asc",
                             model = NULL,
                             call = caller_env()) {
  if (!is_null(model)) {
    sort <- field_name_match(
      sort,
      model = model,
      error_arg = "sort",
      error_call = call
    )
  }

  sort <- set_names(sort, glue("sort[{seq_along(sort) - 1}][field]"))

  req <- rlang::exec(httr2::req_url_query, req, !!!sort)

  direction <- arg_match(
    direction,
    c("asc", "desc"),
    multiple = TRUE,
    error_call = call
  )

  direction <- vctrs::vec_recycle(
    direction,
    size = length(sort),
    x_arg = "direction",
    call = call
  )

  direction <- set_names(
    direction,
    glue("sort[{seq_along(sort) - 1}][direction]")
  )

  rlang::exec(httr2::req_url_query, req, !!!direction)
}

#' Add commentCount to request if required by metadata values
#'
#' @noRd
req_record_metadata <- function(req,
                                metadata,
                                values = c("id", "createdTime", "commentCount"),
                                call = caller_env()) {
  metadata <- record_metadata_match(metadata, values = values, call = call)

  if ("commentCount" %in% metadata) {
    return(httr2::req_url_query(req, recordMetadata = "commentCount"))
  }

  req
}

#' Add a cell format parameter to a request
#'
#' @noRd
req_record_cell_format <- function(req,
                                   cell_format = NULL,
                                   tz = NULL,
                                   locale = NULL,
                                   call = caller_env()) {
  cell_format <- cell_format %||% "json"
  cell_format <- arg_match0(cell_format, c("json", "string"), error_call = call)

  if (cell_format == "string") {
    tz <- tz %||% Sys.timezone()
    locale <- locale %||% Sys.getlocale("LC_TIME")
  }

  httr2::req_url_query(
    req,
    cellFormat = cell_format,
    timeZone = tz,
    userLocale = locale
  )
}

#' Get the body from API requests with a series of offset values
#'
#' @param req A modified HTTP request from [request_airtable()] or a httr2
#'   function.
#' @param offset Offset value to passed to [httr2::req_url_query()] as part of
#'   the API call. See
#'   <https://airtable.com/developers/web/api/list-records#response-offset> for
#'   more information on how the offset value is used by the Airtable API.
#'   Primarily intended for developer use only.
#' @keywords internal
#' @importFrom httr2 req_url_query req_perform resp_body_json
#' @importFrom tibble as_tibble
#' @importFrom vctrs vec_init
req_perform_offset <- function(req,
                               offset = NULL,
                               type = "combine",
                               airtable_id_col = NULL,
                               metadata = c("id", "createdTime", "commentCount"),
                               max_rows = NULL,
                               .name_repair = "unique",
                               call = caller_env()) {
  max_rows <- max_rows %||% 50000
  check_number_whole(max_rows, max = 50000, call = call)
  if (!is_null(metadata)) {
    metadata <- record_metadata_match(metadata, call = call)
  }

  # pre-allocate space for data
  body_list <- vctrs::vec_init(list(), ceiling(max_rows / 100))

  # pre-allocate space for data
  for (i in seq_along(body_list)) {
    req <-
      httr2::req_url_query(
        .req = req,
        offset = offset
      )

    resp <- httr2::req_perform(req, error_call = call)

    body_list[[i]] <-
      resp_body_records(
        resp,
        type = type,
        airtable_id_col = airtable_id_col,
        metadata = metadata,
        .name_repair = .name_repair,
        call = call
      )

    offset <- httr2::resp_body_json(resp)[["offset"]]

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
#' @param type "combine" (default) combines the data frames with fields and
#'   records from the API response. Additional supported options are "records"
#'   and "fields".
#' @param airtable_id_col An alternate name to use for the id column of the
#'   response data frame.
#' @param metadata Columns to return when type is "records" or to combine
#'   with fields when type is "combine".
#' @param .name_repair One of "unique" (default), "universal", "check_unique",
#'   "unique_quiet", or "universal_quiet" passed to [vctrs::vec_cbind()]. See
#'   [vctrs:vec_as_names] for the meaning of these options.
#' @param ... Additional parameters passed to [vctrs::vec_cbind()]. Not used.
#' @keywords internal
#' @importFrom httr2 resp_body_json
#' @importFrom cli cli_warn
#' @importFrom tidyselect any_of
#' @importFrom vctrs vec_cbind
resp_body_records <- function(resp,
                              simplifyVector = TRUE,
                              type = "combine",
                              airtable_id_col = NULL,
                              metadata = c("id", "createdTime", "commentCount"),
                              .name_repair = "unique",
                              ...,
                              call = caller_env()) {
  body <- httr2::resp_body_json(resp, simplifyVector = simplifyVector)

  type <-
    arg_match0(type, c("records", "fields", "combine"), error_call = call)

  if (has_name(body, "records")) {
    # Used by read_airtable and list_records
    records <- body[["records"]]
    fields <- records[["fields"]]
  } else if (has_name(body, c("fields"))) {
    # Used by get_record
    fields <- body[["fields"]]

    if (is_empty(fields)) {
      fields <- data.frame()
      cli::cli_warn(
        "{.arg record} is empty."
      )
    }

    records <- data.frame(
      "id" = body[["id"]],
      "createdTime" = body[["createdTime"]]
    )
  }

  # If metadata is not NULL
  if (!is_null(metadata)) {
    records <- select_cols(.data = records, tidyselect::any_of(metadata))
    record_nm <- names(records)

    if ("id" %in% record_nm) {
      record_nm[record_nm == "id"] <- airtable_id_col %||%
        getOption("rairtable.id_col", "airtable_record_id")
    }
  } else if (type != "records") {
    type <- "fields"
    fields <- as.data.frame(fields)
  }

  switch(type,
    "records" = records,
    "fields" = fields,
    "combine" = vctrs::vec_cbind(
      set_names(records, record_nm),
      as.data.frame(fields),
      ...,
      .name_repair = .name_repair,
      .error_call = call
    )
  )
}

#' Match metadata argument
#'
#' @noRd
record_metadata_match <- function(metadata,
                                  values = c("id", "createdTime", "commentCount"),
                                  call = caller_env()) {
  arg_match(
    metadata,
    values = values,
    multiple = TRUE,
    error_call = call
  )
}
