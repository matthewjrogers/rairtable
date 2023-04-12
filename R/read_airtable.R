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
#' @return A data.frame containing the data read from the specified 'Airtable'
#'   table
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble column_to_rownames

read_airtable <- function(airtable,
                          fields = NULL,
                          id_to_col = TRUE,
                          max_rows = NULL,
                          ...,
                          call = caller_env()) {
  check_airtable_obj(airtable)
  check_logical(id_to_col)
  max_rows <- max_rows %||% 50000
  check_number_whole(max_rows, max = 50000)

  dta <- get_airtable_data(airtable = airtable, fields = fields, max_rows = max_rows, ...)

  if ("data.frame" %in% unlist(lapply(dta[[1]], class))) {
    cli::cli_bullets(
      c(
        "!" = "This data may contain 'user' field types.",
        "i" = "This type is currently unsupported in `insert_records` and `update_records`",
        call = call
      )
    )
  }

  # collapse list to dataframe
  table_data <- dplyr::bind_rows(dta)

  if (!id_to_col) {
    # set ids to rownames if not instructed to do otherwise
    table_data <- tibble::column_to_rownames(table_data, "airtable_record_id")
  }

  table_data
}

#' @noRd
get_airtable_data <- function(airtable,
                              fields = NULL,
                              id_col = getOption("rairtable.id_col", "airtable_record_id"),
                              token = NULL,
                              max_rows = 50000,
                              ...) {
  # pre-allocate space for data
  dta <- vector(ceiling(max_rows / 100), mode = "list")

  offset <- NULL

  view <- attr(airtable, "view")
  if (is_empty(view) || view == "") {
    view <- NULL
  }

  url <- attr(airtable, "request_url")

  .req <- req_airtable_query(
    url = url,
    fields = fields,
    view = view,
    token = token,
    ...
  )

  for (idx in seq_along(dta)) {
    req <-
      httr2::req_url_query(
        .req = .req,
        offset = offset
      )

    resp <- httr2::req_perform(req)

    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # bind record IDs and fields into a dataframe
    dta[[idx]] <- cbind(
      rlang::set_names(list(body[["records"]][["id"]]), id_col),
      body[["records"]][["fields"]]
    )

    if (is_null(body[["offset"]])) {
      # end loop if no offset returned
      break
    }

    offset <- body[["offset"]]
  }

  dplyr::bind_rows(dta)
}
