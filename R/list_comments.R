#' List comments, create a comment, or delete a comment on an Airtable record
#'
#' List all comments for an Airtable record, create a new comment, or delete an
#' existing comment on an Airtable record.
#'
#' @inheritParams request_airtable
#' @param record Record ID (a string starting with "rec") or an Airtable record
#'   URL. If record is a URL, no additional parameters are required. Otherwise,
#'   airtable, url, or base and table must be supplied.
#' @param ... Additional parameters including url or base and table values. url
#'   or base and table must be supplied if airtable is `NULL` and record is a
#'   record ID.
#' @returns
#' - [list_comments()] returns a data frame with record comment IDs, text,
#' and author information.
#' - [create_comment()] returns a data frame with the created comment ID, text,
#' and author information.
#' - [delete_comment()] returns a list with a comment ID..
#' @export
list_comments <- function(airtable = NULL,
                          record = NULL,
                          ...) {
  req <-
    req_comments(
      airtable = airtable,
      record = record,
      ...,
      template = "{base}/{table}/{record}/comments"
    )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)
  comments <- body[["comments"]]
  offset <- body[["offset"]]

  # pre-allocate space for data
  while (!is_null(body[["offset"]])) {
    req <-
      httr2::req_url_query(
        .req = req,
        offset = offset
      )

    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp)
    comments <- c(comments, body[["comments"]])
    offset <- body[["offset"]]
  }

  list_rbind(comments)
}

#' @rdname list_comments
#' @name create_comment
#' @param text String with text of comment to create. Required for
#'   [create_comment()].
#' @export
create_comment <- function(airtable = NULL,
                           record = NULL,
                           text,
                           ...) {
  check_string(text)

  req <- req_comments(
    airtable = airtable,
    record = record,
    ...,
    template = "{base}/{table}/{record}/comments",
    data = list(
      "text" = text
    )
  )

  resp <- httr2::req_perform(req)

  invisible(list_rbind(httr2::resp_body_json(resp)))
}

#' @rdname list_comments
#' @name delete_comment
#' @param comment Comment ID (string starting with the text "com"). Required for
#'   [delete_comment()].
#' @export
delete_comment <- function(airtable = NULL,
                           record = NULL,
                           comment,
                           ...) {
  check_required(comment)

  req <- req_comments(
    airtable = airtable,
    record = record,
    ...,
    comment = comment,
    require_comment = TRUE,
    method = "DELETE",
    template = "{base}/{table}/{record}/comments/{comment}"
  )

  resp <- httr2::req_perform(req)

  invisible(httr2::resp_body_json(resp))
}


#' Create a request for interacting with the comments API endpoint
#'
#' @noRd
req_comments <- function(airtable = NULL,
                         record = NULL,
                         comment = NULL,
                         ...,
                         template = NULL,
                         method = NULL,
                         data = NULL,
                         require_record = TRUE,
                         require_comment = FALSE,
                         call = caller_env()) {

  if (is_null(airtable) && is_url(record)) {
    airtable <- record
  }

  base <- get_base_id(
    airtable = airtable,
    ...,
    call = call
  )

  table <- get_table_id(
    airtable = airtable,
    ...,
    call = call
  )

  record <-
    get_record_id(
      record = record,
      ...,
      call = call
    )

  if (require_record && !is_record_id(record)) {
    cli_abort(
      "{.arg record} must be supplied or a {.arg url}
      with a record ID must be supplied.",
      call = call
    )
  }

  if (require_comment && !is_comment_id(comment)) {
      cli_abort(
        "{.arg comment} must be a valid comment ID.",
        call = call
      )
    }

  req_airtable(
    template = template,
    base = base,
    table = table,
    record = record,
    comment = comment,
    require_base = FALSE,
    require_table = FALSE,
    method = method,
    data = data,
    call = call
  )
}
