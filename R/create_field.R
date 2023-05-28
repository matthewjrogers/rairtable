#' Create a field or column in an Airtable table
#'
#' Create a field or column in the table of an Airtable base. Find more
#' information on the create field API method and field types and options in the
#' Airtable Web API documentation:
#' <https://airtable.com/developers/web/api/create-field> Both name and type are
#' required unless field is provided.
#'
#' @param name Name for the field or column to create.
#' @param type Field type, defaults to 'singleLineText'. See
#'   `field_types` for list of supported field types.
#' @param description Optional. Description for the field or column to create or
#'   description to add to updated field. Description must be no longer than
#'   20,000 characters.
#' @param options Named list of field options. Required for some field types.
#'   See the Airtable Web API documentation on field types and cell values for
#'   more information: <https://airtable.com/developers/web/api/field-model>
#'   Default: `NULL`.
#' @param field A named list that can be used to set the name, type,
#'   description, or options of a field to create. If field is supplied with a
#'   type, any type value is ignored. Any supplied name, description, or options
#'   are used if provided.
#' @inheritParams req_airtable
#' @return A list with the created field name, field ID, description (if
#'   provided), and options.
#' @export
#' @importFrom vctrs list_drop_empty
#' @importFrom httr2 req_perform
create_field <- function(airtable = NULL,
                         name = NULL,
                         type = "singleLineText",
                         description = NULL,
                         options = NULL,
                         field = NULL,
                         token = NULL,
                         ...) {
  if (!is_null(field)) {
    check_list(field)
    name <- field[["name"]] %||% name
    type <- field[["type"]] %||% type
    description <- field[["description"]] %||% description
    options <- field[["options"]] %||% options
  }

  check_string(name, allow_empty = FALSE)
  type <- field_type_match(type)
  check_string(description, allow_null = TRUE)
  check_list(options, allow_null = TRUE)

  data <-
    vctrs::list_drop_empty(
      list(
        description = description,
        name = name,
        options = options,
        type = type
      )
    )

  req <-
    request_airtable_meta(
      airtable = airtable,
      meta = "create_field",
      data = data,
      token = token,
      ...
    )

  resp <- httr2::req_perform(req)

  invisible(httr2::resp_body_json(resp))
}


#' @rdname create_field
#' @name update_field
#' @export
update_field <- function(airtable = NULL,
                         column = NULL,
                         name = NULL,
                         description = NULL,
                         token = NULL,
                         ...) {
  check_string(name, allow_null = TRUE)
  check_field_description(description)

  req <-
    request_airtable_meta(
      airtable = airtable,
      meta = "update_field",
      column = column,
      data = vctrs::list_drop_empty(
        list(
          name = name,
          description = description
        )
      ),
      method = "PATCH",
      token = token,
      ...
    )

  resp <- httr2::req_perform(req)

  invisible(httr2::resp_body_json(resp))
}


#' Check field description
#'
#' @noRd
check_field_description <- function(description = NULL,
                                    allow_null = TRUE,
                                    call = caller_env()) {
  if (allow_null && is_null(description)) {
    return(invisible(NULL))
  }

  check_string(description, allow_empty = FALSE, call = call)

  if (nchar(description) > 20000) {
    cli_abort(
      "{.arg description} must be a string no longer than 20,000 characters.",
      call = call
    )
  }
}
