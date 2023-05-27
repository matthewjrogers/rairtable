#' Create a field or column in an Airtable table
#'
#' Create a field or column in the table of an Airtable base. Find more
#' information on the create field API method and field types and options in the
#' Airtable Web API documentation:
#' <https://airtable.com/developers/web/api/create-field>
#'
#' @param name Name for the field or column to create. Required.
#' @param type Field type, defaults to 'singleLineText'. Required. See
#'   field_types for list of supported field types.
#' @param description Optional. Description for the field or column to create.
#'   Default: `NULL`
#' @param options Named list of field options. Required for some field types.
#'   See the Airtable Web API documentation on field types and cell values for
#'   more information: <https://airtable.com/developers/web/api/field-model>
#'   Default: `NULL`
#' @inheritParams req_airtable
#' @return A list with the created field name, field ID, description (if
#'   provided), and options.
#' @export
#' @importFrom vctrs list_drop_empty
#' @importFrom httr2 req_perform
create_field <- function(airtable = NULL,
                         name,
                         type = "singleLineText",
                         description = NULL,
                         options = NULL,
                         token = NULL,
                         ...) {
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
