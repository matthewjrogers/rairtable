#' Create or update an Airtable table
#'
#' Create or update an Airtable table or create a table model. [create_table()]
#' requires both a name and fields (or a model with a name and fields). Either a
#' name or description must be provided for [update_table()].
#'
#' Find more information on the create table API:
#' <https://airtable.com/developers/web/api/create-table> or the update table
#' API: <https://airtable.com/developers/web/api/update-table> Find more
#' information about the table config for the Airtable API:
#' <https://airtable.com/developers/web/api/model/table-model>
#'
#' @name create_table
#' @param config A table model with a name, description, and field array from
#'   [make_table_config()].
#' @inheritParams make_table_config
#' @inheritParams request_airtable_meta
#' @return Invisibly returns a list with the table ID, name, description,
#'   primary field ID, and field names, types, and IDs.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   create_table(
#'     base = "<exmaple base ID>",
#'     name = "Apartments",
#'     description = "A to-do list of places to visit",
#'     fields = list(
#'       list(
#'         "description" = "Name of the apartment",
#'         "name" = "Name",
#'         "type" = "singleLineText"
#'       ),
#'       list(
#'         "name" = "Address",
#'         "type" = "singleLineText"
#'       ),
#'       list(
#'         "name" = "Visited",
#'         "options" = list(
#'           "color" = "greenBright",
#'           "icon" = "check"
#'         ),
#'         "type" = "checkbox"
#'       )
#'     )
#'   )
#' }
#' }
#' @rdname create_table
#' @export
#' @importFrom httr2 resp_body_json
create_table <- function(airtable = NULL,
                         config = NULL,
                         name = NULL,
                         description = NULL,
                         fields = NULL,
                         model = NULL,
                         token = NULL,
                         ...) {
  if (is_null(config)) {
    config <- make_table_config(
      name = name,
      description = description,
      fields = fields
    )
  } else if (!is_null(model)) {
    config <- copy_table_config(
      model = model,
      name = name,
      description = description
    )
  } else {
    check_table_config(config)
  }

  base <- get_base_id(
    airtable = airtable,
    ...
  )

  resp <- request_airtable_meta(
    base = base,
    meta = "create_table",
    data = config,
    token = token
  )

  cli_progress_step(
    "Creating table {.val {model[['name']]}} with primary field
    {.val {model[['fields']][[1]][['name']]}} and {length(model[['fields']])-1}
    more field{?s}"
  )

  body <- httr2::resp_body_json(resp)

  base_url <- getOption("rairtable.base_url", "https://airtable.com")
  table_url <- glue("{base_url}/{base}/{body[['id']]}")

  cli_progress_step(
    "New table created at {.url {table_url}}"
  )

  invisible(body)
}


#' @rdname create_table
#' @name update_table
#' @export
update_table <- function(airtable = NULL,
                         name = NULL,
                         description = NULL,
                         ...) {
  if (is_null(name) && is_null(description)) {
    cli_abort(
      "{.arg name} or {.arg description} must be supplied."
    )
  }

  check_field_description(description)
  check_string(name, allow_empty = FALSE, allow_null = TRUE)

  resp <- request_airtable_meta(
    airtable = airtable,
    meta = "update_table",
    method = "PATCH",
    data = vctrs::list_drop_empty(
      list(
        name = name,
        description = description
      )
    ),
    ...
  )

  invisible(httr2::resp_body_json(resp))
}
