#' Create or update an Airtable table
#'
#' Create or update an Airtable table or create a table model. At least one of
#' name or description must be provided for [update_table()].
#'
#' Find more information on the create table API:
#' <https://airtable.com/developers/web/api/create-table> or the update table
#' API: <https://airtable.com/developers/web/api/update-table> Find more
#' information about table models for the Airtable API:
#' <https://airtable.com/developers/web/api/model/table-model>
#'
#' @name create_table
#' @param model A table model with a name, description, and field list.
#'   Typically created with [make_table_model()].
#' @inheritParams make_table_model
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
                         model = NULL,
                         name = NULL,
                         description = NULL,
                         fields = NULL,
                         token = NULL,
                         ...) {
  model <- model %||%
    make_table_model(
      name = name,
      description = description,
      fields = fields
    )

  cli_progress_step(
    "Creating a new table: {.val {model[['name']]}}"
  )

  resp <- request_airtable_meta(
    airtable = airtable,
    ...,
    meta = "create_table",
    data = model,
    token = token
  )

  body <- httr2::resp_body_json(resp)


  cli_progress_step(
    "Table ID {.val {body[['id']]}} created."
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

#' @rdname create_table
#' @name make_table_model
#' @param name Table name. Required to create model for [create_table()].
#' @param description Table description. Optional.
#' @param fields Field list. Required to create model for [create_table()].
#' @inheritParams rlang::args_error_context
#' @export
make_table_model <- function(name = NULL,
                             description = NULL,
                             fields = NULL,
                             ...,
                             call = caller_env()) {
  vctrs::list_drop_empty(
    list(
      name = name,
      description = description,
      fields = make_field_array(fields, arg = "fields", call = call)
    )
  )
}

#' Make a field array
#'
#' @noRd
make_field_array <- function(fields = NULL,
                             table = NULL,
                             values = NULL,
                             ...,
                             arg = caller_arg(fields),
                             token = NULL,
                             call = caller_env()) {
  if (is_string(table)) {
    tables <- list_base_tables(..., token = token, call = call)
    table <- tables[tables[["name"]] %in% table | tables[["id"]] %in% table, ]
  }

  field_array <- lapply(
    X = make_field_list(fields, arg = arg, call = call),
    FUN = make_field_config
  )

  if (has_name(table, "fields")) {
    values <- table[["fields"]][[1]][["name"]]
  }

  if (is_null(values)) {
    return(field_array)
  }

  if (is_character(values)) {
    field_array_nm <- names_at(field_array[[1]])
    nm_in_values <- field_array_nm %in% values

    if (any(nm_in_values)) {
      cli_abort(
        c("{.arg fields} must not include any existing field names.",
          "*" = "Rename field{?s} {.val {field_array_nm[nm_in_values]}}
          in {.arg fields} or target {.arg table}."
        ),
        call = call
      )
    }
  }

  field_array
}

#' Make a table config object
#'
#' <https://airtable.com/developers/web/api/model/table-config>
#'
#' @noRd
make_table_config <- function(...) {
  # FIXME: Call make_field_config(...) as part of making a table config object
}
