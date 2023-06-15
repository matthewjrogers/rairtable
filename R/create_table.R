#' Create or update an Airtable table
#'
#' Create or update an Airtable table or create a table model. [create_table()]
#' requires both a name and fields (or a model with a name and fields). Either a
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
  if (is_null(model)) {
    model <- make_table_model(
      name = name,
      description = description,
      fields = fields
    )
  } else {
    check_table_model(model)
  }

  base <- get_base_id(
    airtable = airtable,
    ...
  )

  resp <- request_airtable_meta(
    base = base,
    meta = "create_table",
    data = model,
    token = token
  )

  cli_progress_step(
    "Creating {.val {model[['name']]}} table with primary field
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
  check_string(name, allow_empty = FALSE, call = call)
  # TODO: Double-check if table descriptions have the same constraints as field
  # descriptions
  check_field_description(description = description, allow_null = TRUE, call = call)

  fields <- make_field_array(fields, arg = "fields", call = call)
  check_field_array(fields, call = call)

  vctrs::list_drop_empty(
    list(
      name = name,
      description = description,
      fields = fields
    )
  )
}

#' Check table model
#'
#' @noRd
check_table_model <- function(model, allow_null = FALSE, call = caller_env()) {
  if (allow_null && is_null(model)) {
    return(invisible(NULL))
  }

  check_list(model, allow_null = allow_null, call = call)

  if (!is_string(model[["name"]])) {
    cli_abort(
      "{.arg model} must include a table name.",
      call = call
    )
  }

  if (!is_list(model[["fields"]])) {
    cli_abort(
      "{.arg model} must include a field list.",
      call = call
    )
  }

  try_fetch(
    check_field_array(model[["fields"]], call = call),
    error = function(cnd) {
      cli_abort(
        "{.arg model} fields must be a valid field array.",
        parent = cnd,
        call = call
      )
    }
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

  if (has_name(table, "fields")) {
    values <- table[["fields"]][[1]][["name"]]
  }

  field_array <- lapply(
    X = make_field_list(fields, arg = arg, call = call),
    FUN = make_field_config
  )

  if (is_null(values)) {
    return(field_array)
  }

  if (is_character(values)) {
    field_array_nm <- names_at(field_array[[1]])
    nm_in_values <- field_array_nm %in% values

    if (any(nm_in_values)) {
      cli_abort(
        c("{.arg {arg}} can't use any existing field names.",
          "*" = "Rename field{?s} {.val {field_array_nm[nm_in_values]}}
          in {.arg fields} or target {.arg table}."
        ),
        call = call
      )
    }
  }

  field_array
}

#' Is x a field array list?
#'
#' This function could likely be combined with check_airtable_fields_schema()
#'
#' @noRd
check_field_array <- function(fields,
                              allow_null = FALSE,
                              allow_names = c("type", "name", "description", "options"),
                              call = caller_env()) {
  if (allow_null && is_null(fields)) {
    return(invisible(NULL))
  }

  check_list(fields, allow_null = allow_null, call = call)

  fields_names <- unlist(lapply(fields, names))

  if (!all(fields_names %in% allow_names)) {
    cli_abort(
      "{.arg fields} must only include valued named {.val {allow_names}},
      not {.val {fields_names[!(fields_names %in% allow_names)]}}.",
      call = call
    )
  }
}


#' Make a table config object
#'
#' <https://airtable.com/developers/web/api/model/table-config>
#'
#' @noRd
make_table_config <- function(...) {
  # FIXME: Call make_field_config(...) as part of making a table config object
}
