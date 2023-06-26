#' Make a new table config or copy a table config from an existing table model
#'
#' @description
#' Note that copying a config from a table model requires modifying or ignoring
#' some field types. See [copy_field_config()] for details.
#'
#' Learn more about the Airtable Web API table config object:
#' <https://airtable.com/developers/web/api/model/table-config>
#'
#' @param name Table name. Optional for [copy_table_config()] unless the table
#'   config is expected to be used within the same base.
#' @param description Table description. Optional.
#' @param fields Field list.
#' @inheritParams rlang::args_error_context
#' @export
make_table_config <- function(name = NULL,
                              description = NULL,
                              fields = NULL,
                              model = NULL,
                              ...,
                              call = caller_env()) {
  if (!is_null(model)) {
    table_config <- copy_table_config(
      model = model,
      name = name,
      description = description,
      ...,
      call = call
    )

    return(table_config)
  }

  check_string(name, allow_empty = FALSE, call = call)
  # TODO: Double-check if table descriptions have the same constraints as field
  # descriptions
  check_field_description(
    description = description,
    allow_null = TRUE,
    call = call
  )

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


#' @rdname make_table_config
#' @name copy_table_config
#' @inheritParams get_table_model
#' @param model A model for an existing table to copy for a new table config. If
#'   name or description are supplied, any existing name or description from the
#'   model is ignored. model is ignored if table is supplied.
#' @inheritDotParams get_table_model
#' @export
copy_table_config <- function(table = NULL,
                              model = NULL,
                              name = NULL,
                              description = NULL,
                              ...,
                              call = caller_env()) {
  if (!all(has_name(model, c("name", "fields")))) {
    table <- table %||% model

    model <- get_table_model(
      table = table,
      ...,
      call = call
    )
  }

  make_table_config(
    name = name %||% model[["name"]],
    description = description %||% model[["description"]],
    fields = get_field_config(model[["fields"]], call = call),
    call = call
  )
}


#' Check table model
#'
#' @keywords internal
#' @noRd
check_table_config <- function(model, allow_null = FALSE, call = caller_env()) {
  if (allow_null && is_null(model)) {
    return(invisible(NULL))
  }

  check_list(config, allow_null = allow_null, call = call)

  if (!is_string(config[["name"]])) {
    cli_abort(
      "{.arg config} must include a table name string.",
      call = call
    )
  }

  if (!is_list(config[["fields"]])) {
    cli_abort(
      "{.arg config} must include a field array list.",
      call = call
    )
  }

  try_fetch(
    check_field_array(config[["fields"]], call = call),
    error = function(cnd) {
      cli_abort(
        "{.arg config} fields must be a valid field array.",
        parent = cnd,
        call = call
      )
    }
  )
}

#' Get a table schema from a base schema or table name
#'
#' An internal function used by [copy_table_config()].
#'
#' @param table A base schema or, if schema is provided, a string with a table
#'   name or ID.
#' @inheritParams get_table_models
#' @keywords internal
get_table_model <- function(table = NULL,
                            schema = NULL,
                            simplifyVector = FALSE,
                            ...,
                            call = caller_env()) {
  if (has_name(table, "tables")) {
    check_list(table[["tables"]], max_length = 1, call = call)
  } else {
    table <- get_table_models(
      schema = schema,
      simplifyVector = simplifyVector,
      ...,
      tables = table,
      call = call
    )

    check_list(table, max_length = 1, call = call)
  }

  table[[1]]
}
