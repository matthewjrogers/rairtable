#' Make a field config or a list of field config elements
#'
#' @description
#'
#' These functions are intended for advanced users or developers.
#'
#' [make_field_config()] is a helper function for creating a field config for
#' adding new fields to an existing table or to add to a table config object.
#'
#' [get_field_config()] gets a field config from an existing table model with
#' some modifications to field types (required for compatibility with the
#' Airtable API). See details.
#'
#' Learn more about the Airtable Web API field model:
#' <https://airtable.com/developers/web/api/field-model>
#'
#' @param field A list with a name and type value. Optionally can include
#'   options and description. If field is a single row data frame, it is
#'   converted to a list. If field is a list of lists, the function is applied
#'   to each element in the list.
#' @param name A string to use as a name for the field. Optional if field is supplied.
#' @param type A type to use for the field. Must be a string matching a value in
#'   `field_types`. Optional if field is supplied.
#' @inheritParams rlang::args_error_context
#' @seealso field_types
#' @export
make_field_config <- function(field = NULL,
                              name = NULL,
                              type = NULL,
                              ...,
                              call = caller_env()) {
  field <- field %||% list(name = name, type = type, ...)

  if (is_list_of_lists(field)) {
    return(lapply(field, make_field_config))
  }

  if (is.data.frame(field)) {
    field <- make_list_of_lists(
      field,
      max_rows = 1,
      call = call
    )[[1]]
  }

  check_field_config(
    field,
    call = call
  )

  field
}

#' Check if field is a valid field config list
#'
#' Note that this function does not yet validate field type specific options.
#'
#' @noRd
check_field_config <- function(field,
                               name_arg = "name",
                               type_arg = "type",
                               call = caller_env()) {
  check_list(field, call = call)

  if (!all(has_name(field, c("name", "type")))) {
    cli_abort(
      "{.arg field} must have the names {.val name} and {.val type}.",
      call = call
    )
  }

  check_name(field[["name"]], arg = name_arg, call = call)

  field[["type"]] <- field_type_match(
    field[["type"]],
    error_arg = type_arg
  )

  field
}

#' Create a list of lists from a data frame or list
#'
#' Make a field list to create or update records or prepare a field
#' specification for a table model to use with [create_table()].
#'
#' @param data A data frame or list. Typically, with field specifications.
#' @param cols A character vector of column names to include in the returned
#'   field list.
#' @param max_rows The maximum number of rows allowed if data is a data frame.
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @noRd
make_list_of_lists <- function(data,
                               arg = caller_arg(data),
                               max_rows = NULL,
                               cols = NULL,
                               call = caller_env()) {
  check_required(data, call = call)

  if (!is.data.frame(data)) {
    if (!is_list(data)) {
      cli_abort(
        "{.arg {arg}} must be a list or data frame.",
        call = call
      )
    }

    if (!is_list_of_lists(data)) {
      # FIXME: Double-check that this is consistent with the structure for data
      # frames returned
      data <- list(data)
    }

    return(data)
  }

  check_data_frame(data, arg = arg, call = call)

  if (!is_null(max_rows) && (nrow(data) > max_rows)) {
    cli_abort(
      "{.arg {arg}} must be a list or data frame with {max_rows} row{?s},
        not {nrow(data)} row{?s}.",
      call = call
    )
  }

  if (!is_null(cols)) {
    data <- select_cols(
      tidyselect::any_of(cols),
      .data = data
    )
  }

  do.call(
    "mapply",
    c(
      FUN = list,
      data,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
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
    tables <- get_table_models(..., tables = table, token = token, call = call)
  }

  if (has_name(table, "fields")) {
    values <- table[["fields"]][[1]][["name"]]
  }

  field_array <- lapply(
    X = make_list_of_lists(
      fields,
      cols = c("type", "name", "description", "options"),
      arg = arg,
      call = call
    ),
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
#' This function could possibly be combined with check_airtable_fields_schema()
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


#' @name get_field_config
#' @rdname make_field_config
#'
#' @details Modifications from table model fields to copied field config
#'
#' Fields with type "lastModifiedTime" or "createdTime" are dropped from the
#' config, fields with "multipleRecordLinks" are converted to "singleLineText"
#' unless a replacement linkedTableId is provided, and only name and color
#' options are retained for "singleSelect" or "multipleSelects" fields.
#'
#' @param fields A list of fields. Optional if table or table and schema are
#'   supplied.
#' @param table A table schema from [get_table_models()], a base schema with a
#'   single table, or a string with a table name (if schema is provided).
#' @param schema A base schema from [get_base_schema()] created with
#'   `simplifyVector = FALSE`.
#' @param model A model for an existing table to use for a new field config.
#' @inheritParams prep_table_field
#' @param ... Additional parameters. Unused.
#' @inheritParams rlang::args_error_context
#' @export
get_field_config <- function(fields = NULL,
                             table = NULL,
                             schema = NULL,
                             model = NULL,
                             linkedTableId = NULL,
                             viewIdForRecordSelection = NULL,
                             ...,
                             call = caller_env()) {
  fields <- get_model_fields(fields, table, schema, model, call = call)

  check_list(fields, call = call)

  lapply(
    fields,
    prep_table_field,
    linkedTableId = linkedTableId,
    viewIdForRecordSelection = viewIdForRecordSelection
  )
}

#' Get fields list from base or table schema
#'
#' @noRd
get_model_fields <- function(fields = NULL,
                             table = NULL,
                             schema = NULL,
                             model = NULL,
                             ...,
                             call = caller_env()) {
  if (is_list_of_lists(fields)) {
    return(fields)
  }

  model <- model %||% get_table_model(
    table = table,
    schema = schema,
    ...,
    call = call
  )

  fields <- model[["fields"]]

  if (is_empty(fields)) {
    cli_abort(
      c("{.arg model} must be a list with fields.",
        "*" = "Check input {.arg table}, {.arg schema},
        or {.arg model} for issues."
      ),
      call = call
    )
  }

  fields
}

#' Prep a field configuration from a table model for use as a new field config
#'
#' Internal helper function.
#'
#' @param linkedTableId Table ID for a linked table option to use with any
#'   multipleRecordLinks field type.
#' @param viewIdForRecordSelection View ID from linked table to use for record
#'   selection on multipleRecordLinks field type.
#' @keywords internal
prep_table_field <- function(field,
                             linkedTableId = NULL,
                             viewIdForRecordSelection = NULL) {
  fld_name <- field[["name"]]
  fld_type <- field[["type"]]
  fld_options <- field[["options"]]

  if (fld_type %in% c("lastModifiedTime", "createdTime")) {
    # Drop lastModifiedTime and createdTime fields
    cli::cli_progress_step(
      "Dropping field {.val {fld_name}}"
    )

    return(NULL)
  }

  if (fld_type == "multipleRecordLinks") {
    if (!is_null(linkedTableId)) {
      # Replace linkedTableId and viewIdForRecordSelection with set values
      fld_options[["linkedTableId"]] <- linkedTableId
      fld_options[["viewIdForRecordSelection"]] <- viewIdForRecordSelection
    } else {
      cli::cli_progress_step(
        "Modifying field type for {.val {fld_name}}"
      )

      fld_type <- "singleLineText"
    }
  }

  if (fld_type == "multipleAttachments") {
    # Drop options for multipleAttachments type fields
    fld_options <- NULL
  }

  if (fld_type %in% c("singleSelect", "multipleSelects")) {
    # Keep only the name and color for choices of singleSelect and
    # multipleSelects fields
    fld_options[["choices"]] <- lapply(
      fld_options[["choices"]],
      # FIXME: There must be a more elegant function for subsetting nested list
      # elements by name
      function(y) {
        list(
          name = y[["name"]],
          color = y[["color"]]
        )
      }
    )
  }

  vctrs::list_drop_empty(
    list(
      name = fld_name,
      type = fld_type,
      description = field[["description"]],
      options = fld_options
    )
  )
}
