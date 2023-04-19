#' Create an `airtable_fields_schema` object that can be used for data
#' validation
#'
#' @param field_schema A list of lists. Each child list should have a `name`,
#'   `type`, and `id` as well as an optional `options`
#'
#' @return An `airtable_fields_schema`object
#' @export
#'
airtable_fields_schema <- function(field_schema) {
  return(make_airtable_fields_schema(field_schema))
}

#' @noRd
make_airtable_fields_schema <- function(fields, call = caller_env()) {
  check_list(fields, call = call)
  class(fields) <- "airtable_fields_schema"
  check_airtable_fields_schema(fields)
  fields
}

#' @noRd
check_airtable_fields_schema <- function(fields, call = caller_env()) {
  if (any(vapply(fields, length, NA_integer_)) < 3) {
    cli_abort(
      c("Every item in {.arg fields} must be length 3 or longer with
        names {.val {c('name', 'id', 'data')}}.",
        call = call
      )
    )
  }

  allowed_names <- c("type", "id", "name", "options")
  fields_names <- vapply(fields, FUN = names, FUN.VALUE = NA_character_)

  if (!all(fields_names %in% allowed_names)) {
    cli_abort(
      "{.arg fields} names must be
      {.val {cli::cli_vec(allowed_names, style = c(style, `vec-last` = 'or'))}},
      not {.val {fields_names[!(fields_names %in% allowed_names)]}}.",
      call = call
    )
  }
}

#' @keywords internal
#' @export
print.airtable_fields_schema <- function(x, ...) {
  prefix <- "  "
  for (i in seq_along(x)) {
    cat(prefix, sprintf("Field Name: %s\n", x[[i]]$name))
    cat(prefix, sprintf(". ID: %s\n", x[[i]]$id))
    cat(prefix, sprintf(". Type: %s\n", x[[i]]$type))

    if (!is.null(x[[i]]$options)) {
      cat(prefix, " . Choices:\n")
      for (choice in x[[i]]$options$choices) {
        fmt <- "  .  %s (ID %s, %s) \n"
        cat(prefix, sprintf(fmt, choice$name, choice$id, choice$color))
      }
    }
  }
  invisible(x)
}

#' Vector of field types as of 2023-04-18
#'
#' https://airtable.com/developers/web/api/model/field-type
#' https://airtable.com/developers/web/api/field-model
#'
#' @noRd
field_types <-
  c(
    "singleLineText", "email", "url", "multilineText", "number", "percent",
    "currency", "singleSelect", "multipleSelects", "singleCollaborator",
    "multipleCollaborators", "multipleRecordLinks", "date", "dateTime",
    "phoneNumber", "multipleAttachments", "checkbox", "formula", "createdTime",
    "rollup", "count", "lookup", "multipleLookupValues", "autoNumber", "barcode",
    "rating", "richText", "duration", "lastModifiedTime", "button", "createdBy",
    "lastModifiedBy", "externalSyncSource"
  )

#' Convert a data.frame into a list of lists
#'
#' @noRd
make_field_list <- function(data,
                            arg = caller_arg(data),
                            max_rows = NULL,
                            call = caller_env()) {
  check_required(data, call = call)

  if (!is.data.frame(data) && is_list(data)) {
    return(data)
  }

  check_data_frame(data, call = call)
  if (!is_null(max_rows) && (nrow(data) > max_rows)) {
    cli_abort(
      "{.arg {arg}} must be a list or data.frame with {max_rows} row{?s},
        not {nrow(data)} row{?s}."
    )
  }

  do.call(
    "mapply",
    c(list, data, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  )
}

make_field_array <- function(fields = NULL,
                             call = caller_env()) {
  list(
    "fields" = lapply(make_field_list(fields), make_field_config)
  )
}


#' Make a field config object
#'
#' <https://airtable.com/developers/web/api/field-model>
#'
#' @noRd
make_field_config <- function(field = NULL,
                              name = NULL,
                              type = NULL,
                              existing_names = NULL,
                              ...,
                              call = caller_env()) {
  if (is.data.frame(field)) {
    field <- make_field_list(field, max_rows = 1)
  }

  field <- field %||% list(name = name, type = type, ...)
  check_list(field, call = call)


  if (!all(has_name(field, c("name", "type")))) {
    cli_abort(
      "{.arg field} must have the names {.val name} and {.val type}."
    )
  }

  check_name(field[["name"]], arg = "name", call = call)

  type <- field[["type"]]
  field[["type"]] <- arg_match(type, field_types, error_call = call)

  if (!is_null(existing_names) && (field[["name"]] %in% existing_names)) {
    cli_abort(
      c("A {.arg field} {.val name} value can't repeat any existing name.",
        "i" = "Existing table name values are {.val {existing_names}}"
      ),
      call = call
    )
  }

  field
}
