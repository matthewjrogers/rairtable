#' Create an `airtable_fields_schema` objec
#'
#' Create an `airtable_fields_schema` object from a list of lists to use for data
#' validation.
#'
#' @param field_schema A list of lists. Each child list should have a `name`,
#'   `type`, and `id` as well as an optional `options`
#' @return An `airtable_fields_schema` object
#' @export
airtable_fields_schema <- function(field_schema) {
  new_airtable_fields_schema(field_schema)
}

#' @noRd
new_airtable_fields_schema <- function(fields, call = caller_env()) {
  check_list(fields, call = call)
  class(fields) <- "airtable_fields_schema"
  check_airtable_fields_schema(fields)
  fields
}

#' Is x have an airtable_fields_schema class?
#'
#' @noRd
is_airtable_fields_schema <- function(x) {
  inherits(x, "airtable_fields_schema")
}

#' Error if fields does not meet the requirements for an airtable_fields_schema
#' object
#'
#' @noRd
check_airtable_fields_schema <- function(fields, call = caller_env()) {
  if (any(vapply(fields, length, NA_integer_)) < 3) {
    cli_abort(
      "Every item in {.arg fields} must be length 3 or longer with
        names {.val {c('name', 'id', 'data')}}.",
      call = call
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

#' Drop a named value from list
#'
#' @param drop Name of item from fields to drop
#' @noRd
modify_fields <- function(fields, drop = NULL) {
  lapply(fields, function(x) {
    if (!is_null(drop)) {
      x[[drop]] <- NULL
      x
    }
  })
}

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

  check_data_frame(data, arg = arg, call = call)
  if (!is_null(max_rows) && (nrow(data) > max_rows)) {
    cli_abort(
      "{.arg {arg}} must be a list or data.frame with {max_rows} row{?s},
        not {nrow(data)} row{?s}.",
      call = call
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
                             arg = caller_arg(fields),
                             call = caller_env()) {
  list(
    "fields" = lapply(
      X = make_field_list(fields, arg = arg, call = call),
      FUN = make_field_config
    )
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
    field <- make_field_list(
      field,
      max_rows = 1,
      call = call
    )
  }

  field <- field %||% list(name = name, type = type, ...)
  check_list(field, call = call)

  if (!all(has_name(field, c("name", "type")))) {
    cli_abort(
      "{.arg field} must have the names {.val name} and {.val type}.",
      call = call
    )
  }

  check_name(field[["name"]], arg = "name", call = call)

  field[["type"]] <- field_type_match(field[["type"]], error_arg = "type")

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
