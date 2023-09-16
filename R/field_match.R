#' Match a variable to a vector of field_types
#'
#' @noRd
field_type_match <- function(type,
                             values = field_types[["type"]],
                             error_arg = caller_arg(type),
                             error_call = caller_env()) {
  arg_match(
    type,
    values = values,
    error_arg = error_arg,
    error_call = error_call
  )
}

#' Match a field name to a vector of names from a table model
#'
#' @noRd
field_name_match <- function(nm,
                             values = NULL,
                             model = NULL,
                             multiple = TRUE,
                             error_arg = caller_arg(nm),
                             error_call = caller_env()) {
  values <- values %||% names_at(model[["fields"]])

  arg_match(
    nm,
    values = values,
    multiple = multiple,
    error_arg = error_arg,
    error_call = error_call
  )
}
