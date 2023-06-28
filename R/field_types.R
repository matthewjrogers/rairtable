#' Match a variable to a vector of field_types
#'
#' @noRd
field_type_match <- function(type,
                             error_arg = caller_arg(type),
                             error_call = caller_env()) {
  arg_match(
    type,
    values = field_types[["type"]],
    error_arg = error_arg,
    error_call = error_call
  )
}
