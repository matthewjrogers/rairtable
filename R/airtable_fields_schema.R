#' Create an `airtable_fields_schema` object
#'
#' Create an `airtable_fields_schema` object from a list of lists to use for
#' data validation.
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
  fields <- vctrs::new_vctr(fields, class = "airtable_fields_schema")
  check_airtable_fields_schema(fields, call = call)
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
#' Note: this function maybe could use [check_airtable_field()]
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

  # TODO: Double-check if "description" should be added to the allowed names
  allow_names <- c("type", "id", "name", "options")
  fields_names <- vapply(fields, FUN = names, FUN.VALUE = NA_character_)

  if (!all(fields_names %in% allow_names)) {
    cli_abort(
      "{.arg fields} names must be
      {.val {cli::cli_vec(allow_names, style = c(`vec-last` = 'or'))}},
      not {.val {fields_names[!(fields_names %in% allow_names)]}}.",
      call = call
    )
  }
}
