#' Create an `airtable_fields_schema` object that can be used for data validation
#'
#' @param field_schema A list of lists. Each child list should have a `name`, `type`, and `id` as well as an optional `options`
#'
#' @return An `airtable_fields_schema`object
#' @export
#'
#' @examples
airtable_fields_schema <- function(field_schema){
  return(new_airtable_fields_schema(field_schema))
}