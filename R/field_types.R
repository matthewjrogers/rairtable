#' Airtable field types
#'
#' A vector of Airtable field types as of 2023-04-18. Find more information on
#' Airtable field types at
#' <https://airtable.com/developers/web/api/model/field-type>
#'
#' @export
field_types <-
  c(
    "singleLineText", "email", "url", "multilineText", "number", "percent",
    "currency", "singleSelect", "multipleSelects", "singleCollaborator",
    "multipleCollaborators", "multipleRecordLinks", "date", "dateTime",
    "phoneNumber", "multipleAttachments", "checkbox", "formula", "createdTime",
    "rollup", "count", "lookup", "multipleLookupValues", "autoNumber",
    "barcode", "rating", "richText", "duration", "lastModifiedTime", "button",
    "createdBy", "lastModifiedBy", "externalSyncSource"
  )


#' Match a variable to a vector of field_types
#'
#' @noRd
field_type_match <- function(type,
                             error_arg = caller_arg(type),
                             error_call = caller_env()) {
  arg_match(
    type,
    values = field_types,
    error_arg = error_arg,
    error_call = error_call
  )
}
