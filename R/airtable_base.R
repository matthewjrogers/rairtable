#' Get metadata for an Airtable base
#'
#' Get metadata for an Airtable base.
#'
#' @param base A valid Airtable base ID
#' @param metadata_api_url_pattern URL for GET request. Defaults to
#'   "meta/bases/{base_id}/tables"
#' @inheritParams set_airtable_token
#'
#' @return an `airtable_base_schema` object
#' @export
#'
#' @importFrom httr2 req_perform resp_body_json
#'
#' @examples
#' \dontrun{
#' base <- airtable_base("appXXXXXXXXXXXXX")
#'
#' # read 'table1' from the base
#' tbl_data <- read_airtable(base$table1)
#'
#' # view base schema
#' print(base$schema)
#' }
airtable_base <- function(base,
                          metadata_api_url_pattern = "meta/bases/{base}/tables",
                          token = NULL) {
  check_string(base)

  req <-
    req_airtable_query(
      template = metadata_api_url_pattern,
      base = base,
      token = token
      )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  schema <- body[[1]]
  class(schema) <- "airtable_base_schema"

  for (i in seq_along(schema)) {
    class(schema[[i]]) <- "airtable_table_schema"
    class(schema[[i]]$fields) <- "airtable_fields_schema"
  }

  base_meta <- new.env()
  base_meta$schema <- schema
  for (table in schema) {
    base_meta[[table$name]] <- airtable(
      table = table$name,
      base = base,
      fields = table$fields
    )
  }

  base_meta
}
