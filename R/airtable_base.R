#' Get metadata for an Airtable base or list bases for an account
#'
#' Get an "airtable_base_schema" object, a tibble with a list of tables, or a
#' tibble with a list of bases associated with the provided personal access
#' token.
#'
#' @param base A valid Airtable base ID. Optional if url or airtable parameters
#'   are supplied to the additional parameters.
#' @param ... Additional parameters passed to [req_airtable_schema()]. Supports
#'   the use of an optional url or airtable parameter instead of base for
#'   [airtable_base()]. Additional parameters are not used by
#'   [list_airtable_bases()] at present.
#' @param metadata_api_url_pattern Metadata API URL pattern. Deprecated.
#' @param as_tibble If `TRUE`, return a tibble with a row for each table list
#'   column of fields. Defaults to `FALSE`.
#' @inheritParams set_airtable_token
#'
#' @returns An `airtable_base_schema` object or a tibble.
#' @export
#'
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom tibble as_tibble
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
airtable_base <- function(base = NULL,
                          ...,
                          metadata_api_url_pattern = deprecated(),
                          as_tibble = FALSE,
                          token = NULL) {
  req <- req_airtable_schema(
    base = base,
    ...,
    type = "schema",
    from = "base",
    token = token
  )

  resp <- httr2::req_perform(req)

  if (as_tibble) {
    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    return(tibble::as_tibble(body[["tables"]]))
  }

  body <- httr2::resp_body_json(resp)
  schema <- body[[1]]

  class(schema) <- "airtable_base_schema"

  for (i in seq_along(schema)) {
    class(schema[[i]]) <- "airtable_table_schema"
    class(schema[[i]]$fields) <- "airtable_fields_schema"
  }

  base_meta <- new.env()
  base_meta[["schema"]] <- schema
  for (table in schema) {
    base_meta[[table$name]] <- airtable(
      table = table[["name"]],
      base = base,
      fields = table[["fields"]],
      require_url = FALSE
    )
  }

  base_meta
}

#' @rdname airtable_base
#' @name list_airtable_bases
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom tibble as_tibble
list_airtable_bases <- function(...,
                                token = NULL) {
  req <- req_airtable_schema(
    ...,
    type = "list",
    from = "base",
    token = token
  )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  tibble::as_tibble(body[["bases"]])
}
