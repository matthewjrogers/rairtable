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
#' @returns An `airtable_base_schema` object or a tibble listing the tables from
#'   the Airtable base.
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

  base <- get_airtable_base(
    base = base,
    ...,
    call = call
  )


  body <- httr2::resp_body_json(resp)
  base_schema <- body[["tables"]]

  base_schema <- vctrs::new_vctr(
    .data = set_list_names(base_schema, at = "name"),
    class = "airtable_base_schema"
  )

  for (tbl in seq_along(base_schema)) {
    base_schema[[tbl]] <-
      vctrs::new_vctr(
        .data = base_schema[[tbl]],
        class = "airtable_table_schema"
      )

    base_schema[[tbl]][["fields"]] <-
      vctrs::new_vctr(
        .data = set_list_names(base_schema[[tbl]][["fields"]], at = "name"),
        class = "airtable_fields_schema"
      )
  }

  list(
    "base" = base,
    "schema" = base_schema,
    "tables" = map(
      base_schema,
      function(tbl) {
        airtable(
          table = list(
            "id" = tbl[["id"]],
            "name" = tbl[["name"]],
            "description" = tbl[["description"]]
          ),
          fields = as.list(names(tbl[["fields"]])),
          base = base
        )
      }
    )
  )
}

#' Is x have an airtable_base_schema class?
#'
#' @noRd
is_airtable_base_schema <- function(x) {
  inherits(x, "airtable_base_schema")
}

#' Is x have an airtable_table_schema class?
#'
#' @noRd
is_airtable_table_schema <- function(x) {
  inherits(x, "airtable_table_schema")
}

#' Is x have an airtable_fields_schema class?
#'
#' @noRd
is_airtable_fields_schema <- function(x) {
  inherits(x, "airtable_fields_schema")
}

#' @rdname airtable_base
#' @name list_airtable_bases
#' @param base A character vector of base id values. If supplied to
#'   [list_airtable_bases()], return only those Airtable bases with matching
#'   IDs.
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom tibble as_tibble
list_airtable_bases <- function(...,
                                base = NULL,
                                token = NULL) {
  req <- req_airtable_schema(
    ...,
    type = "list",
    from = "base",
    token = token
  )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  bases <- tibble::as_tibble(body[["bases"]])

  if (is_null(base)) {
    return(bases)
  }

  check_string(base, allow_empty = FALSE)

  bases[base == bases[["id"]], ]
}

#' @export
format.airtable_base_schema <- function(x, ...) {
  formatC(vctrs::vec_data(x))
}

#' @export
vec_ptype_abbr.airtable_base_schema <- function(x, ...) {
  "atbl_app"
}

#' @export
vec_ptype_full.airtable_base_schema <- function(x, ...) {
  "airtable_base_schema"
}

#' @export
print.airtable_base_schema <- function(x, ...) {
  cli::cli_text("{.cls {class(x)}}")

  n_tbls <- length(x)
  cli::cli_text("{cli::symbol$line} {n_tbls} table{?s}:")

  cli::cli_bullets(
    set_names(
      paste0("{.value ", names_at(x, "name"), "} - {.field ", names_at(x, "id"), "}"),
      rep_len("*", n_tbls)
    )
  )

  invisible(x)
}

#' @export
format.airtable_table_schema <- function(x, ...) {
  formatC(vctrs::vec_data(x))
}

#' @export
vec_ptype_abbr.airtable_table_schema <- function(x, ...) {
  "atbl_tbl"
}

#' @export
vec_ptype_full.airtable_table_schema <- function(x, ...) {
  "airtable_table_schema"
}


#' @export
print.airtable_table_schema <- function(x, ...) {
  cli::cli_text("{.cls {class(x)}}")

  fields <- cli::cli_vec(
    names_at(x$fields),
    list("vec-trunc" = getOption("rairtable.fields-trunc", 6))
  )

  cli::cli_bullets(
    c(
      "*" = "Table: {.val {x$name}} - {.field {x$id}}",
      "*" = "Primary Field ID: {.field {x$primaryFieldId}}",
      "*" = "{length(x$fields)} field{?s} including {.field {fields}}."
    )
  )

  invisible(x)
}

#' @export
format.airtable_fields_schema <- function(x, ...) {
  formatC(vctrs::vec_data(x))
}

#' @export
vec_ptype_abbr.airtable_fields_schema <- function(x, ...) {
  "atbl_fld"
}

#' @export
vec_ptype_full.airtable_fields_schema <- function(x, ...) {
  "airtable_fields_schema"
}

#' @export
print.airtable_fields_schema <- function(x, ...) {
  cli::cli_text("{.cls {class(x)}}")

  n_fields <- length(x)
  cli::cli_text("{cli::symbol$line} {n_fields} field{?s}:")
  cli::cli_bullets(
    set_names(
      paste0("{.field ", names_at(x, "name"), "} - {.code ", names_at(x, "type"), "}"),
      rep_len("*", n_fields)
    )
  )
  invisible(x)
}
