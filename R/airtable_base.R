#' Get metadata for an Airtable base or list bases for an account
#'
#' Get an "airtable_base_schema" object, a tibble with a list of tables, or a
#' tibble with a list of bases associated with the provided personal access
#' token. Additional parameters are not used by [list_bases()] at present.
#'
#' @param base An Airtable base ID. For [airtable_base()], base is optional if
#'   url or airtable are supplied and must be a string. For [list_bases()], base
#'   is a character vector of base id values and the function returns only those
#'   Airtable bases with matching IDs.
#' @param ... Optional airtable or url parameter. url must be a URL for an
#'   Airtable API call or an Airtable base. url is optional if airtable or base
#'   are supplied and airtable is optional if base or url are supplied.
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
                          token = NULL,
                          call = caller_env()) {
  base <- get_base_id(
    base = base,
    ...,
    call = call
  )

  req <- request_airtable_meta(
    base = base,
    ...,
    meta = "schema_base",
    token = token,
    call = call
  )

  resp <- httr2::req_perform(req)

  if (as_tibble) {
    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    return(tibble::as_tibble(body[["tables"]]))
  }

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

#' @rdname airtable_base
#' @name list_bases
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom tibble as_tibble
list_bases <- function(base = NULL,
                       token = NULL,
                       call = caller_env()) {
  req <- request_airtable_meta(
    meta = "list_base",
    token = token,
    call = call
  )

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  bases_list <- tibble::as_tibble(body[["bases"]])

  if (is_null(base)) {
    return(bases_list)
  }

  check_string(base, allow_empty = FALSE, call = call)

  bases_list[base == bases_list[["id"]], ]
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

#' Set list names optionally using an attribute from each item in the list
#'
#' @noRd
set_list_names <- function(x, nm = NULL, at = "name") {
  nm <- nm %||% names_at(x, at)
  set_names(x, nm)
}

#' @noRd
names_at <- function(x, at = "name") {
  vapply(x, function(x) {
    x[[at]]
  }, NA_character_)
}
