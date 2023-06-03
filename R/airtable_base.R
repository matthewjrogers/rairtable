#' Get metadata for an Airtable base or list bases for an account
#'
#' Get an "airtable_base_schema" object, a tibble with a list of tables, or a
#' tibble with a list of bases associated with the provided personal access
#' token. Additional parameters are not used by [list_bases()] at present.
#'
#' @param base An Airtable base ID, an airtable object containing a base ID, or
#'   a URL that can be parsed for a base ID. Optional if url or airtable are
#'   supplied to the additional parameters passed to [req_airtable()].
#' @inheritParams req_airtable
#' @param ... Additional parameters passed to [req_airtable()].
#'
#' @returns
#'  - [airtable_base()] returns an `airtable_base_schema` object.
#'  - [get_base_schema()] returns the response from the Airtable get base schema
#'  API method or [list_base_table()] returns a tibble with the list of tables
#'  from the schema response.
#'  - [list_bases()] returns a tibble of base IDs, names, and permission levels.
#' @export
#' @importFrom vctrs new_vctr
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
                          token = NULL,
                          ...) {
  base <- get_base_id(
    base = base,
    ...,
  )

  base_schema <-
    get_base_schema(
      base = base,
      token = token,
      simplifyVector = FALSE
    )

  base_schema <- base_schema[["tables"]]

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

#' @rdname airtable_base
#' @name get_base_schema
#' @inheritParams httr2::resp_body_json
#' @export
#' @importFrom httr2 req_perform resp_body_json
get_base_schema <- function(base = NULL,
                            token = NULL,
                            simplifyVector = TRUE,
                            ...,
                            call = caller_env()) {
  resp <- request_airtable_meta(
    base = base,
    ...,
    meta = "schema_base",
    token = token,
    call = call
  )

  httr2::resp_body_json(resp, simplifyVector = simplifyVector)
}

#' @rdname airtable_base
#' @name list_base_tables
#' @export
#' @importFrom tibble as_tibble
list_base_tables <- function(base = NULL,
                             token = NULL,
                             ...,
                             call = caller_env()) {
  body <- get_base_schema(base = base, token = token, ..., call = call)

  tibble::as_tibble(body[["tables"]])
}

#' @rdname airtable_base
#' @name list_bases
#' @param bases A character vectors of base ID or name values. If provided, the
#'   results of list_bases are filtered to matching bases only. Defaults to
#'   `NULL` which returns all bases associated with the personal access token or
#'   API key.
#' @export
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom tibble as_tibble
list_bases <- function(bases = NULL,
                       token = NULL,
                       ...) {
  resp <- request_airtable_meta(
    meta = "list_base",
    token = token,
    ...
  )

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  base_list <- tibble::as_tibble(body[["bases"]])

  if (is_null(bases)) {
    return(base_list)
  }

  check_character(bases, call = call)

  base_list[(base_list[["id"]] %in% bases) | (base_list[["name"]] %in% bases), ]
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
