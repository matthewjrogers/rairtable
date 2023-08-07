#' Get metadata for an Airtable base or table
#'
#' Get an "airtable_base_schema" object, a base schema, or a list of table
#' models from a base schema. [get_table_model()] primarily exists as a helper
#' function for [copy_table_config()].
#'
#' @param base An Airtable base ID, an airtable object containing a base ID, or
#'   a URL that can be parsed for a base ID. Optional if url or airtable are
#'   supplied to the additional parameters passed to [req_airtable()].
#' @inheritParams req_airtable
#' @param ... Additional parameters:
#' - For [airtable_base()] and [get_base_schema()], additional parameters
#' may include airtable or url (one of which required if base is `NULL`).
#' - For [get_table_models()], additional parameters are passed to
#' [get_base_schema()] so may include simplifyVector.
#' - For [get_table_model()], additional parameters can include airtable or url
#' (one of which is required if table is `NULL`).
#' @returns
#'  - [airtable_base()] returns an `airtable_base_schema` object.
#'  - [get_base_schema()] returns the response from the Airtable get base schema
#'  API method
#'  - [get_table_models()] returns a tibble with the list of tables
#'  from the schema response or a list.
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

  base_schema <- get_base_schema(
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
    base_schema[[tbl]] <- vctrs::new_vctr(
      .data = base_schema[[tbl]],
      class = "airtable_table_schema"
    )

    base_schema[[tbl]][["fields"]] <- vctrs::new_vctr(
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
          table = tbl[["id"]],
          name = tbl[["name"]],
          description = tbl[["description"]],
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
#' @importFrom httr2 resp_body_json
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
#' @name get_table_models
#' @param tables,table Optional character vector of table names or IDs to
#'   include in the returned data frame or list. For [get_table_model()], table
#'   can be a schema but is typically expected to be a table name or ID.
#' @param schema A base schema list from [get_base_schema()] using
#'   `simplifyVector = FALSE`. Optional if base is supplied (or if airtable or url
#'   are supplied with additional parameters).
#' @aliases list_base_tables
#' @export
#' @importFrom tibble as_tibble
#' @importFrom vctrs obj_check_list
get_table_models <- function(base = NULL,
                             tables = NULL,
                             schema = NULL,
                             token = NULL,
                             ...,
                             call = caller_env()) {
  schema <- schema %||%
    get_base_schema(
      base = base,
      token = token,
      ...,
      call = call
    )

  vctrs::obj_check_list(
    schema,
    call = call
  )

  table_models <- schema[["tables"]]

  if (is.data.frame(table_models)) {
    table_models <- tibble::as_tibble(table_models)
  }

  if (is_null(tables)) {
    return(table_models)
  }

  filter_with_values(table_models, values = tables, call = call)
}

#' @rdname airtable_base
#' @name get_table_model
#' @param i Index value for the table to return passed to [vctrs::vec_slice()].
#'   Defaults to 1 which returns the first table from the tables listed in the
#'   base schema.
#' @export
#' @importFrom vctrs vec_slice
get_table_model <- function(table = NULL,
                            schema = NULL,
                            simplifyVector = FALSE,
                            i = 1,
                            ...,
                            call = caller_env()) {
  # If a schema is supplied to table
  if (is.list(table) && has_name(table, "tables")) {
    # expect error if more than nth tables are include in schema
    table <- table[["tables"]]
  }

  # table can be based on a url or airtable parameter
  table <- table %||% get_table_id(table = table, ..., call = call)

  # If table is a table name or table ID
  if (is_string(table)) {
    table <- get_table_models(
      schema = schema,
      simplifyVector = simplifyVector,
      ...,
      tables = table,
      call = call
    )
  }

  table <- vctrs::vec_slice(table, i, error_call = call)

  if (is_list(table)) {
    # return first element from list
    return(table[[1]])
  }

  table
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
      paste0(
        "{.value ", names_at(x, "name"), "} - {.field ", names_at(x, "id"), "}"
      ),
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
      paste0(
        "{.field ", names_at(x, "name"), "} - {.code ", names_at(x, "type"), "}"
      ),
      rep_len("*", n_fields)
    )
  )
  invisible(x)
}
