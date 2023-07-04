#' Create a new Airtable base
#'
#' Use the Airtable Metadata API to create a new Airtable bases. If schema is
#' provided and tables is `NULL`, tables is copied based on the models from an
#' existing base schema.
#'
#' @param workspace A workspace ID (a string starting with "wsp") or URL
#'   containing a workspace ID.
#' @param tables An array of table config lists. Data frames are not currently
#'   supported.
#' @inheritParams request_airtable_meta
#' @inheritParams httr2::resp_body_json
#' @seealso
#' - [get_base_schema()]
#' - [copy_table_config()]
#' @export
#' @importFrom httr2 resp_body_json
#' @importFrom tibble as_tibble
create_base <- function(name,
                        workspace = NULL,
                        tables = NULL,
                        schema = NULL,
                        simplifyVector = FALSE,
                        token = NULL,
                        ...) {
  check_name(name)

  if (!is_null(tables) && !is_null(schema)) {
    cli_alert_warning(
      "{.arg schema} is ignored if {.arg tables} is supplied."
    )
  }

  tables <- tables %||% lapply(
    schema[["tables"]],
    function(x) {
      copy_table_config(model = x)
    }
  )

  check_list(tables)

  # TODO: Consider swapping this for a check_table_config
  if (!all(vapply(
    tables,
    function(x) {
      all(has_name(x, c("name", "fields")))
    },
    TRUE
  ))) {
    cli_abort(
      "{.arg tables} must be a list where all elements contain
      a name and field array."
    )
  }

  workspace <- get_workspace_id(
    workspace = workspace,
    ...
  )

  check_string(workspace)

  resp <- request_airtable_meta(
    meta = "create_base",
    data = list(
      name = name,
      workspaceId = workspace,
      tables = tables
    ),
    token = token
  )

  workspace_url <- paste0("https://airtable.com/workspaces/", workspace)

  cli_alert_success(
    "Created new base {.val {name}} at {.url {workspace_url}}"
  )

  invisible(httr2::resp_body_json(resp, simplifyVector = simplifyVector))
}
