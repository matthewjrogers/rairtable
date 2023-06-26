#' List Airtable bases accessible with a personal access token
#'
#' Use the Airtable Metadata API to list all Airtable bases accessible with a
#' personal access token. Optionally filter bases by name or base ID.
#'
#' @param bases A character vectors of base ID or name values. If provided, the
#'   results of list_bases are filtered to matching bases only. Defaults to
#'   `NULL` which returns all bases associated with the personal access token or
#'   API key.
#' @inheritParams httr2::resp_body_json
#' @seealso [get_table_models()]
#' @returns A tibble or list of base IDs, names, and permission levels.
#' @export
#' @importFrom httr2 resp_body_json
#' @importFrom tibble as_tibble
list_bases <- function(bases = NULL,
                       token = NULL,
                       simplifyVector = TRUE,
                       ...) {
  resp <- request_airtable_meta(
    meta = "list_base",
    token = token,
    ...
  )

  body <- httr2::resp_body_json(resp, simplifyVector = simplifyVector)

  base_list <- body[["bases"]]

  if (simplifyVector) {
    base_list <- tibble::as_tibble(base_list)
  }

  if (is_null(bases)) {
    return(base_list)
  }

  filter_with_values(base_list, bases)
}

#' Filter a data frame or nested list for elements with name or id values
#' matching values parameter
#'
#' @noRd
filter_with_values <- function(x,
                               values = NULL,
                               arg = caller_arg(values),
                               ...,
                               call = caller_env()) {
  check_character(values, arg = arg, call = call)
  if (is_list_of_lists(x)) {
    keep(x, function(x) {
      x[["name"]] %in% values | x[["id"]] %in% values
    })
  } else if (is.data.frame(x)) {
    x[x[["name"]] %in% values | x[["id"]] %in% values, ]
  }
}
