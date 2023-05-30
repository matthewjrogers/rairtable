#' Create or update the structure of an Airtable table
#'
#' @name create_table
#' @inheritParams request_airtable_meta
#' @keywords internal
#' @noRd
create_table <- function(airtable = NULL,
                         model = NULL,
                         name = NULL,
                         description = NULL,
                         fields = NULL,
                         token = NULL,
                         ...) {
  model <- model %||%
    make_table_model(
      name = name,
      description = description,
      fields = fields
    )

  # check_table_model(model)

  resp <- request_airtable_meta(
    airtable = airtable,
    ...,
    meta = "create_table",
    data = model,
    token = token
  )

  httr2::resp_body_json(resp)
}


#' @rdname create_table
#' @name update_table
#' @noRd
update_table <- function(airtable = NULL,
                         # Optional name and description to add to body of request
                         name = NULL,
                         description = NULL,
                         ...) {

  if (is_null(name) && is_null(description)) {
    cli_abort(
      "{.arg name} or {.arg description} must be supplied."
    )
  }

  check_field_description(description)
  check_string(name, allow_empty = FALSE, allow_null = TRUE)

  resp <- request_airtable_meta(
    airtable = airtable,
    meta = "update_table",
    method = "PATCH",
    data = vctrs::list_drop_empty(
      list(
        name = name,
        description = description
      )
    ),
    ...
  )

  httr2::resp_body_json(resp)
}

#' Make a table config object
#'
#' <https://airtable.com/developers/web/api/model/table-config>
#'
#' @noRd
make_table_config <- function(...) {
  # make_field_config(...)
}

#' Make a table model
#'
#' <https://airtable.com/developers/web/api/model/table-model>
#'
#' @noRd
make_table_model <- function(name = NULL,
                             description = NULL,
                             fields = NULL,
                             ...,
                             call = caller_env()) {
  vctrs::list_drop_empty(
    list(
      name = name,
      description = description,
      fields = make_field_array(fields, arg = "fields", call = call)
    )
  )
}
