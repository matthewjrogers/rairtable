#' Create or update the structure of an Airtable table
#'
#' @name create_table
#' @inheritParams req_airtable_schema
#' @keywords internal
#' @noRd
create_table <- function(airtable = NULL,
                         model = NULL,
                         name = NULL,
                         description = NULL,
                         fields = NULL,
                         ...) {
  model <-
    make_table_model(
      model = model,
      name = name,
      description = description,
      fields = fields
    )

  # check_table_model(model)

  req <- req_airtable_schema(
    airtable = airtable,
    ...,
    data = model,
    type = "create",
    from = "table"
  )

  resp <- httr2::req_perform(req)

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
  req_airtable_schema(
    airtable = airtable,
    type = "update",
    from = "table",
    ...
  )
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
make_table_model <- function(model = NULL,
                             name = NULL,
                             description = NULL,
                             fields = NULL,
                             ...,
                             call = caller_env()) {
  model <- model %||%
    list(
      name = name,
      description = description,
      fields = make_field_array(fields, arg = "fields", call = call)
    )

  model
}
