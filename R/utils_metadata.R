
new_airtable_fields_schema <- function(schema = list()) {
  stopifnot(is.list(schema))
  class(schema) <- 'airtable_fields_schema'
  validate_airtable_fields_schema(schema)
  return(schema)
}


validate_airtable_fields_schema <- function(schema) {
  
  if (!all(sapply(schema, length) >= 3)) {
    cli_abort("One or more of the provided fields is of length 2, and at least 3 items are expected. 
         All fields should contain at least a name, Field ID, and data type.")
  }
  
  if (!all(sapply(schema, function(x) all(names(x) %in% c('type', 'id', 'name', 'options'))))) {
    cli_abort("One or more of the provided fields contains a value name other than 'type', 'id', 'name', or 'options'.")
  }
}

#' @keywords internal
#' @export
#'

print.airtable_fields_schema <- function(x, ...) {
  prefix <- "  "
  for(i in seq_along(x)) {
    
    cat(prefix, sprintf("Field Name: %s\n", x[[i]]$name))
    cat(prefix, sprintf(". ID: %s\n", x[[i]]$id))
    cat(prefix, sprintf(". Type: %s\n", x[[i]]$type))
    
    if (!is.null(x[[i]]$options)) {
      cat(prefix, " . Choices:\n")
      for (choice in x[[i]]$options$choices) {
        fmt <- "  .  %s (ID %s, %s) \n"
        cat(prefix, sprintf(fmt, choice$name, choice$id, choice$color))
      }
    }
  }
  return(invisible(x))
}

#' @keywords internal
#' @export
#'

print.airtable_table_schema <- function(x, ...) {
  cat(sprintf("Table Name: %s\n", x$name))
  cat(sprintf(". Table ID: %s\n", x$id))
  cat(sprintf(". Primary Field ID: %s\n", x$primaryFieldId))
  cat(". Fields:\n")
  r <- print(x$fields)
  cat("\n\n")
}

#' @keywords internal
#' @export
#'

print.airtable_base_schema <- function(x, ...) {
  for (table in x) {
    r <- print(table)
  }
}

