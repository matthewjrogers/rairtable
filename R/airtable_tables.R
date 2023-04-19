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
