print.airtable_fields_schema <- function(x, ...){
  prefix <- "  "
  for(i in seq_along(x)){
    
    cat(prefix, sprintf("Field Name: %s\n", x[[i]]$name))
    cat(prefix, sprintf(". ID: %s\n", x[[i]]$id))
    cat(prefix, sprintf(". Type: %s\n", x[[i]]$type))
    
    if (!is.null(x[[i]]$options)){
      cat(prefix, " . Choices:\n")
      for (choice in x[[i]]$options$choices){
        fmt <- "  .  %s (ID %s, %s) \n"
        cat(prefix, sprintf(fmt, choice$name, choice$id, choice$color))
      }
    }
  }
  return(invisible(NULL))
}
print.airtable_table_schema <- function(x, ...){
  cat(sprintf("Table Name: %s\n", x$name))
  cat(sprintf(". Table ID: %s\n", x$id))
  cat(sprintf(". Primary Field ID: %s\n", x$primaryFieldId))
  cat(". Fields:\n")
  r <- print(x$fields)
  cat("\n\n")
}
print.airtable_base_schema <- function(x, ...){
  for (table in x){
    r <- print(table)
  }
}