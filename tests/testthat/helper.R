skip_if_no_pat <- function(pat = NULL, default = "AIRTABLE_PAT") {
  if (!is.null(pat)) {
    return(invisible(NULL))
  }
  pat <- Sys.getenv(default)
  skip_if_not(!is.null(pat) && pat != "")
}

set_rairtable_test_args <- function(base,
                                    table,
                                    view = NULL) {
  Sys.setenv("rairtable.test_base" = base)
  Sys.setenv("rairtable.test_table" = table)
  Sys.setenv("rairtable.test_view" = view)
}

get_rairtable_test_args <- function() {
  list(
    "test_base" = Sys.getenv("rairtable.test_base"),
    "test_table" = Sys.getenv("rairtable.test_table"),
    "test_view" = Sys.getenv("rairtable.test_view")
  )
}

get_rairtable_test_obj <- function(table = Sys.getenv("rairtable.test_table"),
                                   base = Sys.getenv("rairtable.test_base"),
                                   view = Sys.getenv("rairtable.test_view"),
                                   ...) {
  airtable(
    table,
    base,
    view,
    ...
  )
}
