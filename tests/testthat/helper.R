skip_if_no_pat <- function(pat = NULL, default = "AIRTABLE_PAT") {
  if (!is.null(pat)) {
    return(invisible(NULL))
  }
  pat <- Sys.getenv(default)
  skip_if_not(!is.null(pat) && pat != "")
}

set_rairtable_test_url <- function(url) {
  Sys.setenv("rairtable.test_url" = url)
}

get_rairtable_test_url <- function() {
  c("rairtable.test_url" = Sys.getenv("rairtable.test_url"))
}

get_rairtable_test_obj <- function(url = NULL,
                                   table = NULL,
                                   ...) {
  url <- url %||% as.character(get_rairtable_test_url())
  if (is.null(url) || url == "") {
    return(skip("Set `rairtable.test_url` to use `get_rairtable_test_obj()`."))
  }

  airtable(
    table = table %||% url,
    ...
  )
}
