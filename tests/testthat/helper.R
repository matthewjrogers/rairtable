# Skip test if no personal access token can be found
skip_if_no_pat <- function(pat = NULL, default = "AIRTABLE_PAT") {
  if (!is.null(pat)) {
    return(invisible(NULL))
  }
  pat <- Sys.getenv(default)
  skip_if_not(!is.null(pat) && pat != "")
}

# Use the url of a new default Airtable base as a test url (set as envir
# variable)
set_rairtable_test_url <- function(url) {
  Sys.setenv("rairtable.test_url" = url)
}

# Get the test URL with get_rairtable_test_url
get_rairtable_test_url <- function() {
  c("rairtable.test_url" = Sys.getenv("rairtable.test_url"))
}

# Get a test `airtable` object with get_rairtable_test_obj
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
