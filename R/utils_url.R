#' @noRd
is_url <- function(x) {
  if (is_null(x)) {
    return(FALSE)
  }

  grepl(
    "http[s]?://(?:[[:alnum:]]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' @noRd
check_airtable_url <- function(url,
                               require_base = TRUE,
                               require_table = TRUE,
                               require_view = FALSE,
                               call = caller_env()) {
  check_string(url, call = call)
  missing <- NULL

  if (!grepl("app", url) && require_base) {
    missing <- c(missing, "a {.arg base} name starting with {.val app}")
  }

  if (!grepl("tbl", url) && require_table) {
    missing <- c(missing, "a {.arg table} name starting with {.val tbl}")
  }

  if (!grepl("viw", url) && require_view) {
    missing <- c(missing, "a {.arg view} name starting with {.val viw}")
  }

  if (is_url(url) && is.null(missing)) {
    return(invisible(NULL))
  }

  message <-  "{.arg url} is not a valid Airtable API url."

  if (!is.null(missing)) {
    missing <- paste0("{.arg url} is missing ", oxford_comma(missing, final = "and"), ".")

    message <- c(
      message,
      "i" = missing
    )
  }

  cli::cli_abort(
    message = message,
    call = call
  )
}
