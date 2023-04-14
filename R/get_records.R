#' Get records IDs from a data.frame column, data.frame row names, or a
#' character vector
#'
#' @keywords internal
#' @export
get_records <- function(data = NULL,
                        id_col = NULL,
                        id_col_arg = caller_arg(id_col),
                        records = NULL,
                        call = caller_env()) {
  if (is.data.frame(data)) {
    records <- get_record_id_col(
      data = data,
      id_col = id_col,
      id_col_arg = id_col_arg
    )
  } else if (is_character(data) && is_null(records)) {
    records <- data
  }

  check_character(records, call = call)
  records
}


#' @rdname get_records
#' @name get_record_id_col
get_record_id_col <- function(data,
                              id_col = NULL,
                              id_col_arg = caller_arg(id_col),
                              call = caller_env()) {
  if (!is_null(id_col)) {
    data <- select_id_col(id_col, data = data)

    if (ncol(data) > 1) {
      cli_abort(
        "{.arg {id_col_arg}} must select only a single record ID column.",
        call = call
      )
    }

    return(data[[1]])
  }

  if (tibble::has_rownames(data)) {
    return(row.names(data))
  }

  cli_abort(
    "{.arg {id_col_arg}} can't be {.code NULL} when {.arg data} has no rownames",
    call = call
  )
}

#' Use tidyselect to pull a column from a data.frame object
#'
#' @noRd
select_id_col <- function(..., data) {
  if (is_string(...)) {
    print("here")
    data[, ...]
  }

  check_installed("tidyselect")
  data[, tidyselect::eval_select(rlang::expr(c(...)), data = data)]
}

#' Use select_id_col helper function to return names of columns
#'
#' @noRd
id_col_names <- function(..., data) {
  names(select_id_col(..., data = data))
}
