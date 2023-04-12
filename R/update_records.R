#' Update Airtable records
#'
#' Update one or more columns of data in an Airtable table. Supports batch
#' updates and parallel JSON encoding (recommended for large tables).
#'
#' @param data A dataframe containing the records and fields to update
#' @param airtable An airtable object
#' @param columns Columns in the data to update on Airtable. Can be a vector of
#'   character strings, unquoted column names, or a tidyselect helper like
#'   [dplyr::starts_with()], [dplyr::ends_with()] or [dplyr::everything()].
#'   Defaults to [dplyr::everything()]
#' @param airtable_id_col Column containing Airtable record IDs. Not required if
#'   record IDs are stored in row names as returned from [read_airtable()]
#' @param safely If `TRUE`, confirm number and names of columns to update and
#'   number of rows before executing update.
#' @param parallel If `TRUE` use parallel processing for encoding large tables
#' @param batch_size Number of records to update per request. Maximum of 10
#' @inheritParams rlang::args_error_context
#'
#' @return A data.frame (invisibly) of the input data, to be stored as an object
#'   or piped into further `dplyr` functions
#'
#' @export
#'
#' @importFrom httr PATCH
#' @importFrom httr add_headers
#' @importFrom dplyr `%>%`
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr row_number
#' @importFrom rlang enexpr
#' @importFrom rlang .data
#' @importFrom dplyr pull
#'

update_records <- function(data,
                           airtable,
                           columns = dplyr::everything(),
                           airtable_id_col = 'airtable_record_id',
                           safely = TRUE,
                           parallel = FALSE,
                           batch_size = 10,
                           call = caller_env()) {

  validate_airtable(airtable)
  check_data_frame(data)
  check_number_whole(batch_size, max = 10)
  check_logical(safely)
  check_logical(parallel)

  if (!tibble::has_rownames(data) & is.null(rlang::enexpr(airtable_id_col))) {
    cli_abort("Data must either have Airtable IDs in row names or a provided ID column", call = call)
  }

  if (is.null(rlang::enexpr(airtable_id_col))) {
    update_data <- dplyr::select(data, {{ columns }})

    update_col_names <- names(update_data)

  } else {
    update_data <- dplyr::select(data, c({{ airtable_id_col }}, {{ columns }}))

    update_col_names <- names(dplyr::select(update_data, -{{ airtable_id_col }}))
  }

  safety_check(safely,
               cancel_message = "PATCH request cancelled.",
               paste0("You are about to update ", nrow(update_data), " records of the following variables:\n    ", paste0(update_col_names, collapse = ", "), "\nDo you wish to proceed?")
               )


  batch_json_requests <- batch_encode_patch(update_data, id_col = rlang::enexpr(airtable_id_col), batch_size = batch_size, parallel = parallel)



  pb <- progress::progress_bar$new(total = length(batch_json_requests),
                                   format = "  Sending PATCH requests [:bar] :percent eta: :eta"
  )

  vpatch(batch_json_requests, airtable, pb)

  return(invisible(data))
}
