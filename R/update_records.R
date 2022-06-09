#' Update Airtable records
#'
#' Update one or more columns of data in an Airtable table. Supports batch updates and parallel JSON encoding (recommended for large tables).
#'
#' @param data A dataframe containing the records and fields to update
#' @param airtable An airtable object
#' @param columns Columns in the data to update on Airtable. Can be a vector of character strings, unquoted column names, or a \code{dplyr} tidyselect helper like \code{starts_with()}, \code{ends_with()} or \code{everything()}. Defaults to \code{dplyr::everything()}
#' @param airtable_id_col Column containing Airtable record IDs. Not required if record IDs are stored in row names as returned from \code{read_airtable}
#' @param safely If \code{TRUE}, confirm number and names of columns to update and number of rows before executing update.
#' @param parallel If \code{TRUE} use parallel processing for encoding large tables
#' @param batch_size Number of records to update per request. Maximum of 10
#' 
#' @return A dataframe (invisibly) of the input data, to be stored as an object or piped into further `dplyr` functions
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

update_records <- function(data, airtable, columns = dplyr::everything(), airtable_id_col = NULL, safely = TRUE, parallel = FALSE, batch_size = 10){

  validate_airtable(airtable)
  stopifnot(is.data.frame(data))
  stopifnot(batch_size <= 10)
  stopifnot(is.logical(safely))
  stopifnot(is.logical(parallel))

  if (!tibble::has_rownames(data) & is.null(rlang::enexpr(airtable_id_col))){
    stop("Data must either have Airtable IDs in row names or a provided ID column", call. = FALSE)
  }

  if (is.null(rlang::enexpr(airtable_id_col))){
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


  batch_json_requests <- batch_encode_patch(data, id_col = rlang::enexpr(airtable_id_col), batch_size = batch_size, parallel = parallel)



  pb <- progress::progress_bar$new(total = length(batch_json_requests),
                                   format = "  Sending PATCH requests [:bar] :percent eta: :eta"
  )

  vpatch(batch_json_requests, airtable, pb)

  return(invisible(data))
}
