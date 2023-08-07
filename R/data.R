#' Airtable Field Types
#'
#' A data frame of Airtable field types as of 2023-06-27 Find more information
#' on Airtable field types at:
#' <https://airtable.com/developers/web/api/field-model>
#'
#' @format A data frame with 33 rows and 6 variables:
#' \describe{
#'   \item{`type`}{Field type string}
#'   \item{`name`}{Name of field type}
#'   \item{`url`}{API reference URL}
#'   \item{`class`}{Correponding class}
#'   \item{`read_only`}{Flag for read-only field types}
#'   \item{`options`}{Flag for field types with options}
#' }
"field_types"
