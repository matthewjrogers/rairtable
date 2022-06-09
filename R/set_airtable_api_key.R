#' Set or install Airtable API key
#'
#' Set Airtable API key as an environment variable, and optionally install the API key to your .Renviron file for future use.
#'
#' @name set_airtable_api_key
#' @param key A valid Airtable API key
#' @param install Add your API key to .Renviron for future sessions. Optionally overwrite an existing Airtable API key.
#' 
#' @return No return value, called for side effects
#' 
#' @export
#'
#' @importFrom utils URLencode
#' @importFrom utils menu
#' @importFrom utils read.table
#' @importFrom utils write.table
#' @importFrom crayon green
#' @importFrom crayon red
#' @importFrom cli symbol
#'
#' @examples
#' \dontrun{
#' airtable_api_key("XXXXXXXXXX", install = TRUE)
#' }
#'

set_airtable_api_key <- function(key, install = FALSE){

  stopifnot(is.logical(install))

  if (install) {

    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if (!file.exists(renv)){

      file.create(renv)

    } else {
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))

      tv <- readLines(renv)

      if(any(grepl("AIRTABLE_API_KEY", tv))){

        ans <- utils::menu(c(paste(crayon::green(cli::symbol$tick), 'Yes'),
                             paste(crayon::red(cli::symbol$cross), 'No')),
                           title = "An AIRTABLE_API_KEY already exists. Do you want to overwrite it?")

        if (ans == 1){

          cat("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")

          oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
          newenv <- oldenv[-grep("AIRTABLE_API_KEY", oldenv$V1),]

          utils::write.table(newenv, renv, quote = FALSE, sep = "\n",
                             col.names = FALSE, row.names = FALSE
                             )

        } else {

          stop_quietly(crayon::red(cli::symbol$cross), "Your AIRTABLE_API_KEY was not updated.")

        }
      }

    }

    keyconcat <- sprintf("AIRTABLE_API_KEY='%s'", key)

    # Append API key to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)

    cat(crayon::green(cli::symbol$tick), 'Your API key has been stored in your .Renviron and can be accessed by Sys.getenv("AIRTABLE_API_KEY"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(invisible(key))

  } else {

    Sys.setenv(AIRTABLE_API_KEY = key)
    cat(crayon::green(cli::symbol$tick), "AIRTABLE_API_KEY set for current session. To install your API key for use in future sessions, run this function with `install = TRUE`.")
  }

}
