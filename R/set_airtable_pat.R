set_airtable_pat <- function(pat, install = FALSE){
  
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
      
      if(any(grepl("AIRTABLE_PAT", tv))){
        
        ans <- utils::menu(c(paste(crayon::green(cli::symbol$tick), 'Yes'),
                             paste(crayon::red(cli::symbol$cross), 'No')),
                           title = "An AIRTABLE_PAT already exists. Do you want to overwrite it?")
        
        if (ans == 1){
          
          message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
          
          oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
          newenv <- oldenv[-grep("AIRTABLE_PAT", oldenv$V1),]
          
          utils::write.table(newenv, renv, quote = FALSE, sep = "\n",
                             col.names = FALSE, row.names = FALSE
          )
          
        } else {
          
          stop_quietly(crayon::red(cli::symbol$cross), "Your AIRTABLE_PAT was not updated.")
          
        }
      }
      
    }
    
    keyconcat <- sprintf("AIRTABLE_PAT='%s'", pat)
    
    # Append pat to .Renviron file
    write(keyconcat, renv, sep = "\n", append = TRUE)
    
    message(crayon::green(cli::symbol$tick), ' Your personal access token has been stored in your .Renviron and can be accessed by Sys.getenv("AIRTABLE_PAT"). \n  To use now, restart R or run `readRenviron("~/.Renviron")`')
    return(invisible(pat))
    
  } else {
    
    Sys.setenv(AIRTABLE_PAT = pat)
    message(crayon::green(cli::symbol$tick), " AIRTABLE_PAT set for current session. To install your personal access token for use in future sessions, run this function with `install = TRUE`.")
  }
  
}