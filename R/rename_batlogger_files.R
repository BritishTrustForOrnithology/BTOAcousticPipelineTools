#' Rename Batlogger Files
#' 
#' @description Rename the Batlogger files.
#' 
rename_batlogger_files <- function(path_to_process, batlogger_log) {  
  
  #pre-pend root path from search
  files_old <- file.path(path_to_process, batlogger_log$name_original)
  files_new <- file.path(path_to_process, batlogger_log$name_proposed)
  
  #rename
  withProgress(message = "Renaming files...", value = 0, {
    file.rename(files_old, files_new)
  })
  
  #make and save log
  file_log <- save_batlogger_log(path_to_process, batlogger_log)
  
  shinyalert(title = "Success",
             text = paste("wav files renamed. Details in: ", file_log),
             type = "success")
  
  
}