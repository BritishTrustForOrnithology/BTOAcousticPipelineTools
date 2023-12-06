#' Rename audio files
#' 
#' @description Rename the audio files from non-Pipeline friendly version to 
#' friendly version with date_time format. Save a log file of the changes.
#' 
#' @param path_to_process = the directory to audit
#' @param file_info = a dataframe describing all the files in the batch containing, 
#' among other things, columns for original_name and new_name
#' 
rename_audio_files <- function(path_to_process, file_info) {  
  
  #get the old and new names
  files_old <- file_info$original_name
  files_new <- file_info$new_name
  
  #rename
  withProgress(message = "Renaming files...", value = 0, {
    file.rename(files_old, files_new)
  })
  
  #make and save log
  file_log <- file.path(path_to_process, "pipeline tools renaming metadata log.csv")
  write.csv(file_info, file = file_log, row.names = FALSE)

  shinyalert(title = "Success",
             text = paste("wav files renamed. Details in: ", file_log),
             type = "success")
  
  
}