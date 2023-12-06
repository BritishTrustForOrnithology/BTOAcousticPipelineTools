#' Rename audio files
#' 
#' @description Rename the audio files.
#' 
rename_audio_files <- function(path_to_process, file_info) {  
  
  #pre-pend root path from search
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