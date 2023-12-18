#' List Batlogger Files
#' 
#' @description Get a list of all the files that potentially are to be renamed.
#' 
#' @param path_to_process = path containing Batlogger audio files. Can be in subdirectories
#' 
list_audio_files <- function(path_to_process) {

  #produce a list of wav files in a folder (potentially in folders and sub-folders)
  withProgress(message = "Getting list of audio files...", value = 0, {
    
    files_old <- list.files(path = path_to_process,
                            pattern="*.wav", 
                            recursive = TRUE, 
                            ignore.case = TRUE,
                            full.names = TRUE)
  })
  #check there are files returned
  if(length(files_old) == 0) {
    shinyalert(title = "Error", 
               text = "No wav files in the chosen folder. Please try again", 
               type = "error")
  }
  if(length(files_old) > 0) {
    shinyalert(title = "Audio files found", 
               text = paste(length(files_old)," wav files in the chosen folder. Proceed to Step 2"), 
               type = "success")
    return(files_old)    
  }
}
