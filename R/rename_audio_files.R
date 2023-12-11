#' Rename audio files
#' 
#' @description Rename the audio files from non-Pipeline friendly version to 
#' friendly version with date_time format. If there are matched xml files 
#' (batlogger only) these are also renamed. Save a log file of the changes.
#' 
#' @param path_to_process = the directory to audit
#' @param file_info = a dataframe describing all the files in the batch containing, 
#' among other things, columns for original_name and new_name
#' 
rename_audio_files <- function(path_to_process, file_info) {  
  
  #get the old and new wav and xml names
  files_old <- file_info$original_name
  files_new <- file_info$new_name
  xmls_old <- gsub(".WAV|.wav", ".xml", files_old)
  xmls_new <- gsub(".WAV|.wav", ".xml", files_new)
  
  #rename
  withProgress(message = "Renaming audio files...", value = 0, {
    wavdone <- 0
    xmldone <- 0
    for(f in 1:length(files_old)) {
      wav_rename_result <- FALSE
      xml_rename_result <- FALSE
      #rename wav
      wav_rename_result <- file.rename(files_old[f], files_new[f])
      #rename xml if present
      if(file.exists(xmls_old[f])) xml_rename_result <- file.rename(xmls_old[f], xmls_new[f])
      wavdone <- wavdone + wav_rename_result
      xmldone <- xmldone + xml_rename_result
    }
  })

  #check all changes were made
  done_all <- ifelse(wavdone == length(files_old), TRUE, FALSE)
  
  #make and save log
  prefix <- format(Sys.time(), "%Y%m%d_%H%M%S_")
  file_log <- file.path(path_to_process, paste0(prefix,"pipeline tools renaming metadata log.csv"))
  write.csv(file_info, file = file_log, row.names = FALSE)

  if(done_all == TRUE) {
    shinyalert(title = "Success",
               text = paste("All wav files renamed successfully. Details in: ", file_log),
               type = "success")
  }
  if(done_all == FALSE) {
    shinyalert(title = "Error!",
               text = paste("Not all filenames were updated properly. Check folder carefully and see", file_log),
               type = "error")
  }
  
  
}