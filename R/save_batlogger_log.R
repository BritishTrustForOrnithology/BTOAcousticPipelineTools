#' Export batlogger log information
#' 
#' @description Save csv of original and proposed names, any GUANO content and files 
#' with duplication warnings
#' 
#' @param path_to_process = input/output path for Batlogger audio files
#' @param batlogger_log = dataframe containing all file metadata including original 
#' and proposed filenames
#' 
save_batlogger_log <- function(path_to_process, batlogger_log) {
  log <- batlogger_log
  log$File.Name <- NULL
  log$Original.Filename <- NULL
  file_log <- file.path(path_to_process, "batlogger metadata log.csv")
  write.csv(log,file = file_log, row.names = FALSE)
  return(file_log)
}