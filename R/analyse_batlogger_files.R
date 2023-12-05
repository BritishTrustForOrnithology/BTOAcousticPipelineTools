#' Analyse Batlogger Files
#' 
#' @description Analyse Batlogger files to detect GUANO metadata. If present use 
#' this to propose alternate file names. If no GUANO, use folder hierarchy to propose 
#' unique filenames. 
#' 
#' @param path_to_process = input/output path for Batlogger audio files
#' @param files_old = list of files to process in path_to_process
#' 
analyse_batlogger_files <- function(path_to_process, files_old) {  
  #get last level of folders in case needed for renaming files in root
  root <- unlist(strsplit(path_to_process, .Platform$file.sep))
  root <- root[length(root)]
  
  #vector to hold outputs
  files_new <- vector(mode = "list", length = length(files_old))
  files_guano <- vector(mode = "list", length = length(files_old))
  
  #iterate over files, checking if guano is available and if not making alt file name
  withProgress(message = "Compiling filenames...", value = 0, {
    maxits <- length(files_old)  
    
    for(f in 1:length(files_old)) {
      this_file_old <- files_old[f]
      
      #check if GUANO available for renaming
      guano <- read.guano(file.path(path_to_process, this_file_old))
      
      #if GUANO extracted...
      if(length(guano) != 0) {
        #check if key variables are present - if columns are missing, add them with NAs
        if(!"Timestamp" %in% names(guano)) guano$`Timestamp` <- NA
        if(!"Loc Position Lat" %in% names(guano)) guano$`Loc Position Lat` <- NA
        if(!"Loc Position Lon" %in% names(guano)) guano$`Loc Position Lon` <- NA
        
        #save the guano
        files_guano[[f]] <- as.data.frame(guano, stringsAsFactors = FALSE)
        
        #and if all relevant fields populated, use GUANO to rename file
        if (!is.na(guano$`Timestamp`) & !is.na(guano$`Loc Position Lat`) & !is.na(guano$`Loc Position Lon`)) {
          #get and format datetime
          dt <- gsub(pattern = "-|:", "", as.character(guano$`Timestamp`))
          dt <- gsub(pattern = " ", "_", dt)
          
          #get latlong
          lat <- guano$`Loc Position Lat`
          lon <- guano$`Loc Position Lon`
          
          #convert lat-long to character and replace decimal point with ~ as required for some batviewer apps
          lat <- gsub("\\.", "~", as.character(lat))
          lon <- gsub("\\.", "~", as.character(lon))
          
          #compile new filename in Pipeline format
          this_file_new <- file.path(dirname(this_file_old), trimws(paste0(lat, "+", lon, "_", dt, ".wav")))
        } 
        
        #if key fields not populated use folder paths
        else {
          this_file_new <- file.path(dirname(this_file_old), paste0(root, "_", gsub("\\\\|/", "_", this_file_old)))
        } 
      }
      #and if guano not populated use folder paths
      if(length(guano) == 0) {
        files_guano[[f]] <- data.frame(GUANO.Version = NA, stringsAsFactors = FALSE)
        this_file_new <- file.path(dirname(this_file_old), paste0(root, "_", gsub("\\\\|/", "_", this_file_old)))
      }
      #save potential new names
      files_new[[f]] <- this_file_new
      
      #cat(file=stderr(), this_file_new, "\n")
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/maxits, detail = f)
    }
  })
  #unpack
  files_new <- unlist(files_new)
  
  #unpack guano - as variables can vary between files, check set of names and pad with NAs before binding
  allNms <- unique(unlist(lapply(files_guano, names)))
  files_guano <- do.call(rbind, lapply(files_guano,
                                       function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                                          function(y) NA)))))
  #make useful GUANO output file
  batlogger_log <- cbind(files_old, files_new, files_guano)
  names(batlogger_log)[1] <- 'name_original'
  names(batlogger_log)[2] <- 'name_proposed'
  
  #make output table of old and proposed names with dupes warning column
  batlogger_log$id <- as.numeric(row.names(batlogger_log))
  batlogger_log$d1 <- duplicated(batlogger_log$name_proposed)
  batlogger_log <- batlogger_log[order(-batlogger_log$id),]
  batlogger_log$d2 <- duplicated(batlogger_log$name_proposed)
  batlogger_log$warning <- ifelse(batlogger_log$d1 == TRUE | batlogger_log$d2 == TRUE, 'Duplicate name', '')
  batlogger_log <- batlogger_log[order(batlogger_log$id),]
  batlogger_log$id <- NULL
  batlogger_log$d1 <- NULL
  batlogger_log$d2 <- NULL
  
  #collect some stats to report back
  n_files <- nrow(batlogger_log)
  n_dupes <- nrow(batlogger_log[batlogger_log$warning == 'Duplicate name',])
  n_with_guano <- nrow(batlogger_log[!is.na(batlogger_log$Timestamp),])
  
  #export the log if there are problems
  if(n_dupes > 0) {
    file_log <- save_batlogger_log(path_to_process, batlogger_log)
    
    shinyalert(title = "Duplicate filename error",
               text = paste("Proposed filenames include duplicates, most likely due to audio files with identical GUANO metadata. Please check the log file carefully: ", file_log),
               type = "error")
  }
  
  output <- list(batlogger_log = batlogger_log,
                 n_files = n_files,
                 n_dupes = n_dupes,
                 n_with_guano = n_with_guano
  )
  return(output)  
}