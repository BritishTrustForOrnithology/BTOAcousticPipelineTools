#' Copy a set of wav files to new locations
#' 
#' @details Takes a dataframe listing the wav files to copy and a directory where 
#' each should be put. Function creates the directories and copies the files  
#' 
#' @param wavs_to_copy = dataframe with filenames of original and directory for destination
#' 
copy_files <- function(wavs_to_copy, path_output, append_identity) {
  
  
  #get list of directories to create
  spp <- unique(wavs_to_copy$dir)
  
  #make folders for each species
  for(s in 1:length(spp)) {
    dir.create(file.path(path_output, spp[s]), showWarnings = FALSE)
  }
  cat(file=stderr(), 'Checkpoint5 - made species folders\n')
  
  #if appending species name to file, do now
  if(append_identity == TRUE) {
    cat(file=stderr(), 'Checkpoint6 - need to append spp code to filenames\n')
    #append to filename
    wavs_to_copy$file <- paste0(substr(wavs_to_copy$file,1,nchar(wavs_to_copy$file)-4),
                                '_',
                                wavs_to_copy$species,
                                ".wav")
    wavs_to_copy$temp <- NULL
    cat(file=stderr(), 'Checkpoint7 - done appending spp code to filenames\n')
  }
  
  withProgress(message = 'Copying wavs...', value = 0, {
    cat(file=stderr(), 'Checkpoint9 - starting copy of audio files\n')
    maxits <- nrow(wavs_to_copy)  
    for(r in 1:maxits) {
      cat(r, "\n")
      this_rec <- wavs_to_copy[r,]
      name_old <- this_rec$filepath
      name_new <- file.path(path_output, this_rec$dir, this_rec$file)
      
      file.copy(from = name_old, to = name_new, copy.date = TRUE)
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/maxits, detail = r)
    }
  })
  
  #clear the passed options otherwise they stay in session
  passed_path_audio <- NULL
  passed_path_output <- NULL
}