#' Prepare audio files for copying
#' 
#' @description Based on a set of parameters, prepare a list of the original audio 
#' files that will need to be copied, and calculate what the path names will be.
#' 
#' @param dat = dataframe of Pipeline results
#' @param input = the inputs object
#' @param p = numeric, threshold probability for splitting detections into high and low certainty
#' @param sampsize_noid, numeric, number of random detections to sample for the No ID class
#' @param sampsize_random, numeric, number of random detections to sample
#' @param sampsize_tophits, numeric, number of detections to copy, from descending 
#' probability, per site/date
#' @param path_audio = character, path to folder containing original audio files
#' @param path_output = character, path to folder where species folders should be created
#' 
#' @return updated copystatus
#' 
prep_copy_files <- function(dat, input, p, sampsize_noid, sampsize_random, sampsize_tophits, path_audio, path_output) {
  
  #get list of species to iterate over
  sp <- unique(dat$species)
  
  #make list to hold list of files to copy
  files_to_copy <- list()
  
  for(s in 1:length(sp)) {
    this_sp <- sp[s]
    print(this_sp)
    
    #retrieve copy method for this species
    cpmethod <- eval(parse(text=paste0("input$`", this_sp,"`")))
    #cpmethod <- 'High certainty only'
    print(cpmethod)
    
    #process each method
    
    #Method = "None" - skip to next species
    if(cpmethod == "None") next
    
    #Method = "Top hits"
    if(cpmethod == "Top hits") {
      #get detections for this species
      this_detections <- subset(dat, species == this_sp & probability >= p)
      if(nrow(this_detections)==0) next
      #sort by descending p
      this_detections <- this_detections[order(this_detections$location, this_detections$survey.date, -this_detections$probability),]
      #create counter by group (location/date): 1 = highest probability per location/date
      this_detections$num <- ave(this_detections$probability, 
                                 paste(this_detections$location, this_detections$survey.date),
                                 FUN = seq_along)
      #get top N records (or all if fewer)
      this_detections <- subset(this_detections, num <= sampsize_tophits)
      this_detections$num <- NULL
      this_detections$dir <- paste(this_detections$english.name, "(TOPHITS)")
      files_to_copy[[s]] <- this_detections
      rm(this_detections)
    }
    
    #Method = "High certainty only"
    if(cpmethod == "High certainty only") {
      #get detections for this species with probability above threshold
      this_detections <- subset(dat, species == this_sp & probability >= p)
      #confirm there are clips to copy
      if(nrow(this_detections) == 0) next
      this_detections$dir <- paste(this_detections$english.name, "(HIGH)")
      files_to_copy[[s]] <- this_detections
      rm(this_detections)
    }
    
    #Method = "High & Low certainty"
    if(cpmethod == "High & Low certainty") {
      #get detections for this species with probability above threshold and set dir
      this_detections <- subset(dat, species == this_sp)
      #confirm there are clips to copy
      if(nrow(this_detections) == 0) next
      this_detections$dir <- ifelse(this_detections$probability >= p, 
                                    paste(this_detections$english.name, "(HIGH)"),
                                    paste(this_detections$english.name, "(LOW)"))
      files_to_copy[[s]] <- this_detections
      rm(this_detections)
    }
    
    #Method = "Random sample"
    if(cpmethod == "Random sample") {
      #check which sample size to use - different for No ID
      nsamp <- ifelse(this_sp == 'No ID', sampsize_noid, sampsize_random)
      
      #get detections for this species with probability above threshold, sample and set dir
      this_detections_high <- subset(dat, species == this_sp & probability >= p)
      #if records, deal with them
      if(nrow(this_detections_high)>0) {
        #if too many records, sample
        if(nrow(this_detections_high) > nsamp) {
          this_detections_high <- this_detections_high[sample.int(n = nrow(this_detections_high), size = nsamp),]
        }
        this_detections_high$dir <- paste(this_detections_high$english.name, "(SAMPLE HIGH)")
      }
      #get detections for this species with probability below threshold, sample and set dir
      this_detections_low <- subset(dat, species == this_sp & probability < p)
      #if records, deal with them
      if(nrow(this_detections_low)>0) {
        #if too many records, sample
        if(nrow(this_detections_low) > nsamp) {
          this_detections_low <- this_detections_low[sample.int(n = nrow(this_detections_low), size = nsamp),]
        }
        this_detections_low$dir <- paste(this_detections_low$english.name, "(SAMPLE LOW)")
      }
      this_detections <- rbind(this_detections_high, this_detections_low)
      files_to_copy[[s]] <- this_detections
      rm(this_detections_low)
      rm(this_detections_high)
      rm(this_detections)
    }
    
  }
  #unpack
  files_to_copy <- do.call(rbind, files_to_copy)
  
  cat(file=stderr(), 'Checkpoint1 - initial list of files to copy compiled\n')
  
  #confirm there are records to copy
  n_recs <- nrow(files_to_copy)
  if(n_recs == 0) {
    shinyalert(title = "Error", text = "No audio files to copy with these settings", type = "error")
    return(0)
  }
  
  #get number of unique files * species permutations to copy - individual files could have more than one species etc
  file_by_spp <- unique(files_to_copy[,c("file2move", "species", "dir")])
  cat(file = stderr(), nrow(file_by_spp), "\n")
  
  #get list of all audio files in folder
  all_wavs <- data.frame(filepath = list.files(path = path_audio, 
                                               pattern = "*.wav|*.WAV", 
                                               full.names = TRUE, 
                                               recursive = TRUE),
                         stringsAsFactors = FALSE)
  all_wavs$file <- basename(all_wavs$filepath)
  cat(file=stderr(), 'Checkpoint2 - got list of original audio files\n')
  
  
  #merge with list of wavs to copy
  wavs_to_copy <- merge(all_wavs, file_by_spp, 
                        by.x = "file", by.y = "file2move", all.y = TRUE)
  
  
  return(wavs_to_copy)
}