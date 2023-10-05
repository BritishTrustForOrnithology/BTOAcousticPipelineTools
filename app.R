#########################################################################
#'                                                                      # 
#' DO NOT RUN THIS PROGRAM DIRECTLY. INSTEAD RUN LAUNCH_SHINY_APP.R     #
#'                                                                      # 
#########################################################################



#' Shiny App to assist with auditing of audio files processed via BTO Acoustic Pipeline 
#' Written by Simon Gillings
#' July 2022

require(shiny)
require(shinyFiles)
require(shinyalert)
require(shinyjs)
require(tidyr)
#require(guano)
require(DT)


#' THE FUNCTIONS PART -------------------------------------------------------------------------------


#' Read and validate an Acoustic Pipeline results csv file
#' 
#' @description Read and check contents of a Pipeline results csv file. Checks if format is 
#' from cloud Pipeline or from local run of pipeline code. Data are reformatted as needed
#' for consistency.
#' 
#' @param file = character, full file path to a Pipeline results csv file
#' 
#' @return dataframe of Pipeline results
#' 
read_AP_csv <- function(file) {
  #try to read the csv file
  dat <- tryCatch(
    {read.csv(file, stringsAsFactors = FALSE, na.strings = c('NA','null'))}, 
    error = function(cond) {
      shinyalert(title = "Error",
                 text = paste("The following file cannot be read:", basename(file)),
                 type = "error",
                 callbackR = message(paste("bad file:", file)))
      #empty return when data are not the right format
      return(NA)
    },
    warning = function(cond) {
      read.csv(file, stringsAsFactors = FALSE, na.strings = c('NA','null'))
    }
  )
  
  #if try failed to return data, return NA
  if(!is.data.frame(dat)) return(NA)
  
  #otherwise continue
  names(dat) <- tolower(names(dat))
  
  #which format of file is this?
  type <- NA
  #offline file format - should have 13 columns
  if(length(names(dat)) == 13) {
    cat(file=stderr(), "Reading offline format file\n")
    if(all.equal(names(dat), c("upload_directory", "filename", "prefix", "species", "probability",
                               "actual_date", "session_date", "time", "scientific_name", "english_name",
                               "group", "warnings", "classifier_code"))) {
      type <- 'offline'
      names(dat)[which(names(dat)=='filename')] <- 'file2move'
      names(dat)[which(names(dat)=='group')] <- 'species.group'
      names(dat)[which(names(dat)=='session_date')] <- 'survey.date'
      names(dat) <- gsub("_", ".", names(dat))
      dat$location <- dat$prefix
    }
  }
  #cloud file format1 - should have 19 columns
  if(length(names(dat)) == 19) {
    cat(file=stderr(), "Reading cloud1 format file\n")
    if(all.equal(names(dat), c("recording.file.name", "original.file.name", "original.file.part",
                               "latitude", "longitude", "species", "scientific.name",
                               "english.name", "species.group", "probability", "warnings",
                               "actual.date", "survey.date", "time", "classifier.name",
                               "user.id", "upload.key", "upload.name", "survey.name"))) {
      names(dat)[which(names(dat)=='original.file.name')] <- 'file2move'
      dat$location <- paste(dat$latitude, dat$longitude, sep='~')
      type <- 'cloud1'
    }
  }
  #audible file format - should have 20 columns
  if(length(names(dat)) == 20) {
    cat(file=stderr(), "Reading audible cloud1 format file\n")
    if(all.equal(names(dat), c("recording.file.name", "original.file.name", "original.file.part",
                               "latitude", "longitude", "species", "scientific.name",
                               "english.name", "species.group", "score", "warnings", "call.type",
                               "actual.date", "survey.date", "time", "classifier.name",
                               "user.id", "upload.key", "batch.name", "project.name"))) {
      names(dat)[which(names(dat)=='recording.file.name')] <- 'file2move'
      dat$file2move <- paste0(dat$file2move, '.wav')
      dat$location <- paste(dat$latitude, dat$longitude, sep='~')
      dat$english.name <- ifelse(!is.na(dat$call.type), paste0(dat$english.name, " (", dat$call.type, ")"), dat$english.name)
      dat$species.group <- ifelse(is.na(dat$species.group), 'Birds', dat$species.group)
      dat$call.type <- NULL
      
      names(dat)[which(names(dat)=='score')] <- 'probability'
      type <- 'audible'
    }
  }
                                
  #error if file is not an expected format for a Pipeline results file
  if(is.na(type)) {
    shinyalert(title = "Error",
               text = paste("The following file does not match expected Pipeline results format. Ensure the folder only contains valid Pipeline results files.", basename(file)),
               type = "error",
               callbackR = message(paste("bad file:", file)))
    #empty return when data are not the right format
    return(0)
  }
  
  #if type is expected, return dat
  return(dat)
}




#' Compile Acoustic Pipeline results
#' 
#' @description Compile the Acoustic Pipeline results csv files from a folder
#' 
#' @param files = list of csv file locations
#' 
#' @return dataframe of Pipeline results from one or more files
#' 
get_results <- function(files) {
  metrics <- list()
  #path <- "E:/analysis/BTOAcousticPipelineTools/testing/pipeline_results"
  #path <- "C:/Users/simon.gillings/Downloads/20220707"
  # List results file to be read and merged
  #list_files_results <- list.files(path = path, pattern="*.csv", full.names = TRUE)
  
  metrics$num_files <- length(files)
  
  if(metrics$num_files == 0) {
    shinyalert(title = "Error", text = "No csv files selected", type = "warning")
    return(NULL)
  }
  
  #if files are present, loop over files to collate results
  if(metrics$num_files > 0) {
    
    withProgress(message = 'Reading Pipeline Results files', value = 0, {
      
      maxits <- metrics$num_files
      
      #read the results dataset(s) - returning errors/aborting if any of the csvs aren't results files
      dfs <- list()
      for(f in 1:metrics$num_files) {
        dfs[[f]] <- read_AP_csv(file = files[f])

        # Increment the progress bar, and update the detail text.
        incProgress(1/maxits, detail = paste("Reading file", f))
      }
      
    })    
    detections <- do.call(rbind, dfs)  
    
    
    nrows <- nrow(detections)
    metrics$num_detections <- nrows
    if(nrows == 0) {
      shinyalert(title = "Error", text = "Something went wrong. No data were read from selected location", 
                 type = "error")
      return(NULL)
    }
    if(nrows > 0) {
      withProgress(message = 'Compiling dataset...(this may take a while for big datasets)', {

        #tidy up the No ID data
        detections$species.group <- ifelse(detections$species == 'No ID', '__', detections$species.group)
        detections$english.name <- ifelse(detections$species == 'No ID', 'Noise', detections$english.name)
        detections$probability <- ifelse(detections$species == 'No ID', 1, detections$probability)
        
        #detections$english.name <- gsub("'", "", detections$english.name)
        
        #aggregate by filename and species, taking max probability. This deals with cases 
        #where users have uploaded long clips which have then been split into 5s sections, 
        #potentially returning multiple hits for the whole file and causing undue duplication
        #in audit
        cat(file=stderr(), "No. rows in dat BEFORE picking highest probability per file*species:", nrow(detections), "\n")
        detections <- aggregate(data = detections, probability ~ file2move + species.group + english.name + species + location + survey.date, max)
        cat(file=stderr(), "No. rows in dat AFTER picking highest probability per file*species:", nrow(detections), "\n")
        metrics$num_detections_after_aggregation <- nrow(detections)
        #for similar reasons, it is possible to get a "No ID" return for a file section 
        #when the rest of the file contains species. These should be dropped
        noid <- subset(detections, species == 'No ID')
        metrics$num_detections_noid <- nrow(noid)
        metrics$num_detections_noid_after_check <- nrow(noid)
        if(nrow(noid) > 0) {
          id <- subset(detections, species != 'No ID')
          cat(file=stderr() , "No. NoID detections before checking for IDs in same file:", nrow(noid), "\n")
          noid <- noid[!noid$file2move %in% id$file2move,]
          cat(file=stderr() , "No. NoID detections AFTER checking for IDs in same file:", nrow(noid), "\n")
          metrics$num_rows_noid_after_check <- nrow(noid)
          detections <- rbind(id, noid)
        }
        metrics$num_detections_after_noid_check <- nrow(detections)
      })
      out <- list(metrics = metrics, detections = detections)
      message <- paste("Read", nrows,"recordings from selected folder. Proceed to next step")
      shinyalert(title = "Success", text = message, 
                 type = "success")
      return(out)
    } #end if nrows>0
  } #end if files
} #end func



#' Analyse Acoustic Pipeline results
#' 
#' @description Split results into high and low certainty based on selected threshold probability 
#' and report cross-tabulation of number of recordings per species.
#' 
#' @param dat = dataframe of Pipeline results
#' @param p = numeric, threshold probability for splitting detections into high and low certainty
#' 
#' @return dataframe containing cross-tabulation of Pipeline results by species and certainty.
#' 
analyse_results <- function(dat, p) {
  
  #label up whether high or low according to preferred threshold
  dat$certainty <- ifelse(dat$probability >= p, 'High', 'Low')
  
  #mark No ID as High  
  dat$certainty <- ifelse(dat$species == 'No ID', 'High', dat$certainty)
  
  #now get number of identities per species  
  dat$f <- 1
  xtab <- aggregate(data = dat, f ~ species.group + english.name + species + certainty, NROW)
  xtab <- pivot_wider(data = xtab, 
                      id_cols = c(species.group, english.name, species), 
                      names_from = certainty, 
                      values_from = f, values_fill = list(f = 0))
  #enforce having both Low and High columns
  if(!'Low' %in% names(xtab)) xtab$Low <- 0
  if(!'High' %in% names(xtab)) xtab$High <- 0
  xtab <- xtab[order(xtab$species.group, xtab$species),c('species.group', 'english.name', 'species', 'Low', 'High')]
  names(xtab) <- c('Group','Species', 'Species Code', 'Low', 'High')
  return(xtab)
}



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




#' @description Copy a set of wav files to new locations
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
      
      file.copy(from = name_old, to = name_new)
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/maxits, detail = r)
    }
  })
  
  #clear the passed options otherwise they stay in session
  passed_path_audio <- NULL
  passed_path_output <- NULL
}



#' List Batlogger Files
#' 
#' @description Get a list of all the files that potentially are to be renamed.
#' 
#' @param path_to_process = path containing Batlogger audio files. Can be in subdirectories
#' 
list_batlogger_files <- function(path_to_process) {
  #produce a list of wav files in a folder (potentially in folders and sub-folders)
  withProgress(message = "Getting list of audio files...", value = 0, {
    
    files_old <- list.files(path = path_to_process,
                            pattern="*.wav", 
                            recursive = TRUE, 
                            ignore.case = TRUE,
                            full.names = FALSE)
  })
  #check there are files returned
  if(length(files_old) == 0) {
    shinyalert(title = "Error", 
               text = "No wav files in the chosen folder. Please try again", 
               type = "error")
  }
  if(length(files_old) > 0) {
    shinyalert(title = "Success", 
               text = paste(length(files_old)," wav files in the chosen folder. Proceed to Step 2"), 
               type = "success")
    return(files_old)    
  }
}



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



#' Audit audio folders
#' 
#' @description Run some basic checks on the audio folders prior to upload to the Pipeline.
#' 
#' @param path = path to audio folder
#'
audit_audio <- function(path) {
  #produce a list of wav files in a folder (potentially in folders and sub-folders)
  files_audio <- list.files(path = path,
                            pattern="*.wav", 
                            recursive = TRUE, 
                            ignore.case = TRUE,
                            full.names = FALSE)
  
  num_files <- length(files_audio)
  num_unique_filenames <- length(basename(files_audio))
  output <- list(num_files = num_files, 
                 num_unique_filenames = num_unique_filenames)
  return(output)
}


# GUANO metadata package for R
# from https://github.com/riggsd/guano-r/blob/master/guano/R/guano.R
# Accessed: 22/09/2023
# Licencs: MIT.

#' Convert ISO 8601 timestamps into strings - IGNORE TIMEZONE FOR THIS PURPOSE
.parse.timestamp <- function(s) {
  #check and replace space for T between date and time - ISO deviation in some WA recorders
  if(gregexpr(" ",s)[[1]][1]==11) {
    s <- gsub(" ","T",s)
  }
  return(strptime(s, "%Y-%m-%dT%H:%M:%S"))
}


#' THIS version attempts to fix the poor formating of WA metadata but still some issues with tz conversion
#' Parse ISO 8601 subset timestamps
#' .parse.timestamp <- function(s) {
#'   #check and replace space for T between date and time - ISO deviation in some WA recorders
#'   if(gregexpr(" ",s)[[1]][1]==11) {
#'     s <- gsub(" ","T",s)
#'   }
#'   
#'   #UTC times
#'   if (is.null(s) || is.na(s) || s == "") {
#'     return(NA)
#'   } else if (endsWith(s, "Z")) {
#'     return(strptime(s, "%Y-%m-%dT%H:%M:%S", tz="UTC"))
#'   
#'   # UTC offset
#'   } else if (length(gregexpr(":", s)[[1]]) == 3) {
#'     len <- nchar(s)
#'     #ORIGINAL utc_offset <- strtoi(substr(s, len-5, len-3), base=10)      # UTC offset hours, eg: 4
#'     #get timestamp component by looking for +/-
#'     if(length(gregexpr("-",s)[[1]])==3) {
#'       utc_offset <- strtoi(substr(s, gregexpr("-",s)[[1]][3], len-3), base=10)
#'     }
#'     if(grep("+",s, fixed=TRUE) == 1) {
#'       utc_offset <- strtoi(substr(s, gregexpr("+",s, fixed=TRUE)[[1]][1], len-3), base=10)
#'     }
#'     tz <- paste("Etc/GMT", sprintf("%+d", utc_offset), sep="")  # timezone, eg: "Etc/GMT+4"
#'     return(strptime(s, "%Y-%m-%dT%H:%M:%S", tz=tz))
#'   
#'   # local
#'   } else {
#'     return(strptime(s, "%Y-%m-%dT%H:%M:%S"))
#'   }
#' }



#' Maps metadata keys to a data type coercion function
data.types <- list(
  `Filter HP`=as.double,
  `Filter LP`=as.double,
  Humidity=as.double,
  Length=as.double,
  `Loc Accuracy`=as.integer,
  `Loc Elevation`=as.double,
  Note=function(val) gsub("\\\\n", "\n", val),
  Samplerate=as.integer,
  #`Species Auto ID`=?, `Species Manual ID`=?,  # TODO: comma separated
  #Tags=?,  # TODO: comma separated
  TE=function(val) if (is.na(val) || is.null(val) || val == "") 1 else as.integer(val),
  `Temperature Ext`=as.double, `Temperature Int`=as.double,
  Timestamp=.parse.timestamp
)


#' Read a single GUANO file
#' 
#' @param filename The GUANO filename or path
#' @param verbose Whether to print the raw GUANO lines before parsing data types
#' @return list of named metadata fields
read.guano <- function(filename, verbose = FALSE) {
  f <- file(filename, "rb")
  riff.id <- readChar(f, 4)
  if (length(riff.id) == 0 || riff.id != "RIFF") return(NULL)
  riff.size <- readBin(f, integer(), size=4, endian="little")
  wave.id <- readChar(f, 4)  # "WAVE"
  if (length(wave.id) == 0 || wave.id != "WAVE") return(NULL)
  
  read.subchunk <- function() {
    id <- readChar(f, 4)
    if (length(id) == 0 || id == "") return(NULL)
    size <- readBin(f, integer(), size=4, endian="little")
    list(id=id, size=size)
  }
  
  skip.subchunk <- function(chunk) {
    #print(sprintf("Skipping subchunk '%s' ...", chunk$id))
    pos <- seek(f, NA)
    seek(f, pos + chunk$size)
  }
  
  md <- list()
  
  while (!is.null(chunk <- read.subchunk())) {
    if (chunk$id != "guan") {
      skip.subchunk(chunk)
      next
    }
    md[["File Path"]] <- normalizePath(filename)
    md[["File Name"]] <- basename(filename)
    md.txt <- readChar(f, chunk$size)
    Encoding(md.txt) <- "UTF-8"  # FIXME: this still isn't setting the encoding to UTF-8
    for (line in strsplit(md.txt, "\n")[[1]]) {
      line <- trimws(line)
      if (line == "") {
        next
      }
      if(verbose == TRUE) print(line)
      toks <- strsplit(sub(":", "\n", line), "\n")
      key <- trimws(toks[[1]][1])
      val <- trimws(toks[[1]][2])
      if (is.na(key) || is.null(key) || key == "") {
        next
      }
      if (!is.null(data.types[[key]])) {
        val <- data.types[[key]](val)
      }
      md[[key]] <- val
    }
    if ("Loc Position" %in% names(md)) {
      coords <- lapply(strsplit(md[["Loc Position"]], " "), as.double)[[1]]
      md[["Loc Position Lat"]] <- coords[1]
      md[["Loc Position Lon"]] <- coords[2]
      md[["Loc Position"]] <- NULL
    }
  }
  
  close(f)
  return(md)
}








#' THE SHINY PART -------------------------------------------------------------------------------

#passed parameters
passed_path_audio <- getShinyOption('path_audio', default = NULL)
passed_path_output <- getShinyOption('path_output', default = NULL)

#constants
credit <- 'App written by Simon Gillings, BTO'

#get the drive letters
volumes <- getVolumes()()

#define the UI
ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs,
  tags$head(
    tags$style(HTML
      ("
      .shiny-notification {
        position: fixed; 
        top: 60% ;
        left: 50%;
      }
      h4 {
        color: #23395D;
      }
      "))
  ),
  
  # Application title
  titlePanel(windowTitle = "BTO Acoustic Pipeline Tools",
             title = div(img(src="APlogo100px.png"), "BTO Acoustic Pipeline Tools (version 1.1)", style="font-size:100px; color: #31566d;")),
  
  tabsetPanel(
    tabPanel("Welcome", fluid = TRUE,
             h5("The BTO Acoustic Pipeline Tools app provides functions to assist with use of the 
                BTO Acoustic Pipeline. This software is provided under the MIT License:"),
             tags$br(),
             h6("Copyright (c) 2022 British Trust for Ornithology"),
             h6('Permission is hereby granted, free of charge, to any person obtaining a copy of 
             this software and associated documentation files (the "Software"), to deal in the 
             Software without restriction, including without limitation the rights to use, copy, 
             modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
             and to permit persons to whom the Software is furnished to do so, subject to the 
             following conditions:'),
             h6('The above copyright notice and this permission notice shall be included in 
             all copies or substantial portions of the Software.'),
             h6('THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
              IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
              FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
              AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
              LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
              OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
              SOFTWARE.'),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             h6('Version 1.1, August 2022'),
             h6(credit)
    ),
    
    # tabPanel("Audit recordings", fluid = TRUE,
    #   sidebarPanel(
    #     h4("Purpose"),
    #     tags$p("Before uploading audio files to the Pipeline it may be worth auditing the 
    #            recordings to check for common problems, such as duplicate filenames."),
    #     tags$br(),
    #     tags$br(),
    #     
    #     h4("Step 1: Select audio folder"),
    #     tags$p("Hint: Only use left side of popup to navigate to folder", style = "color: red;"),
    #     shinyDirButton(id = 'dir_audioaudit', 
    #                    label = 'Select folder', 
    #                    title = 'Select folder containing Batlogger audio files',
    #                    class = "btn-primary"),
    #     verbatimTextOutput("path_audioaudit", placeholder = TRUE),
    #     actionButton("audit_audio", "Audit audio files", class = "btn-success")
    #   ),
    #   mainPanel(
    #     tags$p("Results will go here..."),
    #     textOutput("num_files"),
    #     textOutput("num_filenames")
    #   )
    # ),
    
    tabPanel("Rename Batlogger files", fluid = TRUE,
      sidebarPanel(
       h4("Purpose"),
       tags$p("Batlogger wav files are numbered sequentially so when using one or more detectors 
              it is possible to have multiple files of the same name. Not having uniquely named 
              wav files can be problematic when it comes to auditing the results/recordings. This 
              utility renames Batlogger files, preferentially using GUANO metadata if available 
              (to include latitude, longitude, date and time in the name); if not filenames are pre-pended 
              with a concatenated version of the name of the folder hierarchy of where the wav 
              files are located.  In addition to renaming recordings, a log (csv file) is exported 
              containing any GUANO information that the files contain. We recommend that users 
              with Batloggers do this before uploading recordings to the Pipeline. This utility 
              may be useful for renaming wav files from other makes and models of bat detector, 
              or to export GUANO metadata from wav files."),
       tags$p("Please note, this utility will rename your original audio files. Monitor the warning messages carefully to ensure it is doing what you want.", style = "color: red"),
       tags$br(),
       tags$br(),
       
       h4("Step 1: Select audio folder"),
       tags$p("Select the folder containing the audio files you want to rename."),
       tags$p("Hint: Only use left side of popup to navigate to folder", style = "color: red;"),
       shinyDirButton(id = 'dir_batlogger', 
                      label = 'Select folder', 
                      title = 'Select folder containing Batlogger audio files',
                      class = "btn-primary"),
       verbatimTextOutput("path_batlogger", placeholder = TRUE),
       actionButton("scan_for_audio", "Scan for audio files", class = "btn-success"),
       tags$br(),
       hr(style="border-color: grey;"),
       tags$br(),
       h4("Step 2: Analyse files"),
       tags$p("Analyse the Batlogger files for GUANO metadata, suggest new file names and check for duplication issues."),
       actionButton("analyse_audio", "Analyse audio files", class = "btn-success"),
       tags$br(),
       hr(style="border-color: grey;"),
       tags$br(),
       h4("Step 3: Rename files"),
       tags$p("Warning: Clicking the following button will change audio filenames in the selected folders. Proceed with caution", style = "color: DarkOrange;"),
       actionButton("rename_audio", "Rename audio files", class = "btn-success"),
       tags$br(),
       tags$br(),
       tags$br(),
       actionButton("exit3", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);"),
      ),
      mainPanel(
        tags$div(id = 'batloggertab',
          h4('Batlogger file diagnostics'),       
          textOutput("batlogger_n_files"),
          tags$br(),
          textOutput("batlogger_n_with_guano"),
          tags$br(),
          textOutput("batlogger_n_dupe_names"),
          tags$br(),
          tags$h4('List of detected audio files with proposed new name'),
        ),
        tableOutput('batlogger_table')
      ),
    ),
    
    tabPanel('Organise wavs for auditing', fluid=TRUE,
             
      sidebarPanel(
        h4("Purpose"),
        tags$p("This utility is to save users time when auditing 
                identifications provided by the BTO Acoustic Pipeline. The utility can be 
                configured to copying wav files into folders according to the species 
                identified in each sound file (as detailed in the Pipeline results csv file). 
                Copying methods can be adjusted per species to select identities above certainty 
                thresholds, highest scoring identities per survey location, or in order to return
                a random samples. The last option is useful for very common species and noise
                (No ID) classes and allows the calculation of identification error rates for the 
                random sample: a low error rate from the random sample may justify not checking 
                all clips for common species."),
        tags$br(),
        tags$br(),
        
        h4("Step 1: Import Pipeline Results"),
        tags$p("Select one or more Acoustic Pipeline results csv files. Hold down Ctrl/Command key to select multiple files"),
        shinyFilesButton(id = 'file_csv', 
                       label = 'Select files', 
                       title = 'Select one or more csv files',
                       multiple = TRUE,
                       class = "btn-primary"),
        verbatimTextOutput("file_csv", placeholder = TRUE),
        actionButton("read_results", "Import results", class = "btn-success"),
        tags$br(),
        hr(style="border-color: grey;"),
        tags$br(),
        
        tags$div(id = "step2", 
          h4("Step 2: Analyse and check sample sizes"),
          tags$p("Obtain a list of species detected in this dataset and based on a threshold probability
          set using the slider below, determine the number of audio files of High and Low certainty. We 
          recommend using a threshold of 0.5 but this can be tailored to your situation."),
          sliderInput(
            "pthreshold",
            "Set threshold for low/high certainty:",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.05
          ),
          actionButton("analyse_results", "Analyse recordings", class = "btn-success"),
          tags$br(),
          hr(style="border-color: grey;")
        ),
        
        tags$br(),
        
        tags$div(id = "step3", 
          h4("Step 3: Set copy methods and parameters for sampling"),
          tags$p("Use the drop-down menus in each row of the table to decide on the copy method to use for 
                 that species/identity, then use the sliders below to set the sample sizes for any methods 
                 you will use."),
          sliderInput(
            inputId = "sampsize_tophits",
            label = "Top hits - number of files to copy per location and survey night",
            min = 1,
            max = 10,
            value = 5
          ),
          sliderInput(
            inputId = "sampsize_random",
            label = "Random sample - number of files to randomly sample per species",
            min = 1,
            max = 1000,
            value = 100
          ),
          sliderInput(
            inputId = "sampsize_noid",
            label = "Random sample (No ID class only) - number of files to randomly sample",
            min = 1,
            max = 1000,
            value = 100
          ),
          hr(style="border-color: grey;")
        ),
        
        tags$br(),
        
        tags$div(id = "step4",
          h4("Step 4: Set audio and output folders"),
          tags$p("Finally, select the folder containing your original audio files, and a folder where you want the species folders to be made, and then click Prepare files for copy. Your original audio files will not be modified."),
          tags$p("Hint: Only use left side of popup to navigate to folder", style = "color: red;"),
          shinyDirButton(id = 'dir_audio', 
                         label = 'Select audio folder', 
                         title = 'Select folder containing original audio files',
                         class = "btn-primary"),
          verbatimTextOutput("path_audio", placeholder = TRUE),
          shinyDirButton(id = 'dir_output', 
                         label = 'Select folder for species outputs', 
                         title = 'Select folder where species files to be copied',
                         class = "btn-primary"),
          verbatimTextOutput("path_output", placeholder = TRUE),
          checkboxInput(
            inputId = "append_identity",
            label = 'Append species code to filenames?',
            value = FALSE,
            width = NULL
          ),
          #tags$br(),
          hr(style="border-color: grey;"),
          tags$br(),
        ),
        
        tags$div(id = "step4manual",
                 h4("Step 4: Use preset audio and output folders"),
                 tags$p("Folder containing your original audio files and folder where you want the species folders to be made, as defined by passed options. Now click Prepare files for copy. Your original audio files will not be modified."),
                 verbatimTextOutput("path_audio2", placeholder = TRUE),
                 verbatimTextOutput("path_output2", placeholder = TRUE),
                 checkboxInput(
                   inputId = "append_identity2",
                   label = 'Append species code to filenames?',
                   value = FALSE,
                   width = NULL
                 ),
                 #tags$br(),
                 hr(style="border-color: grey;"),
                 tags$br(),
        ),
        
        
        tags$div(id = "step5",
          h4("Step 5: Prepare, validate and copy the files"),
          tags$p("Prepare the files needing to be copied..."),
          actionButton("prepare_files", "Prepare files for copy", class = "btn-success"),
          tags$p("Check for missing files and other potential issues..."),
          actionButton("validate_files", "Validate files for copy", class = "btn-success"),
          tags$p("Copy the files to the new folders..."),
          actionButton("copy_files", "Copy files", class = "btn-success")
        ),
        tags$br(),
        tags$br(),
        tags$br(),
        actionButton("exit1", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);"),
      ),
      mainPanel(
        tags$div(id = 'diagnostics',
          h4("Diagnostics"),
          textOutput("tempval"),
          textOutput("num_results_files"),
          textOutput("num_detections"),
          textOutput("num_detections_perfile"),
          textOutput("num_noid"),
          textOutput("num_noid_reduced"),
          textOutput("num_detections_final"),
        ),
        tags$br(),
        tags$br(),
        tags$div(id = "summary",
          h4("Species summary"),
          tags$p("Number of detections by species group, species and ID certainty. If
             necessary, adjust the probability threshold using the slider and press 
             Analyse recordings to re-run. Files and folders will be made on the basis 
             of the Copy method selected for each species/identity:"),
          tags$ul(
            tags$li("None: no wav files will be copied for this species"),
            tags$li("Top hits: for each location and survey night, the x wav files with highest probability will be copied"),
            tags$li("High certainty only: all wavs files with probability exceeding the threshold"),
            tags$li("High & Low certainty: all wav files for the species, split into High and Low folders according to threshold probability"),
            tags$li("Random sample: a random sample of x wav files will be created, separately for High and Low certainty recordings")
          )
        ),
        tags$br(),
        tags$br(),
        DT::dataTableOutput('xtab'),
        )
    ),
    tabPanel("Exit", fluid = TRUE,
      tags$br(),
      tags$br(),
      tags$br(),
      actionButton("exit2", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);"),
    )
  )
)

#define the server
server <- function(input, output, session) {
  
  #set up various global defaults  
  global <- reactiveValues(
    audit = NULL,
    batlogger_files = NULL,
    path_audioaudit = NULL,
    path_batlogger = NULL,
    wavs_to_copy = NULL,
    path_audio = passed_path_audio,
    path_output = passed_path_output,
    num_results_files = NULL,
    num_detections = NULL,
    num_detections_perfile = NULL,
    num_noid = NULL,
    num_noid_reduced = NULL,
    num_detections_final = NULL,
    detections = 0,
    xtab = NULL,
    proceed = TRUE,
    batlogger_table = NULL
  )
  
  #visibility/enable start states
  hide(id = 'batloggertab')
  hide(id = 'diagnostics')
  hide(id = 'summary')
  hide(id = 'step2')
  hide(id = 'step3')
  hide(id = 'step4')
  hide(id = 'step4manual')
  hide(id = 'step5')
  hide(id = 'save_log')
  hide(id = 'save_guano')
  disable(id = 'validate_files')
  disable(id = 'copy_files')
  disable(id = 'analyse_audio')
  disable(id = 'rename_audio')

  
  #select the folders
  shinyDirChoose(input,'dir_audioaudit', roots = volumes, session = session, filetypes = c(''))
  shinyDirChoose(input,'dir_batlogger', roots = volumes, session = session, filetypes = c(''))
  shinyFileChoose(input, 'file_csv', roots = volumes, session = session, filetypes=c('csv'))
  shinyDirChoose(input,'dir_audio', roots = volumes, session = session, filetypes = c(''))
  shinyDirChoose(input,'dir_output', roots = volumes, session = session, filetypes = c(''))
  
  #event handlers
  #file and directory event handlers
  observeEvent(eventExpr = {input$dir_audioaudit}, handlerExpr = {global$path_audioaudit <- parseDirPath(volumes, input$dir_audioaudit)} )
  observeEvent(eventExpr = {input$dir_batlogger}, handlerExpr = {global$path_batlogger <- parseDirPath(volumes, input$dir_batlogger)} )
  observeEvent(eventExpr = {input$file_csv}, handlerExpr = {
    file_csv1 <- parseFilePaths(volumes, input$file_csv)
    global$file_csv <- file_csv1$datapath
  } )
  observeEvent(eventExpr = {input$dir_audio}, handlerExpr = {global$path_audio <- parseDirPath(volumes, input$dir_audio)} )
  observeEvent(eventExpr = {input$dir_output}, handlerExpr = {
    global$path_output <- parseDirPath(volumes, input$dir_output)
    if(!is.null(global$path_audio) & !is.null(global$path_output)) show(id = "step5")
  } )

  
  
  #event handler for audit audio
  observeEvent(input$audit_audio, {
    global$audit <- audit_audio(path = global$path_audio)
  })
  
  #event handler for listing the batlogger files
  observeEvent(input$scan_for_audio, {
    global$batlogger_files <- list_batlogger_files(path_to_process = global$path_batlogger)
    enable(id = 'analyse_audio')
  })

  #event handler for analysing batlogger files
  observeEvent(input$analyse_audio, {
    temp <- analyse_batlogger_files(path_to_process = global$path_batlogger, files_old = global$batlogger_files)
    #unpack
    global$batlogger_n_files <- temp$n_files
    global$batlogger_n_dupe_names <- temp$n_dupes
    global$batlogger_n_with_guano <- temp$n_with_guano
    global$batlogger_log <- temp$batlogger_log

    #format table for plotting and toggle on visibility of the div
    global$batlogger_table <- temp$batlogger_log[,c('name_original', 'name_proposed', 'warning')]
    names(global$batlogger_table) <- c('Original file name', 'Proposed file name', 'Warning')
    show(id = 'batloggertab')
    
    #if there are no duplicates, enable the rename button
    if(global$batlogger_n_dupe_names == 0) enable(id = 'rename_audio')
  })
  

  
  #event handler for renaming the batlogger files
  observeEvent(input$rename_audio, {
    rename_batlogger_files(path_to_process = global$path_batlogger, batlogger_log = global$batlogger_log)
    #once done, disable button to prevent repress
    disable(id = 'rename_audio')
  })
  
  #event handler for getting the files
  observeEvent(input$read_results, {
    temp <- get_results(files = global$file_csv)
    global$detections <- temp$detections
    global$num_results_files <- temp$metrics$num_files
    global$num_detections <- temp$metrics$num_detections
    global$num_detections_perfile <- temp$metrics$num_detections_after_aggregation
    global$num_noid <- temp$metrics$num_detections_noid
    global$num_noid_reduced <- temp$metrics$num_detections_noid_after_check
    global$num_detections_final <- temp$metrics$num_detections_after_noid_check
    show(id = 'diagnostics')
    show(id = 'step2')
    print(global$path_audio)
    print(global$path_output)
  })

  #event handler for analysing the results - key part here is rendering a datatable 
  #with a dropdown menu in each row presenting the copy methods
  observeEvent(input$analyse_results, {
    dat <- global$detections
    p <- input$pthreshold
    
    #label up whether high or low according to preferred threshold
    dat$certainty <- ifelse(dat$probability >= p, 'High', 'Low')
    
    #mark No ID as High  
    dat$certainty <- ifelse(dat$species == 'No ID', 'High', dat$certainty)
    
    #now get number of identities per species  
    dat$f <- 1
    xtab <- aggregate(data = dat, f ~ species.group + english.name + species + certainty, NROW)
    xtab <- pivot_wider(data = xtab, 
                        id_cols = c(species.group, english.name, species), 
                        names_from = certainty, 
                        values_from = f, values_fill = list(f = 0))
    #enforce having both Low and High columns
    if(!'Low' %in% names(xtab)) xtab$Low <- 0
    if(!'High' %in% names(xtab)) xtab$High <- 0
    xtab <- xtab[order(xtab$species.group, xtab$species),c('species.group', 'english.name', 'species', 'Low', 'High')]
    
    xtab$method <- NA
    
    #set defaults for copy method
    xtab$method_default <- 'High certainty only'
    xtab$method_default <- ifelse(xtab$species == 'No ID', "None", xtab$method_default)
    xtab$method_default <- ifelse(xtab$species == 'Pippip', "Random sample", xtab$method_default)
    xtab$method_default <- ifelse(xtab$species.group == 'bush-cricket', "Top hits", xtab$method_default)
    xtab$method_default <- ifelse(xtab$species.group == 'moth', "Top hits", xtab$method_default)
    
    #rename columns for printing
    names(xtab) <- c('Group','Species', 'Species Code', 'Low', 'High', 'Copy Method', 'method_default')
    
    #loop over species and make the copy method dropdown
    for (i in 1:nrow(xtab)) {
      xtab$`Copy Method`[i] <- as.character(selectInput(paste(xtab$`Species Code`[i]),
                                                        "",
                                                        choices = c("None", "Top hits", "High certainty only", "High & Low certainty", "Random sample"),
                                                        selected = xtab$method_default[i]
                                                         )
                                               )
    }
    #only retain the columns needed for the table
    global$xtab <- xtab[,c('Group','Species', 'Species Code', 'Low', 'High', 'Copy Method')]
    
    #toggle states
    show(id = 'summary')
    show(id = 'step3')
    #if paths already set from options, check they exist then show step4manual and step5
    if(!is.null(global$path_audio) & !is.null(global$path_output)) {
      if(!dir.exists(global$path_audio)) stop('path_audio does not exist')
      if(!dir.exists(global$path_output)) stop('path_output does not exist')
      show(id = "step4manual")
      show(id = "step5")
    }
    #else just show the normal step4 for entry
    if(is.null(global$path_audio) & is.null(global$path_output)) show(id = 'step4')
  })

  # event handlers for whether to append species names when renaming files. Two versions for watching the two places this can be selected
  observeEvent(input$append_identity, {
    global$append_identity <- input$append_identity
    #print(global$append_identity)
  })
  observeEvent(input$append_identity2, {
    global$append_identity <- input$append_identity2
    #print(global$append_identity)
  })
  
  
  # event handler for preparing the list of files and destination directories
  observeEvent(input$prepare_files, {
    global$wavs_to_copy <- prep_copy_files(dat = global$detections,
                                           input = input,
                                           p = input$pthreshold,
                                           sampsize_noid = input$sampsize_noid,
                                           sampsize_random = input$sampsize_random,
                                           sampsize_tophits = input$sampsize_tophits,
                                           path_audio = global$path_audio,
                                           path_output = global$path_output)
    if(nrow(global$wavs_to_copy) > 0) {
      shinyalert(title = "File list prepared", 
                 text = paste("Now validate to check for problems, e.g. missing files"),
                 type = "success")    
      #enable the validate button now
      enable(id = 'validate_files')
    }
  })
  

  #event handler for the validate files button. Check for missing audio files - if 
  #none, activate copy button. If missing files, write log and ask user if they want 
  #to proceed, if so, activate copy button
  observeEvent(input$validate_files, {
    #check for missing audio files - check for rows where filepath is not populated, 
    #indicates original wav is not found
    missing_files <- subset(global$wavs_to_copy, is.na(filepath))
    if(nrow(missing_files)>0) {
      shinyalert(title = "Missing audio files!", 
                 text = paste("There are audio files listed in results.csv that are missing from the audio folder! A csv file listing these missing files will be saved to", global$path_output, "Do you want to continue with the copying?"),
                 type = "warning",
                 showCancelButton = TRUE,
                 callbackR = function(x) { if(x == TRUE) enable(id = 'copy_files') })
      write.csv(missing_files, file.path(global$path_output, 'missing_audio_files.csv'), row.names = FALSE)
    }
    #if no missing files, activate Copy button
    if(nrow(missing_files) == 0) {
      shinyalert(title = "File list validated", 
                 text = paste("You can now copy the files to species folders"),
                 type = "success",
                 showCancelButton = TRUE,
                 callbackR = function(x) { if(x == TRUE) enable(id = 'copy_files') })
    }
  })
  
  
  #event handler for the copy files button. Button state is hidden by default and only 
  #visible once validation approved. Return to hidden state once copied to prevent repeating. And clear path vars to reset state.
  observeEvent(input$copy_files, {
    copy_files(wavs_to_copy = global$wavs_to_copy, path_output = global$path_output, append_identity = global$append_identity)
    
    shinyalert(title = "Finished", 
               text = "Finished copying files to species folders", 
               type = "success",
               callbackR = function(x) { if(x == TRUE) disable(id = 'copy_files') })
    #cat(file=stderr(), 'Checkpoint12 - finished copying audio files\n')
    global$path_audio <- NULL
    global$path_output <- NULL
  })

  #event handler for exit buttons
  observeEvent(input$exit1, { stopApp() })
  observeEvent(input$exit2, { stopApp() })
  observeEvent(input$exit3, { stopApp() })

  #outputs
  output$batlogger_n_files <- renderText( { paste("Number of batlogger audio files =", global$batlogger_n_files)})
  output$batlogger_n_dupe_names <- renderText( { paste("Number of files with duplicate proposed names =", global$batlogger_n_dupe_names)})
  output$batlogger_n_with_guano <- renderText( { paste("Number of files with embedded GUANO metadata =", global$batlogger_n_with_guano)})
  output$num_files <- renderText( { paste("Number of audio files =", global$audit$num_files)})
  output$num_filenames <- renderText( { paste("Number of unique filenames", global$audit$num_unique_filenames)})
  output$path_batlogger <- renderText({ global$path_batlogger })
  output$path_audioaudit <- renderText({ global$path_audioaudit })
  output$file_csv <- renderText({ paste(unlist(global$file_csv), collapse = '\n') })
  output$path_audio <- output$path_audio2 <- renderText({ global$path_audio })
  output$path_output <- output$path_output2 <- renderText({ global$path_output })
  output$nrecs <- renderText({ nrow(global$detections) })
  output$num_results_files <- renderText( { paste("Number of results files read:", global$num_results_files)})
  output$num_detections <- renderText( { paste("Initial number of records:", global$num_detections)})
  output$num_detections_perfile <- renderText( { paste("Number of records after deduping per species/file:", global$num_detections_perfile)})
  output$num_noid <- renderText( { paste("Initial number of 'No ID' records:", global$num_noid)})
  output$num_noid_reduced <- renderText( { paste("Remaining number of 'No ID' records after removing for files with positive IDs:", global$num_noid_reduced)})
  output$num_detections_final <- renderText( { paste("Final number of records:", global$num_detections_final)})
  
  #from https://community.rstudio.com/t/update-dt-table-dropdowns-with-reactive-data-in-shiny/96100/2
  #the callback is essential to capture the inputs in each row
  output$xtab = DT::renderDataTable(
    global$xtab, escape = FALSE, selection = 'none', server = FALSE, rownames= FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE),
    callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  
  output$batlogger_table = renderTable(global$batlogger_table)
}

# Run the application
shinyApp(ui = ui, server = server)