#' Audit audio folders
#' 
#' @description Scan and audit the files in the required folder. Check for corrupt files, 
#' bad filenames, duplicates, etc. See if these can be rectified with either GUANO or XML
#' content. If so provide a recommended new name.
#' 
#' @param path_to_process = path to audio folder
#' @param files = list of wav files to audit
#'
#' @return A list containing various diagnostic summary stats, plus a dataframe 
#' with diagnostics for every file in the batch.
#' 
audit_audio <- function(path_to_process, files) {

  #container for results of iteration over files
  outs <- list()
  
  #for each file check...
  withProgress(message = "Analysing audio files...", min = 0, max = length(files), {
    for(f in 1:length(files)) {
      this_wav <- files[f]
      this_dirname <- dirname(this_wav)
      this_file <- basename(this_wav)
      
      #check if the file can be read
      file_corrupt <- tryCatch({
        # Attempt to read the file
        data <- tuneR::readWave(this_wav, header=TRUE)
        0
      }, error = function(e) {
        # Handle the error
        1
      })
      
      #check filename compatibility and datetime using various filename templates
      #if filename good, extract dt_filename
      filename_check <- check_filename_compliance(this_file)
      filename_bad <- ifelse(filename_check$good, 0, 1)
      dt_filename <- filename_check$dt_filename

      #check for guano (if the file is not corrupt)
      if(file_corrupt==0) this_guano <- read_guano(this_wav)
      if(file_corrupt==1) this_guano <- list()
      has_guano <- FALSE
      dt_guano <- NA
      lat_guano <- NA
      lon_guano <- NA
      #if GUANO extracted...
      if(length(this_guano) != 0) {
        has_guano <- TRUE
        #check if key variables are present - if columns are missing, add them with NAs
        if(!"Timestamp" %in% names(this_guano)) this_guano$`Timestamp` <- NA
        if(!"Loc Position Lat" %in% names(this_guano)) this_guano$`Loc Position Lat` <- NA
        if(!"Loc Position Lon" %in% names(this_guano)) this_guano$`Loc Position Lon` <- NA
        
        #save the guano
        #files_guano[[f]] <- as.data.frame(guano, stringsAsFactors = FALSE)
        
        #and if all relevant fields populated, use GUANO to rename file
        if(!is.na(this_guano$`Timestamp`)) {
          #get and format datetime
          dt_guano <- gsub(pattern = "-|:", "", as.character(this_guano$`Timestamp`))
          dt_guano <- gsub(pattern = " ", "_", dt_guano)
        }
        if(!is.na(this_guano$`Loc Position Lat`)){
          lat_guano <- round(as.numeric(this_guano$`Loc Position Lat`),4)
        }
        if(!is.na(this_guano$`Loc Position Lon`)) {
          lon_guano <- round(as.numeric(this_guano$`Loc Position Lon`),4)
        }
      }   
      
      #check for XML
      #is there an XML file?
      datetime_value <- NA
      position_value <- NA
      dt <- NA
      lat_xml <- NA
      lon_xml <- NA
      dt_xml <- NA
      file_xml <- gsub(".wav|.WAV", ".xml", this_wav)
      if(!file.exists(file_xml)) {
        has_xml <- FALSE
      }
      #if XML exists, read it and find the datetime value
      if(file.exists(file_xml)) {
        has_xml <- TRUE
        doc <- read_xml(file_xml)
        datetime_value <- xml_text(xml_find_first(doc, "//DateTime"))
        position_value <- xml_text(xml_find_first(doc, "//Position"))
        if(!is.na(datetime_value)) {
          dt <- strptime(datetime_value, format = "%d.%m.%Y %H:%M:%S")
          dt_xml <- format(dt, "%Y%m%d_%H%M%S")
        }
        if(!is.na(position_value)) {
          lat_xml <- round(as.numeric(stringr::str_split_fixed(position_value, " ", Inf)[1]), 4)
          lon_xml <- round(as.numeric(stringr::str_split_fixed(position_value, " ", Inf)[2]), 4)
        }
      }
      
      #process the date time info
      #if multiple sources present, do datetimes agree?
      dt2use <- NA
      dt_mismatch <- 0
      dt_mismatch <- ifelse(!is.na(dt_guano) & !is.na(dt_xml) & dt_guano != dt_xml, 1, dt_mismatch)
      dt_mismatch <- ifelse(!is.na(dt_guano) & !is.na(dt_filename) & dt_guano != dt_filename, 1, dt_mismatch)
      dt_mismatch <- ifelse(!is.na(dt_xml) & !is.na(dt_filename) & dt_xml != dt_filename, 1, dt_mismatch)
      #if dates don't mismatch use datetimes in order filename > guano > xml
      if(is.na(dt2use) & dt_mismatch == 0 & !is.na(dt_filename)) dt2use <- dt_filename
      #if dates don't mismatch and guano populated, use guano    
      if(is.na(dt2use) & dt_mismatch == 0 & !is.na(dt_guano)) dt2use <- dt_guano
      #if dates don't mismatch and XML populated, use XML
      if(is.na(dt2use) & dt_mismatch == 0 & !is.na(dt_xml)) dt2use <- dt_xml
  
      #process the lat-long info
      #if both present, do GUANO and XML agree?
      lat2use <- NA
      lon2use <- NA
      lat_mismatch <- 0
      lon_mismatch <- 0
      lat_mismatch <- ifelse(!is.na(lat_guano) & !is.na(lat_xml) & lat_guano != lat_xml, 1, lat_mismatch)
      lon_mismatch <- ifelse(!is.na(lon_guano) & !is.na(lon_xml) & lon_guano != lon_xml, 1, lon_mismatch)
      #if lats don't mismatch and guano populated, use guano    
      if(lat_mismatch == 0 & !is.na(lat_guano)) lat2use <- lat_guano
      if(lat_mismatch == 0 & !is.na(lat_xml)) lat2use <- lat_xml
      #if lons don't mismatch and XML populated, use XML
      if(lon_mismatch == 0 & !is.na(lon_guano)) lon2use <- lon_guano
      if(lon_mismatch == 0 & !is.na(lon_xml)) lon2use <- lon_xml
      
      #can the file be processed without change?
      #ie does it have a safe filename and/or a guano date and/or xml date?
      acceptable <- ifelse(filename_bad == 0 | !is.na(dt_guano) | !is.na(dt_xml), 1, 0)
      
      #can it be renamed - only if filename bad and has datetime from guano or xml?
      renamable <- ifelse(filename_bad == 1 & !is.na(dt2use), 1, 0)

      #can the file not be renamed - only relevant for files that are badly named?
      unrenamable <- ifelse(filename_bad == 1 & is.na(dt2use), 1, 0)
      
      #if the file can be renamed based on data, create newname
      newname <- NA
      if(renamable==1) {
        #if lat and lon both available, use them
        if(!is.na(lat2use) & !is.na(lon2use)) {
          #convert lat-long to character and replace decimal point with ~ as required for some batviewer apps
          lat <- gsub("\\.", "~", as.character(lat2use))
          lon <- gsub("\\.", "~", as.character(lon2use))
          #compile new filename in Pipeline format
          newname <- file.path(this_dirname, trimws(paste0(lat, "+", lon, "_", dt2use, ".wav")))
        }
        #if either lat or long unavailable use datetime renaming with old filename suffix
        if(is.na(lat2use) | is.na(lon2use)) {
          newname <- file.path(this_dirname, trimws(paste0(dt2use,"_old_",this_file)))
        }
      }
  
      outs[[f]] <- data.frame(original_name = this_wav, 
                              directory = this_dirname, 
                              file = this_file,
                              file_corrupt = file_corrupt,
                              acceptable = acceptable, 
                              filename_bad,
                              dt_filename,
                              has_guano, 
                              dt_guano, 
                              lat_guano, 
                              lon_guano,
                              has_xml, 
                              dt_xml,
                              lat_xml,
                              lon_xml,
                              dt_mismatch,
                              lat_mismatch,
                              lon_mismatch,
                              renamable, 
                              new_name = newname, 
                              unrenamable)
      rm(list=c('this_wav', 'this_file', 'this_dirname', 'file_corrupt', 'filename_bad', 'acceptable', 
                'has_guano', 'dt_guano', 'lat_guano', 'lon_guano', 
                'has_xml', 'dt_xml', 'lat_xml', 'lon_xml',
                'dt_mismatch', 'lat_mismatch', 'lon_mismatch',
                'dt2use', 'lat2use', 'lon2use',
                'renamable', 'newname', 'dt', 'this_guano', 'datetime_value', 'position_value', 'unrenamable'))
      
    } #end file iteration loop
  })  #end progress messaging
    
    
  #unpack
  file_data <- do.call(rbind, outs)
  file_data$nfiles <- 1 #dummy var to count fils
  
  #which files are duplicates
  file_data$oldname_duplicated <- ifelse( duplicated(file_data$file, incomparables = c(NA)) | duplicated(file_data$file, fromLast = TRUE, incomparables = c(NA)), TRUE, FALSE)
  file_data$newname_duplicated <- ifelse( duplicated(file_data$new_name, incomparables = c(NA)) | duplicated(file_data$new_name, fromLast = TRUE, incomparables = c(NA)), TRUE, FALSE)

  #get summary stats for each folder
  summary_per_dir <- aggregate(data = file_data, cbind(nfiles, file_corrupt, acceptable, filename_bad, has_guano, has_xml, renamable, unrenamable) ~ directory, sum)
  
  #headline stats
  n_files <- length(files)
  n_dirs <- nrow(summary_per_dir)
  n_files_acceptable <- sum(file_data$acceptable)                     #how many can process now (good name or GUANO)
  n_files_corrupt <- sum(summary_per_dir$file_corrupt)                #how many files are corrupt
  n_files_good_names <- length(files) - sum(file_data$filename_bad)   #how many files have good names
  n_files_bad_names <- sum(file_data$filename_bad)                    #how many files have bad names
  n_duplicated_oldnames <- sum(file_data$oldname_duplicated)          #how many of the original filenames are duplicates?
  n_duplicated_newnames <- sum(file_data$newname_duplicated)          #how many of the new filenames will be duplicates?
  n_cannot_rename <- sum(file_data$unrenamable)                       #how many can't be renamed?
  
  
  output <- list(n_files = n_files,
                 n_dirs = n_dirs,
                 n_files_acceptable = n_files_acceptable,
                 n_files_corrupt = n_files_corrupt,
                 n_files_good_names = n_files_good_names,
                 n_files_bad_names = n_files_bad_names,
                 n_cannot_rename = n_cannot_rename,
                 n_duplicated_oldnames = n_duplicated_oldnames,
                 n_duplicated_newnames = n_duplicated_newnames,
                 #data
                 summary_per_dir = summary_per_dir,
                 file_data = file_data)
  return(output)
}
