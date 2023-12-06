#' Audit audio folders
#' 
#' @description Scan the files in the required folder. Check for problem file names and see if these can be rectified
#' with either GUANO or XML content. If so provide a recommended new name.
#' 
#' @param path = path to audio folder
#'
audit_audio <- function(path_to_process, files_old) {
  #container for results of iteration over files
  outs <- list()
  
  #for each file check...
  for(f in 1:length(files_old)) {
    this_wav <- files_old[f]
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
    
    #check filename compatibility using various filename templates
    pattern_underscore <- "\\d{8}_\\d{6}"
    pattern_hyphen <- "\\d{8}-\\d{6}"
    pattern_peersonic <- "\\d{4}_\\d{2}_\\d{2}__\\d{2}_\\d{2}_\\d{2}"
    pattern_petersson <- "\\d{4}-\\d{2}-\\d{2}_\\d{2}_\\d{2}_\\d{2}"
    patterns <- paste(c(pattern_underscore, pattern_hyphen, pattern_peersonic, pattern_petersson), collapse = '|')
    # audiomoth <- '20120812_091406.wav'
    # wildlifeacoustics <- 'SM4A123_20120812_091406.wav'
    # petersson <- 'ABC2010-08-26_10_39_50_M00667DEF.wav'
    # peersonic <- 'Wav0123_2008_07_04__22_58_14.wav'
    # grepl(pattern_underscore, audiomoth)
    # grepl(pattern_underscore, wildlifeacoustics)
    # grepl(pattern_peersonic, peersonic)
    # grepl(pattern_petersson, petersson)
     
    filename_fail <- 1
    if(grepl(patterns, this_file)) {
      filename_fail <- 0
    }
    
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
        lat_guano <- this_guano$`Loc Position Lat`
      }
      if(!is.na(this_guano$`Loc Position Lon`)) {
        lon_guano <- this_guano$`Loc Position Lon`
      }
    }   
    
    #check for XML
    #is there an XML file?
    datetime_value <- NA
    dt <- NA
    file_xml <- gsub(".wav", ".xml", this_wav)
    if(!file.exists(file_xml)) {
      has_xml <- FALSE
      dt_xml <- NA
      #lat_xml <- NA
      #lon_xml <- NA
    }
    #if XML exists, read it and find the datetime value
    if(file.exists(file_xml)) {
      has_xml <- TRUE
      doc <- read_xml(file_xml)
      datetime_value <- xml_text(xml_find_first(doc, "//DateTime"))
      if(!is.na(datetime_value)) {
        dt <- strptime(datetime_value, format = "%d.%m.%Y %H:%M:%S")
        dt_xml <- format(dt, "%Y%m%d_%H%M%S")
      }
      #lat_xml <- NA
      #lon_xml <- NA
    }
    
    #if both present, do GUANO and XML agree?
    dt_mismatch <- 0
    dt_mismatch <- ifelse(!is.na(dt_guano) & !is.na(dt_xml) & dt_guano != dt_xml, 1, dt_mismatch)

    #can the file be processed?
    #ie does it have a safe filename and/or a guano date?
    success <- ifelse(filename_fail == 0 | !is.na(dt_guano), 1, 0)
    
    
    
    #can it be renamed - does it have dt from guano or xml?
    renamable <- 0
    renamable <- ifelse(!is.na(dt_guano) | !is.na(dt_xml), 1, 0)
    renamable <- ifelse(dt_mismatch == 1, 0, renamable)

    #can the file not be renamed?
    unrenamable <- 1-renamable
    
    #if the file needs renaming, and can be renamed based on data, create newname
    newname <- NA
    dt2use <- NA
    if(renamable==1) {
      dt2use <- ifelse(!is.na(dt_guano), dt_guano, dt_xml)
      newname <- file.path(this_dirname, paste0(dt2use,"_old_",this_file))
    }
    
    
    
    outs[[f]] <- data.frame(original_name = this_wav, 
                            directory = this_dirname, 
                            file_corrupt = file_corrupt,
                            success = success, 
                            filename_fail,
                            has_guano, 
                            dt_guano, 
                            lat_guano, 
                            lon_guano,
                            has_xml, 
                            dt_xml,
                            dt_mismatch, 
                            renamable, 
                            new_name = newname, 
                            unrenamable)
    rm(list=c('this_wav', 'this_dirname', 'file_corrupt', 'filename_fail', 'success', 'has_guano', 'dt_guano', 'lat_guano', 'lon_guano', 
              'has_xml', 'dt_xml', 'dt_mismatch', 'renamable', 'newname', 'dt', 'this_guano', 'datetime_value', 'dt2use', 'unrenamable'))
    
  } #end file iteration loop
  #unpack
  file_data <- do.call(rbind, outs)
  file_data$nfiles <- 1 #dummy var to count fils
  
  #get summary stats for each folder
  summary_per_dir <- aggregate(data = file_data, cbind(nfiles, file_corrupt, filename_fail, success, has_guano, has_xml, renamable, unrenamable) ~ directory, sum)
  
  #headline stats
  n_files <- length(files_old)
  n_dirs <- nrow(summary_per_dir)
  n_files_corrupt <- sum(summary_per_dir$file_corrupt)
  
  #how many of the original files can be processed immediately?
  n_ok_to_process <- sum(file_data$success)
  
  #are they all OK?
  all_ok <- ifelse(n_ok_to_process == n_files, TRUE, FALSE)
    
  #how many of the original filenames are duplicates?
  n_duplicated_oldnames <- sum(duplicated(basename(files_old)))
  
  #how many of the new filenames will be duplicates?
  n_duplicated_newnames <- sum(duplicated(file_data$new_name))
  
  #how many need renaming?
  n_need_renaming <- n_files - n_ok_to_process
  
  #how many can't be renamed?
  n_cannot_rename <- sum(file_data$unrenamable)
  
  #allow renaming only if: all filenames fail, no files that can't be renamed, no duplicates will be generated
  all_filename_fail <- ifelse(sum(file_data$filename_fail) == n_files,1,0)
  allow_rename <- ifelse(all_filename_fail == 1 & n_cannot_rename == 0 & n_duplicated_newnames == 0, TRUE, FALSE)
  
  output <- list(n_files = n_files,
                 n_dirs = n_dirs,
                 n_files_corrupt = n_files_corrupt,
                 all_ok = all_ok,
                 all_filename_fail = all_filename_fail,
                 allow_rename = allow_rename,
                 n_ok_to_process = n_ok_to_process,
                 n_need_renaming = n_need_renaming,
                 n_cannot_rename = n_cannot_rename,
                 n_duplicated_oldnames = n_duplicated_oldnames,
                 n_duplicated_newnames = n_duplicated_newnames,
                 summary_per_dir = summary_per_dir,
                 file_data = file_data)
  return(output)
}
