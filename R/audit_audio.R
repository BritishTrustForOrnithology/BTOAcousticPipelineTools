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
    
    #check filename compatibility
    filename_fail <- 1
    #YYYYMMDD_HHMMSS.wav format or #YYYYMMDD-HHMMSS.wav format
    if(grepl("\\d{8}_\\d{6}", this_file) | grepl("\\d{8}-\\d{6}", this_file)) {
      filename_fail <- 0
    }
    
    #check for guano
    this_guano <- read_guano(this_wav)
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

    #can it be renamed - does it have dt from guano or xml?
    renamable <- 0
    renamable <- ifelse(!is.na(dt_guano) | !is.na(dt_xml), 1, 0)
    renamable <- ifelse(dt_mismatch == 1, 0, renamable)

    
    #if the file needs renaming, and can be renamed based on data, create newname
    newname <- NA
    if(renamable==1) {
      dt2use <- ifelse(!is.na(dt_guano), dt_guano, dt_xml)
      newname <- file.path(this_dirname, paste0(dt2use,"_",this_file))
    }
    
    unrenamable <- 1-renamable
    
    
    outs[[f]] <- data.frame(wav = this_wav, dir = this_dirname, filename_fail,
                      has_guano, dt_guano, lat_guano, lon_guano, 
                      has_xml, dt_xml, 
                      dt_mismatch, renamable, newname, unrenamable)
    rm(list=c('this_wav', 'this_dirname', 'filename_fail', 'has_guano', 'dt_guano', 'lat_guano', 'lon_guano', 
              'has_xml', 'dt_xml', 'dt_agree', 'renamable', 'newname', 'dt', 'this_guano', 'datetime_value', 'dt2use', 'unrenamable'))
    
  } #end file iteration loop
  #unpack
  file_data <- do.call(rbind, outs)
  
  #get summary stats for each folder
  summary_per_dir <- aggregate(data = file_data, cbind(filename_fail, has_guano, has_xml, renamable, unrenamable) ~ dir, sum)
  
  #headline stats
  num_files <- length(files_old)
  
  #how many of the original filenames will be duplicates?
  n_duplicated_oldnames <- sum(duplicated(basename(files_old)))
  
  #how many of the new filenames will be duplicates?
  n_duplicated_newnames <- sum(duplicated(file_data$newname))
  
  output <- list(num_files = num_files, 
                 n_duplicated_oldnames = n_duplicated_oldnames,
                 n_duplicated_newnames = n_duplicated_newnames,
                 summary_per_dir = summary_per_dir,
                 file_data = file_data)
  return(output)
}
