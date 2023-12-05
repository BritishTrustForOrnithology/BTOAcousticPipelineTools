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
  
  #standardise some names
  names(dat)[which(names(dat)=='score')] <- 'probability'
  
  #which format of file is this?
  type <- NA
  if(length(names(dat)) == 13) {
    type <- 'offline'
  }
  if(length(names(dat)) == 19) {
    type <- 'old_bat'
  }  
  if(length(names(dat)) == 20) {
    type <- ifelse(substr(dat$classifier.name[1],1,7)=='Classif', 'new_bat', 'new_bird')
  }
  
  #standardise the different input formats for downstream use
  #offline file format
  if(type == 'offline') {
    cat(file=stderr(), "Trying OFFLINE format file\n")
    expnames <- "upload_directory,filename,prefix,species,probability,actual_date,session_date,time,scientific_name,english_name,group,warnings,classifier_code"
    if(expnames != paste(names(dat), collapse=',')) {
      msg <- paste0('Unexpected OFFLINE file format. \nExpected: ',expnames,'\nGot: ', paste(names(dat), collapse=','))
      stop(msg)
    }
    names(dat)[which(names(dat)=='filename')] <- 'file2move'
    names(dat)[which(names(dat)=='group')] <- 'species.group'
    names(dat)[which(names(dat)=='session_date')] <- 'survey.date'
    names(dat)[which(names(dat)=='scientific_name')] <- 'scientific.name'
    names(dat)[which(names(dat)=='english_name')] <- 'english.name'
    names(dat) <- gsub("_", ".", names(dat))
    dat$location <- dat$prefix
    #drop redundant columns
    dat <- subset(dat, select=c('file2move', 'location', 'survey.date','species', 'scientific.name', 'english.name', 'species.group', 'probability'))
    cat(file=stderr(), "Success reading OFFLINE format file\n")
  }
  
  #old online bat version pre-release of bird app (October 2023)
  if(type == 'old_bat') {
    cat(file=stderr(), "Trying OLD BAT format file\n")
    expnames <- "recording.file.name,original.file.name,original.file.part,latitude,longitude,species,scientific.name,english.name,species.group,probability,warnings,actual.date,survey.date,time,classifier.name,user.id,upload.key,upload.name,survey.name"
    if(expnames != paste(names(dat), collapse=',')) {
      msg <- paste0('Unexpected OLD BAT file format. \nExpected: ',expnames,'\nGot: ', paste(names(dat), collapse=','))
      stop(msg)
    }
    names(dat)[which(names(dat)=='original.file.name')] <- 'file2move'
    dat$location <- paste(dat$latitude, dat$longitude, sep='~')
    #drop redundant columns
    dat <- subset(dat, select=c('file2move', 'location', 'survey.date','species', 'scientific.name', 'english.name', 'species.group', 'probability'))
    cat(file=stderr(), "Success reading OLD BAT format file\n")
  }
  
  #new bat version
  if(type == 'new_bat') {
    cat(file=stderr(), "Trying ULTRASONIC format file\n")
    expnames <- "recording.file.name,original.file.name,original.file.part,latitude,longitude,species,scientific.name,english.name,species.group,probability,warnings,call.type,actual.date,survey.date,time,classifier.name,user.id,upload.key,batch.name,project.name"
    if(expnames != paste(names(dat), collapse=',')) {
      msg <- paste0('Unexpected ULTRASONIC file format. \nExpected: ',expnames,'\nGot: ', paste(names(dat), collapse=','))
      stop(msg)
    }
    names(dat)[which(names(dat)=='original.file.name')] <- 'file2move'
    dat$location <- paste(dat$latitude, dat$longitude, sep='~')
    dat$english.name <- ifelse(!is.na(dat$call.type), paste0(dat$english.name, " (", dat$call.type, ")"), dat$english.name)
    #drop redundant columns
    dat <- subset(dat, select=c('file2move', 'location', 'survey.date','species', 'scientific.name', 'english.name', 'species.group', 'probability'))
    cat(file=stderr(), "Success reading ULTRASONIC format file\n")
  }
  #new bird version
  if(type == 'new_bird') {
    cat(file=stderr(), "Trying AUDIBLE format file\n")
    expnames <- "recording.file.name,original.file.name,original.file.part,latitude,longitude,species,scientific.name,english.name,species.group,probability,warnings,call.type,actual.date,survey.date,time,classifier.name,user.id,upload.key,batch.name,project.name"
    if(expnames != paste(names(dat), collapse=',')) {
      msg <- paste0('Unexpected AUDIBLE file format. \nExpected: ',expnames,'\nGot: ', paste(names(dat), collapse=','))
      stop(msg)
    }
    names(dat)[which(names(dat)=='recording.file.name')] <- 'file2move'
    dat$file2move <- paste0(dat$file2move, '.wav')
    dat$location <- paste(dat$latitude, dat$longitude, sep='~')
    dat$english.name <- ifelse(!is.na(dat$call.type), paste0(dat$english.name, " (", dat$call.type, ")"), dat$english.name)
    #drop redundant columns
    dat <- subset(dat, select=c('file2move', 'location', 'survey.date','species', 'scientific.name', 'english.name', 'species.group', 'probability'))
    cat(file=stderr(), "Success reading AUDIBLE format file\n")
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
