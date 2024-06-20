#passed parameters
passed_path_audio <- getShinyOption('path_audio', default = NULL)
passed_path_output <- getShinyOption('path_output', default = NULL)

#constants
credit <- 'App written by Simon Gillings, BTO'
version <- read.csv(file.path(dirname(path_to_app),'version.txt'), colClasses = 'character')

#get the drive letters
volumes <- shinyFiles::getVolumes()()

#' Convert ISO 8601 timestamps into strings - IGNORE TIMEZONE FOR THIS PURPOSE
#' Modified from the GUANO metadata package for R, 
#' https://github.com/riggsd/guano-r/blob/master/guano/R/guano.R Accessed: 
#' 22/09/2023  Licencse MIT.
.parse.timestamp <- function(s) {
  #check and replace space for T between date and time - ISO deviation in some WA recorders
  if(gregexpr(" ",s)[[1]][1]==11) {
    s <- gsub(" ","T",s)
  }
  return(strptime(s, "%Y-%m-%dT%H:%M:%S"))
}

