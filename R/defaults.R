#' THE SHINY PART -------------------------------------------------------------------------------

#before doing anything, check this version is up to date
#version_check(path_to_app)

#passed parameters
passed_path_audio <- getShinyOption('path_audio', default = NULL)
passed_path_output <- getShinyOption('path_output', default = NULL)

#constants
credit <- 'App written by Simon Gillings, BTO'
version <- read.csv(file.path(dirname(path_to_app),'version.txt'), colClasses = 'character')

#get the drive letters
volumes <- shinyFiles::getVolumes()()