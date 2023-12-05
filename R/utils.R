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

