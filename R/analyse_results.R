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