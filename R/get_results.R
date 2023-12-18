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