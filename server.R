#require(tuneR)
require(shiny)
require(shinyFiles)
require(shinyalert)
require(shinyjs)
require(tidyr)
require(DT)
require(xml2)

source('R/utils.R')



#define the server
server <- function(input, output, session) {
  
  version_check(path_to_app)
  
  #set up various global defaults  
  global <- reactiveValues(
    
    #global vars for the preprocessing audit step
    path_audioaudit = NULL,
    wavs_to_audit = NULL,
    n_files_corrupt = NULL,
    all_ok = NULL,
    all_filename_fail = NULL,
    n_dirs_in_batch = NULL,
    n_files_in_batch = NULL,
    allow_rename = NULL,
    n_need_renaming = NULL,
    n_cannot_rename = NULL,
    n_files_in_batch = NULL,
    n_original_names_duplicated = NULL,
    n_new_names_duplicated = NULL,
    summary_per_dir = NULL,
    file_data = NULL,
    
    #batlogger_files = NULL,
    #path_batlogger = NULL,
    
    #global vars for the organising postprocessing step
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
  show(id = 'audit1')
  hide(id = 'scan_for_audio')
  hide(id = 'audit2')
  hide(id = 'audit3')
  hide(id = 'audit_batch_summary')
  hide(id = 'audit_errors')
  hide(id = 'audit_warnings')
  hide(id = 'audit_info')
  hide(id = 'audit_newnames')
  
  #hide(id = 'batloggertab')
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
  #disable(id = 'analyse_audio')
  disable(id = 'rename_audio')
  
  
  #select the folders
  shinyDirChoose(input,'dir_audioaudit', roots = volumes, session = session, filetypes = c(''))
  shinyDirChoose(input,'dir_batlogger', roots = volumes, session = session, filetypes = c(''))
  shinyFileChoose(input, 'file_csv', roots = volumes, session = session, filetypes=c('csv'))
  shinyDirChoose(input,'dir_audio', roots = volumes, session = session, filetypes = c(''))
  shinyDirChoose(input,'dir_output', roots = volumes, session = session, filetypes = c(''))
  
  #event handlers
  #file and directory event handlers
  observeEvent(eventExpr = {input$dir_audioaudit}, handlerExpr = {
    global$path_audioaudit <- parseDirPath(volumes, input$dir_audioaudit)
    show(id = 'scan_for_audio')
    hide(id = 'audit_batch_summary')
    hide(id = 'audit2')
    hide(id = 'audit3')
    hide(id = 'audit_message')
    # hide(id = 'audit_errors')
    # hide(id = 'audit_warnings')
    # hide(id = 'audit_info')
    hide(id = 'audit_newnames')
  } )

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
  observeEvent(input$scan_for_audio, {
    global$wavs_to_audit <- list_audio_files(path_to_process = global$path_audioaudit)
    if(length(global$wavs_to_audit)>0) {
      show(id = 'audit2')
    }
  })

  
  #event handler for analysing batlogger files
  observeEvent(input$analyse_audio, {
    #analyse the files for name conformity, guano, xml and create newnames if required
    temp <- audit_audio(path_to_process = global$path_audioaudit, files = global$wavs_to_audit)
    print(temp)
    #unpack
    global$n_files_in_batch <- temp$n_files
    global$n_dirs_in_batch <- temp$n_dirs
    global$n_files_acceptable <- temp$n_files_acceptable
    global$n_files_corrupt <- temp$n_files_corrupt
    global$n_files_good_names <- temp$n_files_good_names
    global$n_files_bad_names <- temp$n_files_bad_names
    global$n_cannot_rename <- temp$n_cannot_rename
    global$n_duplicated_oldnames <- temp$n_duplicated_oldnames
    global$n_duplicated_newnames <- temp$n_duplicated_newnames
    global$n_files_with_location <- temp$n_files_with_location
    global$summary_per_dir <- temp$summary_per_dir
    global$file_data <- temp$file_data
    
    #BOOLEANS
    #allow renaming only if: no files that can't be renamed, no duplicates will be generated
    global$allow_rename <- ifelse(global$n_files_bad_names == global$n_files_in_batch & global$n_cannot_rename == 0 & global$n_duplicated_newnames == 0, TRUE, FALSE)
    
    global$all_files_with_location <- ifelse(global$n_files_with_location == global$n_files_in_batch, TRUE, FALSE)
    
    
    #error 
    global$some_files_corrupt <- ifelse(global$n_files_corrupt > 0, TRUE, FALSE)
    global$some_files_unrenamable <- ifelse(global$n_files_bad_names > 0 & global$n_cannot_rename > 0, TRUE, FALSE)
    global$some_newnames_duplicated <- ifelse(global$n_duplicated_newnames > 0, TRUE, FALSE)
    #warnings
    global$some_names_good <- ifelse(global$n_files_bad_names > 0, TRUE, FALSE)  
    global$no_names_good <- ifelse(global$n_files_good_names == 0, TRUE, FALSE)  
    global$some_oldnames_duplicated <- ifelse(global$n_duplicated_oldnames > 0, TRUE, FALSE)
    #info
    global$all_names_good <- ifelse(global$n_files_bad_names == 0, TRUE, FALSE)
    global$all_names_bad <- ifelse(global$n_files_bad_names == global$n_files_in_batch, TRUE, FALSE)
    global$all_files_processable <- ifelse(global$n_files_acceptable == global$n_files_in_batch, TRUE, FALSE)
    global$all_files_renamable <- ifelse(global$n_files_bad_names > 0 & global$n_cannot_rename == 0 & global$n_duplicated_newnames == 0, TRUE, FALSE)
    
    #VISIBILITY ----------------------------------------------------------------    
    #toggle on visibility of batch summary block
    show(id = 'audit_batch_summary')
    show(id = 'audit_message')
    

    #enable audio rename button
    if(global$allow_rename == TRUE) {
      show(id='audit3')
      show(id='audit_newnames')
      enable(id = 'rename_audio')
    }
    
    # LOGS ---------------------------------------------------------------------
    prefix <- format(Sys.time(), "%Y%m%d_%H%M%S_")
    #write log of corrupt files
    if(global$n_files_corrupt > 0) {
      corrupt <- subset(global$file_data, file_corrupt == 1)
      write.csv(corrupt, file = file.path(global$path_audioaudit, paste0(prefix,'log_corrupt_files.csv')), row.names = FALSE)
    }
    #write log of files that can't be renamed
    if(global$n_cannot_rename >0) {
      cannot <- subset(global$file_data, unrenamable == 1)
      write.csv(cannot, file = file.path(global$path_audioaudit, paste0(prefix,'log_cannot_rename.csv')), row.names = FALSE)
    }
    #write log of files that will have duplicated names
    if(global$n_duplicated_newnames >0) {
      newdupe <- subset(global$file_data, newname_duplicated == TRUE)
      write.csv(newdupe, file = file.path(global$path_audioaudit, paste0(prefix,'log_potential_duplicate_new_names.csv')), row.names = FALSE)
    }
  })
  

  #event handler for renaming the audio files
  observeEvent(input$rename_audio, {
    rename_audio_files(path_to_process = global$path_audioaudit, file_info = global$file_data)
    #once done, disable button to prevent repress
    disable(id = 'rename_audio')
    hide(id = 'audit_batch_summary')
    hide(id = 'audit_newnames')
    hide(id = 'audit2')
    hide(id = 'audit3')
    hide(id = 'audit_message')
    #and clear audit data
    global <- reactiveValues()
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
                                                        selected = xtab$method_default[i],
                                                        selectize = FALSE
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
  

  output$audit_message = renderText({
    text <- 
    
    #files are corrupt  
    if(global$some_files_corrupt == TRUE) {
      paste(global$n_files_corrupt, "of", global$n_files_in_batch, "files appear to be corrupt and cannot be read. See YYYYMMDD_log_corrupt_files.csv for details.") 
    }
    
    #all names are good = good to go
    else if(global$all_names_good == TRUE) {
      paste('All files have names containing date and time in a format recognised by the Pipeline. They can be processed as they are.')
    }
    
    #all names are bad but GUANO/XML so processable and...
    else if(global$all_names_bad == TRUE & global$all_files_processable == TRUE) {
      
      #some files can't be renamed
      if(global$some_files_unrenamable == TRUE) {
        paste('Files are processable by the Pipeline owing to presence of GUANO and/or XML. However, filenames are not in a recognised format and we recommend files are renamed, ideally with location, date and time to avoid downstream duplication issues. Unfortunately, the app cannot find sufficient information to safely rename the files automatically. See YYYYMMDD_log_cannot_rename.csv for details and correct manually before trying again.')
      }
      
      #renaming will create duplicates
      else if(global$some_newnames_duplicated == TRUE) {
        paste('Files are processable by the Pipeline owing to presence of GUANO and/or XML. However, filenames are not in a recognised format and we recommend files are renamed, ideally with location, date and time to avoid downstream duplication issues. Unfortunately, the app cannot rename the files automatically as this will generate files with duplicate names. See YYYYMMDD_log_potential_duplicate_new_names.csv for details.')
      }
      
      else if(global$all_files_with_location == TRUE) {
        paste('Files are processable by the Pipeline owing to presence of GUANO and/or XML. However, filenames are not in a recognised format and we recommend files are renamed. The app has extracted location, date and time from the metadata and suggested new filenames are indicated below. To proceed with renaming click the rename button below. If there are XML files associated with each WAV file these will also be renamed. Note that renaming cannot be undone. Once renamed, see YYYYMMDD_log_renaming_metadata.csv for details.')
      }
      
      else if(global$all_files_with_location == FALSE) {
        paste('Files are processable by the Pipeline owing to presence of GUANO and/or XML. However, filenames are not in a recognised format and we recommend files are renamed. The app has extracted date and time from the metadata but was unable to extract location information. Suggested new filenames are indicated below, with the old filename as a suffix. To proceed with renaming click the rename button below. If there are XML files associated with each WAV file these will also be renamed. Note that renaming cannot be undone. Once renamed, see YYYYMMDD_log_renaming_metadata.csv for details.')
      }
    } 
    else {
      paste('The app cannot work with this batch of files. This could be for a number of reasons including: some or all filenames are not in a recognised format and there is no embedded GUANO or associated XML file so these files cannot be processed by the Acoustic Pipeline as they are; filenames cannot be renamed automatically; the batch contains a combination of good and bad file names. We recommend carefully checking the table above which may hint where the problem lies. You may have to rename the files manually or via bespoke scripts.')
    }
    
    text
    
  })
  

  
  #AUDIT TABLES ----------------------------------------------------------------
  #directory stats
  output$audit_summary_per_dir = renderTable({
    dat <- global$summary_per_dir
    names(dat) <- c('Directory', 'Total', 'Corrupt', 'Acceptable', 'Bad filename', 'Has GUANO', 'Has XML', 'Renamable', 'Cannot rename') 
    dat
    }, digits=0
    )
  
  #suggested names
  output$audit_proposed_names = renderTable({
    if(global$allow_rename == TRUE) {
      keeprows <- min(10, nrow(global$file_data))
      global$file_data[1:keeprows, c('original_name', 'new_name')]
    }
  })
  
}