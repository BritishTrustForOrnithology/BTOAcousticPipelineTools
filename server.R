require(shiny)
require(shinyFiles)
require(shinyalert)
require(shinyjs)
require(tidyr)
require(DT)
require(xml2)


source('R/defaults.R')



#define the server
server <- function(input, output, session) {
  
  #set up various global defaults  
  global <- reactiveValues(
    wavs_to_audit = NULL,
    batlogger_files = NULL,
    path_audioaudit = NULL,
    path_batlogger = NULL,
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
  hide(id = 'batch_summary')
  
  hide(id = 'batloggertab')
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
  
  # #event handler for listing the batlogger files
  # observeEvent(input$scan_for_audio, {
  #   global$batlogger_files <- list_batlogger_files(path_to_process = global$path_batlogger)
  #   enable(id = 'analyse_audio')
  # })
  
  #event handler for analysing batlogger files
  observeEvent(input$analyse_audio, {
    temp <- audit_audio(path_to_process = global$path_audioaudit, files_old = global$wavs_to_audit)
    #temp <- audit_audio(path_to_process = 'C:/batlogger', files_old)
    #unpack
    global$n_files_in_batch <- temp$num_files
    global$n_original_names_duplicated <- temp$n_duplicated_oldnames
    global$n_new_names_duplicated <- temp$n_duplicated_newnames
    global$summary_per_dir <- temp$summary_per_dir
    print(global$summary_per_dir)
    global$file_data <- temp$file_data

    #format table for plotting and toggle on visibility of the div
    #global$batlogger_table <- temp$batlogger_log[,c('name_original', 'name_proposed', 'warning')]
    #names(global$batlogger_table) <- c('Original file name', 'Proposed file name', 'Warning')
    show(id = 'batch_summary')

    #if there are no duplicates, enable the rename button
    #if(global$batlogger_n_dupe_names == 0) enable(id = 'rename_audio')
  })
  
  # #event handler for analysing batlogger files
  # observeEvent(input$analyse_audio, {
  #   temp <- analyse_batlogger_files(path_to_process = global$path_batlogger, files_old = global$batlogger_files)
  #   #unpack
  #   global$batlogger_n_files <- temp$n_files
  #   global$batlogger_n_dupe_names <- temp$n_dupes
  #   global$batlogger_n_with_guano <- temp$n_with_guano
  #   global$batlogger_log <- temp$batlogger_log
  #   
  #   #format table for plotting and toggle on visibility of the div
  #   global$batlogger_table <- temp$batlogger_log[,c('name_original', 'name_proposed', 'warning')]
  #   names(global$batlogger_table) <- c('Original file name', 'Proposed file name', 'Warning')
  #   show(id = 'batloggertab')
  #   
  #   #if there are no duplicates, enable the rename button
  #   if(global$batlogger_n_dupe_names == 0) enable(id = 'rename_audio')
  # })
  
  
  
  #event handler for renaming the batlogger files
  observeEvent(input$rename_audio, {
    rename_batlogger_files(path_to_process = global$path_batlogger, batlogger_log = global$batlogger_log)
    #once done, disable button to prevent repress
    disable(id = 'rename_audio')
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
                                                        selected = xtab$method_default[i]
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
  
  #outputs
  output$audit_file_count <- renderText( { paste("Number of audio files in batch =", global$n_files_in_batch)})
  output$audit_dupe_oldnames <- renderText( { paste("Number of duplicates in original file names =", global$n_original_names_duplicated)})
  output$audit_dupe_newnames <- renderText( { paste("Number of duplicates in possible new file names =", global$n_new_names_duplicated)})
  
  
  
  
  #output$batlogger_n_files <- renderText( { paste("Number of batlogger audio files =", global$batlogger_n_files)})
  #output$batlogger_n_dupe_names <- renderText( { paste("Number of files with duplicate proposed names =", global$batlogger_n_dupe_names)})
  #output$batlogger_n_with_guano <- renderText( { paste("Number of files with embedded GUANO metadata =", global$batlogger_n_with_guano)})
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
  
  #output$batlogger_table = renderTable(global$batlogger_table)
  
  output$summary_per_dir = renderTable(global$summary_per_dir)
  

}