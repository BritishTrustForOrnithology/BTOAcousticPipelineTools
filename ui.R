require(shiny)
require(shinyFiles)
require(shinyalert)
require(shinyjs)
#require(tidyr)
#require(DT)

source('R/defaults.R')

#define the UI
ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs,
  tags$head(
    tags$style(HTML
               ("
      .shiny-notification {
        position: fixed; 
        top: 60% ;
        left: 50%;
      }
      h4 {
        color: #23395D;
      }
      "))
  ),
  
  # Application title
  titlePanel(windowTitle = "BTO Acoustic Pipeline Tools",
             title = div(img(src="APlogo100px.png"), paste0("BTO Acoustic Pipeline Tools (v.",version,")"), style="font-size:100px; color: #31566d;")),
  
  tabsetPanel(
    tabPanel("Welcome", fluid = TRUE,
             h5("The BTO Acoustic Pipeline Tools app provides functions to assist with use of the 
                BTO Acoustic Pipeline. This software is provided under the MIT License:"),
             tags$br(),
             h6("Copyright (c) 2022 British Trust for Ornithology"),
             h6('Permission is hereby granted, free of charge, to any person obtaining a copy of 
             this software and associated documentation files (the "Software"), to deal in the 
             Software without restriction, including without limitation the rights to use, copy, 
             modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
             and to permit persons to whom the Software is furnished to do so, subject to the 
             following conditions:'),
             h6('The above copyright notice and this permission notice shall be included in 
             all copies or substantial portions of the Software.'),
             h6('THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
              IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
              FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
              AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
              LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
              OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
              SOFTWARE.'),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             h6('Version 1.1, August 2022'),
             h6(credit)
    ),
    
    tabPanel("Preprocessing", fluid = TRUE,
             sidebarPanel(
               h4("Purpose"),
               tags$p("Preprocessing check for common problems. Includes checking for file names that fail Pipeline requirements and whether embedded GUANO or XML files can be used to produce new unique filenames."),
               tags$br(),
               
               tags$div(id = "audit1", 
                        h4("Step 1: Select audio folder"),
                        tags$p("You can select a folder with subfolders for these checks but note that the Pipeline App (ultrasonic) only allows processing of single folders (no subfolders)."),
                        tags$p("Hint: Only use left side of popup to navigate to folder", style = "color: red;"),
                        shinyDirButton(id = 'dir_audioaudit',
                                       label = 'Select folder',
                                       title = 'Select folder containing Batlogger audio files',
                                       class = "btn-primary"),
                        verbatimTextOutput("path_audioaudit", placeholder = TRUE),
                        actionButton("scan_for_audio", "Scan for audio files", class = "btn-success"),
               ),
               tags$br(),
               
               tags$div(id = "audit2", 
                        h4("Step 2: Analyse files"),
                        tags$p("Analyse the files for GUANO metadata and XML files, suggest new file names and check for duplication issues."),
                        tags$p("Hint: analysis will include subfolders if present", style = "color: red;"),
                        actionButton("analyse_audio", "Analyse audio files", class = "btn-success")
               ),
               tags$br(),
               hr(style="border-color: grey;"),
               tags$br(),
             ),
             mainPanel(
               tags$div(id = 'batch_summary',
                 tags$h4("Summary stats for the audio files in the selected folder(s)"),
                 textOutput("audit_file_count"),
                 textOutput("audit_dir_count"),
                 tags$br(),
                 tags$h4("Diagnostics by folder"),
                 tableOutput("summary_per_dir"),
                 tags$br(),
                 textOutput('audit_filename_status'),
                 tags$br(),
                 textOutput('audit_rename_fail')
                )
             )
    ),
    
    tabPanel("Rename Batlogger files", fluid = TRUE,
             sidebarPanel(
               h4("Purpose"),
               tags$p("Batlogger wav files are numbered sequentially so when using one or more detectors 
              it is possible to have multiple files of the same name. Not having uniquely named 
              wav files can be problematic when it comes to auditing the results/recordings. This 
              utility renames Batlogger files, preferentially using GUANO metadata if available 
              (to include latitude, longitude, date and time in the name); if not filenames are pre-pended 
              with a concatenated version of the name of the folder hierarchy of where the wav 
              files are located.  In addition to renaming recordings, a log (csv file) is exported 
              containing any GUANO information that the files contain. We recommend that users 
              with Batloggers do this before uploading recordings to the Pipeline. This utility 
              may be useful for renaming wav files from other makes and models of bat detector, 
              or to export GUANO metadata from wav files."),
               tags$p("Please note, this utility will rename your original audio files. Monitor the warning messages carefully to ensure it is doing what you want.", style = "color: red"),
               tags$br(),
               tags$br(),
               
               h4("Step 1: Select audio folder"),
               tags$p("Select the folder containing the audio files you want to rename."),
               tags$p("Hint: Only use left side of popup to navigate to folder", style = "color: red;"),
               shinyDirButton(id = 'dir_batlogger', 
                              label = 'Select folder', 
                              title = 'Select folder containing Batlogger audio files',
                              class = "btn-primary"),
               verbatimTextOutput("path_batlogger", placeholder = TRUE),
               actionButton("scan_for_audio", "Scan for audio files", class = "btn-success"),
               tags$br(),
               hr(style="border-color: grey;"),
               tags$br(),
               h4("Step 2: Analyse files"),
               tags$p("Analyse the Batlogger files for GUANO metadata, suggest new file names and check for duplication issues."),
               actionButton("analyse_audio", "Analyse audio files", class = "btn-success"),
               tags$br(),
               hr(style="border-color: grey;"),
               tags$br(),
               h4("Step 3: Rename files"),
               tags$p("Warning: Clicking the following button will change audio filenames in the selected folders. Proceed with caution", style = "color: DarkOrange;"),
               actionButton("rename_audio", "Rename audio files", class = "btn-success"),
               tags$br(),
               tags$br(),
               tags$br(),
               actionButton("exit3", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);"),
             ),
             mainPanel(
               tags$div(id = 'batloggertab',
                        h4('Batlogger file diagnostics'),       
                        textOutput("batlogger_n_files"),
                        tags$br(),
                        textOutput("batlogger_n_with_guano"),
                        tags$br(),
                        textOutput("batlogger_n_dupe_names"),
                        tags$br(),
                        tags$h4('List of detected audio files with proposed new name'),
               ),
               tableOutput('batlogger_table')
             ),
    ),
    
    tabPanel('Organise wavs for auditing', fluid=TRUE,
             
             sidebarPanel(
               h4("Purpose"),
               tags$p("This utility is to save users time when auditing 
                identifications provided by the BTO Acoustic Pipeline. The utility can be 
                configured to copying wav files into folders according to the species 
                identified in each sound file (as detailed in the Pipeline results csv file). 
                Copying methods can be adjusted per species to select identities above certainty 
                thresholds, highest scoring identities per survey location, or in order to return
                a random samples. The last option is useful for very common species and noise
                (No ID) classes and allows the calculation of identification error rates for the 
                random sample: a low error rate from the random sample may justify not checking 
                all clips for common species."),
               tags$br(),
               tags$br(),
               
               h4("Step 1: Import Pipeline Results"),
               tags$p("Select one or more Acoustic Pipeline results csv files. Hold down Ctrl/Command key to select multiple files"),
               shinyFilesButton(id = 'file_csv', 
                                label = 'Select files', 
                                title = 'Select one or more csv files',
                                multiple = TRUE,
                                class = "btn-primary"),
               verbatimTextOutput("file_csv", placeholder = TRUE),
               actionButton("read_results", "Import results", class = "btn-success"),
               tags$br(),
               hr(style="border-color: grey;"),
               tags$br(),
               
               tags$div(id = "step2", 
                        h4("Step 2: Analyse and check sample sizes"),
                        tags$p("Obtain a list of species detected in this dataset and based on a threshold probability
          set using the slider below, determine the number of audio files of High and Low certainty. We 
          recommend using a threshold of 0.5 but this can be tailored to your situation."),
                        sliderInput(
                          "pthreshold",
                          "Set threshold for low/high certainty:",
                          min = 0,
                          max = 1,
                          value = 0.5,
                          step = 0.05
                        ),
                        actionButton("analyse_results", "Analyse recordings", class = "btn-success"),
                        tags$br(),
                        hr(style="border-color: grey;")
               ),
               
               tags$br(),
               
               tags$div(id = "step3", 
                        h4("Step 3: Set copy methods and parameters for sampling"),
                        tags$p("Use the drop-down menus in each row of the table to decide on the copy method to use for 
                 that species/identity, then use the sliders below to set the sample sizes for any methods 
                 you will use."),
                        sliderInput(
                          inputId = "sampsize_tophits",
                          label = "Top hits - number of files to copy per location and survey night",
                          min = 1,
                          max = 10,
                          value = 5
                        ),
                        sliderInput(
                          inputId = "sampsize_random",
                          label = "Random sample - number of files to randomly sample per species",
                          min = 1,
                          max = 1000,
                          value = 100
                        ),
                        sliderInput(
                          inputId = "sampsize_noid",
                          label = "Random sample (No ID class only) - number of files to randomly sample",
                          min = 1,
                          max = 1000,
                          value = 100
                        ),
                        hr(style="border-color: grey;")
               ),
               
               tags$br(),
               
               tags$div(id = "step4",
                        h4("Step 4: Set audio and output folders"),
                        tags$p("Finally, select the folder containing your original audio files, and a folder where you want the species folders to be made, and then click Prepare files for copy. Your original audio files will not be modified."),
                        tags$p("Hint: Only use left side of popup to navigate to folder", style = "color: red;"),
                        shinyDirButton(id = 'dir_audio', 
                                       label = 'Select audio folder', 
                                       title = 'Select folder containing original audio files',
                                       class = "btn-primary"),
                        verbatimTextOutput("path_audio", placeholder = TRUE),
                        shinyDirButton(id = 'dir_output', 
                                       label = 'Select folder for species outputs', 
                                       title = 'Select folder where species files to be copied',
                                       class = "btn-primary"),
                        verbatimTextOutput("path_output", placeholder = TRUE),
                        checkboxInput(
                          inputId = "append_identity",
                          label = 'Append species code to filenames?',
                          value = FALSE,
                          width = NULL
                        ),
                        #tags$br(),
                        hr(style="border-color: grey;"),
                        tags$br(),
               ),
               
               tags$div(id = "step4manual",
                        h4("Step 4: Use preset audio and output folders"),
                        tags$p("Folder containing your original audio files and folder where you want the species folders to be made, as defined by passed options. Now click Prepare files for copy. Your original audio files will not be modified."),
                        verbatimTextOutput("path_audio2", placeholder = TRUE),
                        verbatimTextOutput("path_output2", placeholder = TRUE),
                        checkboxInput(
                          inputId = "append_identity2",
                          label = 'Append species code to filenames?',
                          value = FALSE,
                          width = NULL
                        ),
                        #tags$br(),
                        hr(style="border-color: grey;"),
                        tags$br(),
               ),
               
               
               tags$div(id = "step5",
                        h4("Step 5: Prepare, validate and copy the files"),
                        tags$p("Prepare the files needing to be copied..."),
                        actionButton("prepare_files", "Prepare files for copy", class = "btn-success"),
                        tags$p("Check for missing files and other potential issues..."),
                        actionButton("validate_files", "Validate files for copy", class = "btn-success"),
                        tags$p("Copy the files to the new folders..."),
                        actionButton("copy_files", "Copy files", class = "btn-success")
               ),
               tags$br(),
               tags$br(),
               tags$br(),
               actionButton("exit1", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);"),
             ),
             mainPanel(
               tags$div(id = 'diagnostics',
                        h4("Diagnostics"),
                        textOutput("tempval"),
                        textOutput("num_results_files"),
                        textOutput("num_detections"),
                        textOutput("num_detections_perfile"),
                        textOutput("num_noid"),
                        textOutput("num_noid_reduced"),
                        textOutput("num_detections_final"),
               ),
               tags$br(),
               tags$br(),
               tags$div(id = "summary",
                        h4("Species summary"),
                        tags$p("Number of detections by species group, species and ID certainty. If
             necessary, adjust the probability threshold using the slider and press 
             Analyse recordings to re-run. Files and folders will be made on the basis 
             of the Copy method selected for each species/identity:"),
                        tags$ul(
                          tags$li("None: no wav files will be copied for this species"),
                          tags$li("Top hits: for each location and survey night, the x wav files with highest probability will be copied"),
                          tags$li("High certainty only: all wavs files with probability exceeding the threshold"),
                          tags$li("High & Low certainty: all wav files for the species, split into High and Low folders according to threshold probability"),
                          tags$li("Random sample: a random sample of x wav files will be created, separately for High and Low certainty recordings")
                        )
               ),
               tags$br(),
               tags$br(),
               DT::dataTableOutput('xtab'),
             )
    ),
    tabPanel("Exit", fluid = TRUE,
             tags$br(),
             tags$br(),
             tags$br(),
             actionButton("exit2", "Close App", class = "btn-danger", onclick = "setTimeout(function(){window.close();},500);"),
    )
  )
)