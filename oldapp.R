#########################################################################
#'                                                                      # 
#' DO NOT RUN THIS PROGRAM DIRECTLY. INSTEAD RUN LAUNCH_SHINY_APP.R     #
#'                                                                      # 
#########################################################################



#' Shiny App to assist with auditing of audio files processed via BTO Acoustic Pipeline 
#' Written by Simon Gillings
#' July 2022






# Run the application
shinyApp(ui = "ui.R", server = "server.R")