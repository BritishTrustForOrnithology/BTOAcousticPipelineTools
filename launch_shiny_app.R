#'  To run the Acoustic Pipeline Tools app on your computer:
#'
#'  1. Do not run the app.R program directly. Instead use launch_shiny_app.R
#'  
#'  2. Edit the path_to_app line to give the location of the app.R program on 
#'     your computer. Note that you must use forward (/) slashes, e.g.:
#'     path_to_app <- 'C:/My Documents/app.R'
#'
#'  3. For most users you do not need to change the shinyOptions line. But if you
#'     are processing a very large volume of files (>100,000) it is sometimes 
#'     more efficient to pass the full path to the audio files and to the output 
#'     location at start-up, e.g.:
#'     shinyOptions(path_audio='C:/recordings/2022', path_output='C:/checking/2022')
#'     Remember to reset these to =NULL before next running.
#'
#'  4. Run all command lines and the app will launch in a browser winder.

#path to where app is saved on your machine- update if necessary
path_to_app <- 'app.R'

#run the app
shinyOptions(path_audio=NULL, path_output=NULL)
shiny::runApp(path_to_app, launch.browser = TRUE)
