# BTOAcousticPipelineTools
An R Shiny App to help with auditing audio recordings processed with the BTO Acoustic Pipeline.

![Screenshot](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools_dev/blob/main/www/screengrab01.jpg)

## Requirements

You will need R installed on your computer. We have tested the app on R versions 3.6.1 and 4.2. You do not need to have RStudio installed but if you are familiar with it this may be an easier way to run the code.

You will need the following R packages installed. If you are not familiar with installing R packages we have provided a configuration program (see below). You will only need to run this once. Required packages = shiny, shinyAlert, shinyFiles, shinyjs, devtools, tidyr, DT and guano. Note that guano needs to be installed from Github; the rest can be installed from CRAN (see below).

## Installation

* Open R, either directly or through RStudio.
* Download the app code: click the green "Code" button above and select Download ZIP. 
* Save and unzip the download. Remember where this is as you'll need to navigate to it to run the app.
* From R or RStudio open the script file called 'code/configure_local_machine.R'. Run this script to install/update the required R packages. This only needs to be done once on each computer (though you may need to repeat it if you update R).

## Usage

To run the app, with R/RStudio open, open the script file called 'launch_shiny_app.R' and run it. You should not need to change any of the code, but if you are experience with R and move the 'app.R' file you will need to update path_to_app so that it points to where 'app.R' is located. 

Running the 'launch_shiny_app.R' will open the app in a new browser window.


## Issues

If you have any problems with the app please add them on the Issues tab at the top of this screen.

Simon Gillings & Stuart Newson
August 2022

![APlogo](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools_dev/blob/main/www/APlogo100px.png)



