# BTOAcousticPipelineTools
An R Shiny App to help with auditing audio recordings processed with the BTO Acoustic Pipeline.

![Screenshot](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools/blob/main/www/screengrab01.jpg)

## Quick Start

1. Check Requirements
2. Follow Installation instructions
3. Do not directly run 'app.R'. Instead run 'launch_shiny_app.R', but only after completing steps 1 and 2 above


## Requirements

You will need R installed on your computer. We have tested the app on R versions 3.6.1 and 4.2. You do not need to have RStudio installed but if you are familiar with it this may be an easier way to run the code.

You will need the following R packages installed. If you are not familiar with installing R packages we have provided a configuration program (see below). You will only need to run this once. Required packages = shiny, shinyAlert, shinyFiles, shinyjs, devtools, tidyr, DT and guano. Note that guano needs to be installed from Github; the rest can be installed from CRAN (see below).


## Installation

* Open R, either directly or through RStudio.
* Download the package of R code: click the green "Code" button above and select Download ZIP. 
* Save and unzip the download. Remember where this is as you'll need to navigate to it to run the app.
* From R or RStudio open the script file called 'code/configure_local_machine.R'. Run this script to install/update the required R packages. This only needs to be done once on each computer (though you may need to repeat it if you update R).


## Usage

* *Do not directly run 'app.R'.* Instead, with R/RStudio open, open the script file called 'launch_shiny_app.R'. Follow the instructions there, in particular noting the need to update the path_to_app line to say where you have saved the 'app.R' file. 
* For most uses this is the only change you will need to make. For very large validation jobs (e.g. for a batch of >100,000 wav files) it may be more efficient to pass the paths to the audio and output directories, which can be done by adding these to the shinyOptions command as detailed in 'launch_shiny_app.R'.
* Running the 'launch_shiny_app.R' will open the app in a new browser window.


## Issues

If you have any problems with the app please add them on the Issues tab at the top of this screen.

Simon Gillings & Stuart Newson
August 2022

![APlogo](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools/blob/main/www/APlogo100px.png)



