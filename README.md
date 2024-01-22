# BTOAcousticPipelineTools
An R Shiny App to help with auditing audio recordings processed with the BTO Acoustic Pipeline.

![Screenshot](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools/blob/main/www/screengrab01.jpg)

## Quick Start

1. Check Requirements
2. Follow Installation instructions
3. Open the Project and run 'launch_shiny_app.R'. See Usage for details. 


## Requirements

You will need R installed on your computer. We have tested the app on R versions 3.6.1 and 4.3.2. You do not need to have RStudio installed but if you are familiar with it this may be an easier way to run the code.

You will need the following R packages installed. If you are not familiar with installing R packages we have provided a configuration program (see below). You will only need to run this once. Required packages (tested version in brackets) = shiny (1.8.0), shinyAlert (3.0.0), shinyFiles (0.9.3), shinyjs (2.1.0), DT (0.31), devtools, tidyr, , xml2, tuneR.


## Installation

1. Open R, either directly or through RStudio.
2. Download the package of R code: click the green "Code" button above and select Download ZIP. 
3. Save and unzip the download. Remember where this is as you'll need to navigate to it to run the app.
4. Open RStudio, in top menu go to File > Open Project, and browse to where you have unzipped the code and select and open BTOAcousticPipelineToolsPUBLIC.Rproj
![Screenshot](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools/blob/main/www/help_open_project.jpg)

5. Once the Project has opened, in the lower right *Files* panel of Studio, open the file called *configure_local_machine.R*. Run this script to install/update the required R packages. This only needs to be done once on each computer (though you may need to repeat it if you update R).
6. Now proceed to the **Usage** section below


## Usage

1. If you do not have the BTOAcousticPipelineTools Project already open, repeat step 4 above. You can tell if the Project is open as it will say BTOAcousticPipelineTools at the top of the screen; otherwise it will just say RStudio.
2. Open the script file called *launch_shiny_app.R*. Provided you have followed step 4 above you should not need to edit anything in this script, otherwise follow the instructions on screen. To run the script press the green Run App button near the top of the screen.
3. For very large validation jobs (e.g. for a batch of >100,000 wav files) it may be more efficient to directly provide the paths to the audio and output directories at startup, rather than using the folder browser buttons. This can be done by adding these to the shinyOptions command as detailed in 'launch_shiny_app.R'. 
4. Running the 'launch_shiny_app.R' will open the app in a new browser window.


## How to update

Periodically we will release new versions of the app, adding new features or to reflect changes in the dependencies. To update the app on your computer we recommend you delete your existing copy from your computer and start afresh following the Installation instructions above. Please also re-read the Usage instructions in case they have changed since your previous version.


## Change Log

19/01/2024- version 2.1. Minor update to get table select inputs working again with shiny version 1.8.0. Also app version is checked at startup.

18/12/2023 - version 2. Major update to what was formerly a utility to rename Batlogger files, now renamed Preprocessing and extending functionality to include processing of old Batlogger files with associated XML files. Also now does a basic check for corrupt files. Version 2.0 now requires additional packages xml2 and tuneR.

22/09/2023 - removed dependency on guano-r as a) this does not always install easily for new users and b) this does not parse Wildlife Acoustics GUANO metadata properly. Now custom functions based on guano-r are included within the Shiny app code.


## Issues

If you have any problems with the app please add them on the Issues tab at the top of this screen.

Simon Gillings & Stuart Newson
August 2022

![APlogo](https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools/blob/main/www/APlogo100px.png)



