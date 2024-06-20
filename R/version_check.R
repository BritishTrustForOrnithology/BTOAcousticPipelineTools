#' Compare local version with current version on Github
#' @details Throw an error if the local version is different to the web version.
version_check <- function(path_to_app) {
  
  #check package versions
  cat('\n\n-------------------------- SESSION INFO ------------------------------------\n',
      '  In case of problems check this information with the requirements at:\n',
      '  https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools\n',
      '  and log any issues on the Issues tab.\n',
      '----------------------------------------------------------------------------\n\n')
  print(sessionInfo())
  cat('\n----------------------------------------------------------------------------\n\n')
  
  version_web <- tryCatch(
    {
      # Attempt to open the file
      file_content <- read.csv(url("https://raw.githubusercontent.com/BritishTrustForOrnithology/BTOAcousticPipelineTools/main/version.txt"))
    },
    error = function(e) {
      # Handle the error (e.g., file not found)
      print("\n\nError: cannot access the version file on the web. Check web connection.")
    }
  )
  version_local <- read.csv(file.path(dirname(path_to_app),'version.txt'))
  if(version_web$version != version_local$version) warning("You are not running the latest version of the app. \nInstalled version = ", version, " Latest version = ", version_web$version, "\nYou are advised to stop as certain features may no longer work.\nPlease remove this version and reinstall from:\nhttps://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools\n")
  if(version_web$version == version_local$version) message("Success! You are running the latest version of the app. Please continue.\n")  
}
