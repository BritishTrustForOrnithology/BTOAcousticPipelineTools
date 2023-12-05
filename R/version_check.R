#' Compare local version with current version on Github
#' @details Throw an error if the local version is different to the web version.
version_check <- function(path_to_app) {
  version_web <- tryCatch(
    {
      # Attempt to open the file
      file_content <- read.csv(url("https://raw.githubusercontent.com/BritishTrustForOrnithology/BTOAcousticPipelineTools/main/version.txt"))
    },
    error = function(e) {
      # Handle the error (e.g., file not found)
      print("Error: cannot access the version file on the web")
    }
  )
  version_local <- read.csv(file.path(dirname(path_to_app),'version.txt'))
  if(version_web$version != version_local$version) stop("You are not running the latest version of the app. You are advised to stop as certain features may no longer work. Please remove this version and reinstall from https://github.com/BritishTrustForOrnithology/BTOAcousticPipelineTools")
  if(version_web$version == version_local$version) cat("Success! You are running the latest version of the app. Please continue.\n")  
}
