#' Check filenames for Pipeline formats and valid datetimes
#' 
#' @details Check the filename string that it matches accepted formats as used by Audiomoth, Wildlife Acoustics,
#' Peersonic and Petersson. If the string passes, then check if the date time component of the filename is a valid
#' datetime.
#' 
#' @param filename string of the filename (just basename, no path)
#' 
#' @examples
#' # example code
check_filename_compliance(filename = '20120812-091406.wav')                     #audiomoth PASS
check_filename_compliance(filename = "34440445_202306.wav")                     #audiomoth FAIL - bad date
check_filename_compliance(filename = "20121115_252306.wav")                     #audiomoth FAIL - bad time
check_filename_compliance(filename = "34440445_202306132456.wav")               #audiomoth FAIL - trailing numbers
check_filename_compliance(filename = 'SM4A123_20120812_091406.wav')             #wildlifeacoustics PASS
check_filename_compliance(filename = 'Wav0123_2008_07_04__22_58_14.wav')        #peersonic PASS
check_filename_compliance(filename = 'Wav0123_2008_07_04_22_58_14.wav')         #peersonic FAIL (bad dt format)
check_filename_compliance(filename = 'Wav0123_2008_06__31_22_58_14.wav')        #peersonic FAIL (bad date)
check_filename_compliance(filename = 'Wav0123_2008_07__31_25_58_14.wav')        #peersonic FAIL (bad time)
check_filename_compliance(filename = 'ABC2010-08-26_10_39_50_M00667DEF.wav')    #petersson PASS
check_filename_compliance(filename = '1232010-08-26_10_39_50_M00667DEF.wav')    #petersson FAIL (numeric start)
check_filename_compliance(filename = '1232010-08-26_10_39_50_00667DEF.wav')     #petersson FAIL (numeric end)
check_filename_compliance(filename = 'ABC2010-06-31_10_39_50_M00667DEF.wav')    #petersson FAIL (bad date)
check_filename_compliance(filename = 'C:/20120812-091406.wav')                  #audiomoth ERROR (included path)
  
check_filename_compliance <- function(filename) {
  if(basename(filename) != filename) stop(paste('Passed filename should not include path:', filename))
  
  good <- FALSE
  
  #Audiomoth / Wildlife Acoustics format
  #20120812_091406.wav'                     # audiomoth
  #SM4A123_20120812_091406.wav'             # wildlifeacoustics
  pattern_underscore <- "(?:^|[^0-9])\\d{8}_\\d{6}[^0-9]"
  if(grepl(pattern_underscore, filename) == TRUE) {
    dtbit <- regmatches(filename, regexpr("\\d{8}_\\d{6}", filename))
    #can this be interpreted as a date time?
    good <- !is.na(as.POSIXct(dtbit, format = "%Y%m%d_%H%M%S"))
  }

  #Like WA and Audiomoth but with hyphens
  #20120812-091406.wav
  pattern_hyphen <- "(?:^|[^0-9])\\d{8}-\\d{6}[^0-9]"
  if(grepl(pattern_hyphen, filename) == TRUE) {
    dtbit <- regmatches(filename, regexpr("\\d{8}-\\d{6}", filename))
    #can this be interpreted as a date time?
    good <- !is.na(as.POSIXct(dtbit, format = "%Y%m%d-%H%M%S"))
  }
  
  #Peersonic format
  #Wav0123_2008_07_04__22_58_14.wav
  pattern_peersonic <- "(?:^|[^0-9])\\d{4}_\\d{2}_\\d{2}__\\d{2}_\\d{2}_\\d{2}[^0-9]"
  if(grepl(pattern_peersonic, filename) == TRUE) {
    dtbit <- regmatches(filename, regexpr("\\d{4}_\\d{2}_\\d{2}__\\d{2}_\\d{2}_\\d{2}", filename))
    #can this be interpreted as a date time?
    good <- !is.na(as.POSIXct(dtbit, format = "%Y_%m_%d__%H_%M_%S"))
  }
  
  #Petersson format
  #ABC2010-08-26_10_39_50_M00667DEF.wav
  pattern_petersson <- "(?:^|[^0-9])\\d{4}-\\d{2}-\\d{2}_\\d{2}_\\d{2}_\\d{2}[^0-9]"
  if(grepl(pattern_petersson, filename) == TRUE) {
    dtbit <- regmatches(filename, regexpr("\\d{4}-\\d{2}-\\d{2}_\\d{2}_\\d{2}_\\d{2}", filename))
    #can this be interpreted as a date time?
    good <- !is.na(as.POSIXct(dtbit, format = "%Y-%m-%d_%H_%M_%S"))
  }
  
  return(good)
}
