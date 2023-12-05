#' Read a single GUANO file
#' 
#' @param filename The GUANO filename or path
#' @param verbose Whether to print the raw GUANO lines before parsing data types
#' @return list of named metadata fields
read_guano <- function(filename, verbose = FALSE) {
  #' Maps metadata keys to a data type coercion function
  d.types <- list(
    `Filter HP`=as.double,
    `Filter LP`=as.double,
    Humidity=as.double,
    Length=as.double,
    `Loc Accuracy`=as.integer,
    `Loc Elevation`=as.double,
    Note=function(val) gsub("\\\\n", "\n", val),
    Samplerate=as.integer,
    #`Species Auto ID`=?, `Species Manual ID`=?,  # TODO: comma separated
    #Tags=?,  # TODO: comma separated
    TE=function(val) if (is.na(val) || is.null(val) || val == "") 1 else as.integer(val),
    `Temperature Ext`=as.double, `Temperature Int`=as.double,
    Timestamp=.parse.timestamp
  )
  
  
  f <- file(filename, "rb")
  riff.id <- readChar(f, 4)
  if (length(riff.id) == 0 || riff.id != "RIFF") return(NULL)
  riff.size <- readBin(f, integer(), size=4, endian="little")
  wave.id <- readChar(f, 4)  # "WAVE"
  if (length(wave.id) == 0 || wave.id != "WAVE") return(NULL)
  
  read.subchunk <- function() {
    id <- readChar(f, 4)
    if (length(id) == 0 || id == "") return(NULL)
    size <- readBin(f, integer(), size=4, endian="little")
    list(id=id, size=size)
  }
  
  skip.subchunk <- function(chunk) {
    #print(sprintf("Skipping subchunk '%s' ...", chunk$id))
    pos <- seek(f, NA)
    seek(f, pos + chunk$size)
  }
  
  md <- list()
  
  while (!is.null(chunk <- read.subchunk())) {
    if (chunk$id != "guan") {
      skip.subchunk(chunk)
      next
    }
    md[["File Path"]] <- normalizePath(filename)
    md[["File Name"]] <- basename(filename)
    md.txt <- readChar(f, chunk$size)
    Encoding(md.txt) <- "UTF-8"  # FIXME: this still isn't setting the encoding to UTF-8
    for (line in strsplit(md.txt, "\n")[[1]]) {
      line <- trimws(line)
      if (line == "") {
        next
      }
      if(verbose == TRUE) print(line)
      toks <- strsplit(sub(":", "\n", line), "\n")
      key <- trimws(toks[[1]][1])
      val <- trimws(toks[[1]][2])
      if (is.na(key) || is.null(key) || key == "") {
        next
      }
      if (!is.null(d.types[[key]])) {
        val <- d.types[[key]](val)
      }
      md[[key]] <- val
    }
    if ("Loc Position" %in% names(md)) {
      coords <- lapply(strsplit(md[["Loc Position"]], " "), as.double)[[1]]
      md[["Loc Position Lat"]] <- coords[1]
      md[["Loc Position Lon"]] <- coords[2]
      md[["Loc Position"]] <- NULL
    }
  }
  
  close(f)
  return(md)
}
