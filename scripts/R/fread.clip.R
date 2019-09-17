#' fread clipboard reader
#' 
#' Reads clipboard data into a data table and optionally saves it
#' 
#' @tmpfile name of file to be saved
#' @... other options to be passed to fread
#' 
#' @example
#' DT <- fread.clip()
fread.clip <- function(tempfile="",...) {
  X <- tempfile(pattern = paste0(tempfile,"_"),fileext = ".txt",tmpdir = getwd())
  writeLines(readLines("clipboard"), X)
  fread(X, ...)
}