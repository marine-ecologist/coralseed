#' Safely Save R Objects Without Overwriting the File
#' 
#' code from https://github.com/stopsack/khsmisc/blob/HEAD/R/datahandling.R
#'  
#' @description Wraps \code{\link[base]{save}}. If the file already exists,
#'   it will not be overwritten. A message will be printed indicating whether
#'   the file was successfully written or if it already existed.
#'
#' @param ... Data frame(s), other object(s), and further arguments,
#'   passed on to \code{\link[base]{save}}. Required.
#' @param file Path/file name to for output. Required.
#'

save2 <- function(..., file = stop("'file' must be specified")) {
  if(!file.exists(file)) {
    save(..., file = file)
    print(paste("File written:", file))
  } else
    print(paste("Output file", file, "already exists. Not overwritten."))
}