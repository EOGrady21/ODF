#' Source a directory
#' @author: Unknown
#' Source an entire directory, useful for quickly incorporating a series of .R
#' function files
#'
#' @param file_path the path of the directory to source
#'
#' @return
#' @export
#'
#' @
sourceDir <- function(file_path) {
X <- list.files(path=file_path, pattern='*.R',full.names=FALSE)
for(j in X) {
   if (j != 'sourceDir.R')  source(paste(file_path,j,sep="/"),echo=FALSE)
}
return()
}
