sourceDir <- function(file_path) {
X <- list.files(path=file_path, pattern='*.R',full.names=FALSE)
for(j in X) {
   if (j != 'sourceDir.R')  source(paste(file_path,j,sep="/"),echo=FALSE)
}
return()
}