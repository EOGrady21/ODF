####Edit ODF####

#' Edit an ODF attribute
#'
#'Edit a metadata parameter in an existing ODF file, useful for correcting or
#'updating archived data files
#'
#' @param odffile an ODF file connection
#' @param param The parameter you wish to edit
#' @param value The value you wish to insert into the odf file
#'
#' @return an updated ODF file
#'
#'
#'
#'
#'
#' @export
editParam <- function(odffile, param, value){
 o <-  read_odf(odffile)
 headindex <- grep(param, o)
 header <- names(o[headindex])
 eval(parse(text = paste0('o$', header, '$', param, '<-  value')))
 write_odf(odf_object = o, output_file = odffile)

}

#' edit an ODF data frame
#'
#' Function returns ODF data frame which can be edited/ processed as necessary then written out again
#'
#' @param odffile an odf file connection
#'
#' @return the data frame from the odf file
#' @export
#'
#'
editData <- function(odffile){
  o <- read_odf(odffile)
  data <- o$DATA
  return (data)
}


#' Batch Edit ODF files
#'
#'This function can be used to edit a series of ODF files at a time, if a batch
#'of ODF files were produced with an incorrect parameter or value
#'
#'
#' @param odffiles a list of odf file names which are to be edited
#' @param param The parameter you wish to edit
#' @param value The value you wish to insert into ODF files
#'
#' @return edited ODF files
#' @export
#'
#' @examples see editParam example
#'
#' batchedit(odffiles = list.files(path = '.', pattern = '*.ODF'), param = 'ORGANIZATION', value = 'DFO SABS')
#'
#'
batchedit <- function(odffiles, param, value){
  for ( i in 1:length(odffiles)){
    o <-  read_odf(odffiles[i])
    headindex <- grep(param, o)
    header <- names(o[headindex])
    eval(parse(text = paste0('o$', header, '$', param, '<- ', value)))
    write_odf(odf_object = o, output_file = odffiles[i])
  }
}


#' odfSetMetadata
#'
#' @param odffile ODF file connection
#' @param param parameter of metadata to be changed
#' @param value new value of metadata parameter
#'
#' @return an oce odf object
#' @export
#'
#'
odfSetMetadata <- function(odffile, param, value){
  odf <- read.odf(odffile)
  odf <- oceSetMetadata(odf, param, value)
  return(odf)
}
