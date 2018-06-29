####Edit ODF####

#' Edit an ODF attribute
#'
#' @param odffile an ODF file connection
#' @param param The parameter you wish to edit
#' @param value The value you wish to insert into the odf file
#'
#' @return an updated ODF file
#' @export
#'
#' @examples
#' 
#' editParam('MADCPS*...*.ODF, 'MIN_DEPTH', '2.5')
editParam <- function(odffile, param, value){
 o <-  read_odf(odffile)
 headindex <- grep(param, o) 
 header <- names(o[headindex])
 eval(parse(text = paste0('o$', header, '$', param, '<- ', value)))
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
#' @examples
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

