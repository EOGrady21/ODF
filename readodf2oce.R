####readodf2oce####
##using oce library


#' odf2oce
#'
#'Returns oce object with complete metadata, can also handle instruments which
#'do not have an equivalent oce object class
#' @param odffile an odf file name and path
#'
#' @return an oce object 
#' @export
#'
#' @examples
#' #read current meter
#' #note sometimes as.cm  does not include all metadata
#' o <- read.odf(file = 'C:/Users/ChisholmE/Documents/sample files/mcm/MCM_HUD2013008_1844_602_3600.ODF')
#' mcm <- as.cm(o)
#' #using odf2oce returns all metadata from odf
#' mcm <- oce2odf('C:/Users/ChisholmE/Documents/sample files/mcm/MCM_HUD2013008_1844_602_3600.ODF')


odf2oce <- function(odffile){
  odf <- read.odf(file = odffile)
  oce <- new('oce')
  oce@data <- odf@data
  oce@metadata <- odf@metadata
  return(oce)
}





