bfspec <- function(C) {
#'   ------------------------------------------------------------------------
#'   BFSPEC: Creates a file specification string based on ODF header information.
#' 
#'   ODSToolbox Version: 2.0
#' 
#'   Creation Date: 02-SEP-2015
#'   Last Updated: 02-SEP-2015
#'   
#'   @author: Jeff Jackson
#' 
#'   @version: 1.0 
#' 
#'   @copyright: 2014, Fisheries and Oceans Canada. All Rights Reserved.   
#' 
#'   Source:
#'       Ocean Data and Information Services,
#'       Bedford Institute of Oceanography, DFO, Canada.
#'       DataServicesDonnees@dfo-mpo.gc.ca
#' 
#'   @summary: Creates a file specification string based on ODF header information.
#' 
#'   Usage: bstr = bfspec(A)
#' 
#'   Input:
#'       A: An ODF structured array.
#' 
#'   Output:
#'       bstr: The file specification string.
#' 
#'   Example:
#'       bfstr = bfspec(ODF)
#' 
#'   Notes:
#'  
#'   Updates:
#' 
#'     Jeff Jackson (02-SEP-2015)
#'     - 
#'   
#'   Report any bugs to DataServicesDonnees@dfo-mpo.gc.ca
#'   ------------------------------------------------------------------------

  ch <- C@metadata$odfHeader$CRUISE_HEADER
  eh <- C@metadata$odfHeader$EVENT_HEADER
  
  bstr <- paste(eh$DATA_TYPE, '_', ch$CRUISE_NUMBER, '_', eh$EVENT_NUMBER, '_', 
          eh$EVENT_QUALIFIER1, '_', eh$EVENT_QUALIFIER2, sep = "")

  return(bstr)
}