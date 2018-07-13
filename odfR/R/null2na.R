null2na <- function(A) {
#'   -----------------------------------------------------------------------    
#'   null2na: Replace null values (-99) with NA in the data of an ODF
#'   structure.
#'   
#'   ODSToolbox Version: 2.0
#'   
#'   Creation Date: 09-SEP-2015
#'   Last Updated: 12-NOV-2015
#' 
#'   @author: Jeff Jackson
#' 
#'   @version: 1.0 
#' 
#'   @copyright: Fisheries and Oceans Canada
#' 
#'   Source:
#'       Ocean Data and Information Services,
#'       Bedford Institute of Oceanography, DFO, Canada.
#'       DataServicesDonnees@dfo-mpo.gc.ca
#' 
#'   @summary: Substitutes null data with empty strings in an ODF structure.
#' 
#'   Usage:
#'       A = null2na(A)
#' 
#'   Input:
#'       A : an ODF structured array
#' 
#'   Output:
#'       A : the updated ODF structured array with null data replaced with NA
#' 
#'   Example:
#'       A = null2na(A)
#' 
#'   Notes:
#' 
#'   See also 
#'     
#'   Updates:
#' 
#'     Jeff Jackson (22-SEP-2015)
#'     - Fixed an error.
#' 
#'     Jeff Jackson (12-NOV-2015)
#'     - Fixed a logic error.
#' 
#'   Report any bugs to DataServicesDonnees@dfo-mpo.gc.ca
#'   ------------------------------------------------------------------------

  # Get the CRUISE_HEADERs.
  ch <- A@metadata$odfHeader$CRUISE_HEADER
  
  # Get the PARAMETER_HEADERs.
  plist <- A@metadata$odfHeader$PARAMETER_HEADER
  
  # Get the DATA records.
  nrows <- length(A@data[[1]])
  np <- length(A@data)
                  
  # Get a list of the codes, wmo_codes and data types from the parameter headers.
  codes <- NULL
  wmo_codes <- NULL
  types <- NULL
  sytm_present <- FALSE
  ss <- 0
  for (i in 1:length(plist)) {
    code <- plist[[i]]$CODE
    wmo_code <- plist[[i]]$WMO_CODE
    type <- plist[[i]]$TYPE

    if (!is.null(code)) {
      codes <- c(codes, code)
      # Find out if there is a SYTM parameter in the ODF file.
      if (code == "SYTM_01") {
        sytm_present <- TRUE
        ss <- i
      }
    }
    if (!is.null(wmo_code)) {
      wmo_codes <- c(wmo_codes, wmo_code)
      # Find out if there is a SYTM parameter in the ODF file.
      if (wmo_code == "SYTM_01") {
        sytm_present <- TRUE
        ss <- i
      }
    }
    types <- c(types, type)
  }
    
  # Loop through all of the data values in the data set. Skip SYTM column(s).
  for (pp in 1:np) {
    # Loop through all of the columns in the current row.
    if ((sytm_present == TRUE) && (pp == ss)) {
      next()
    }
    ctype <- types[pp]
    for (rr in 1:nrows) {
      if ((ctype == 'INTE') || (ctype == 'SING') || (ctype == 'DOUB')) {
        # Convert the current column's data type from string to float.
        value <- A@data[[pp]][rr]
        if (!is.null(value) && !is.na(value)) {
          if (value == -99) {
            A@data[[pp]][rr] <- NA
          }
        }
      }
    }
  }
  return(A)
}