####add parameter####

#'Add ODF parameter
#'
#'   add a parameter to an existing ODF structure array (b)
#'reads in variable, add a parameter header, cal header and column to the data portion of the ODF
#'
#'parameter header has: type, name, units, code, null_value, print_field_width,
#'print_decimal_places, angle_of_Section magnetic_Variation, depth,
#'minimum_value, maximum_value, number_valid, number_null
#'
#'polynomial_cal_header has, parameter_name, calibration_date, application_date,
#'number_coefficients, coefficients
#'
#'
#'data columns should be added in order of parameter headers
#'
#' @param b odf structure
#' @param obj oce/ odf (?) object to  pull data and metadata from
#' @param VARNAME name of parameter (in object data is being pulled from)
#' @param cal TRUE/ FALSE whether or not to produce a polynomial cal header for same parameter code
#'
#'
#' @export
#'
#' @examples
#'
#' b <- gen_odfstruct()
#' obj <- read.oce('MCTD****.ODF')
#'
#' b <- add_parameter(b, obj, VARNAME = 'salinity', cal = TRUE)
#'


add_parameter <- function(b, data, VARNAME, cal = FALSE){

  gf3 <- as.gf3(VARNAME)


  length(b$PARAMETER_HEADER) <- length(b$PARAMETER_HEADER) +1

  params <- list()
  for (i in length(b$PARAMETR_HEADER)){
    params[[i]] <- b$PARAMETER_HEADER$CODE
  }
  if (gf3 %in% params){
    gf3$code <- paste(gf3$code, '02', sep = '_')
  } else{
    gf3$code <- paste(gf3$code, '01', sep = '_')
  }

  i <- length(b$PARAMETER_HEADER)
  b$PARAMETER_HEADER[[i]] <-
    list(
      TYPE = 'SING',
      NAME = VARNAME,
      UNITS = gf3$units,
      CODE = gf3$code ,
      NULL_VALUE = '-99',
      PRINT_FIELD_WDITH = gf3$width,
      PRINT_DECIMAL_PLACES = gf3$prec,
      ANGLE_OF_SECTION = '',
      MAGNETIC_VARIATION = '',
      DEPTH = '',
      MINIMUM_VALUE = min(data, na.rm = TRUE),
      MAXIMUM_VALUE = max(data, na.rm = TRUE),
      NUMBER_VALID = '',
      NUMBER_NULL = ''
    )




  if (cal == TRUE){
    length(b$POLYNOMIAL_CAL_HEADER) <- length(b$POLYNOMIAL_CAL_HEADER) +1
    i <- length(b$POLYNOMIAL_CAL_HEADER)

    b$POLYNOMIAL_CAL_HEADER[[i]] <-
      list(
        PARAMETER_NAME = VARNAME,
        CALIBRATION_DATE = '',
        APPLICATION_DATE = '',
        NUMBER_COEFFICIENTS = '',
        COEFFICIENTS = ''
      )

  }


  if (is.null(dim(b$DATA))){
    b$DATA <- matrix(dim = dim(data))
    b$DATA <-  as.matrix(data)
  } else {
    b$DATA <- cbind.data.frame(b$DATA, data)
  }
  i <- length(b$DATA[1,])

  colnames(b$DATA[[i]]) <- gf3$code

  b$DATA <- as.data.frame(b$DATA)

  return(b)
}



as.gf3 <- function(VARNAME){
  load("~/roger/gf3defs.RData")

  if (!( VARNAME %in% gf3defs$CODE)){

  if (VARNAME == 'u'){
    codevar <- 'EWCT'
  }
  if (VARNAME == 'v'){
    codevar <- 'NSCT'
  }
  if (VARNAME == 'w'){
    codevar <- 'VCSP'
  }
  if (VARNAME == 'errv'){
    codevar <- 'ERRV'
  }
  if (VARNAME == 'pgd'){
    codevar <- 'PGDP'
  }
  if (VARNAME == 'agc'){
    codevar <- 'BEAM'
  }
  if (VARNAME == 'sytm'){
    codevar <- 'SYTM'
  }
    if (VARNAME == 'salinity'){
      codevar <- 'PSAL'
    }
    if (VARNAME == 'temperature'){
      codevar <- 'TEMP'
    }
    if (VARNAME == 'sigmaTheta'){
      codevar <- 'SIGP'
    }
    if (VARNAME == 'theta'){
      codevar <- 'POTM'
    }
    if (VARNAME == 'density'){
      codevar <- 'DENS'
    }
    if (VARNAME == 'oxygen'){
      codevar <- 'DOXY'
    }
    if (VARNAME == 'oxygenVoltage'){
      codevar <- 'OXYV'
    }
    if (VARNAME == 'pressure'){
      codevar <- 'PRES'
    }
  }
  if (VARNAME %in% gf3defs$GF3_CODE){
    codevar <- VARNAME
  }

  #add more oce to gf3 code translations
    #eg for ctd, cm, tr, etc
  loc <- gf3defs$GF3_CODE == codevar
  VARNAME <- list(
    code = gf3defs$GF3_CODE[loc],
    def = gf3defs$GF3_DEFINITION[loc],
    units = gf3defs$UNITS[loc],
    width = gf3defs$WIDTH[loc],
    prec = gf3defs$PRECISION[loc]
  )
  return(VARNAME)
}
