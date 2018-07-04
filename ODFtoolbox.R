###ODF_R Toolbox####



####oce2odf####


#'Functions which allow tranfer between oce objects and ODF files
#'
#'author: Emily Chisholm, emily.chisholm "\@\" dfo-mpo.gc.ca
#'date: June 28 2018
#'
#'
#'
#'
#'
#'
#'
#'
#'
NULL




#'oce2odfHeader
#'
#'creates ODF standard header from metadata within an oce object to be copied and replicated in ODF files
#'object can subsequently be used in function `oce2odf`
#'
#'returns empty odf object with header metadata filled out 
#'
#' @param obj oce object

#note before running please write
#obj[['event_comments']]
#obj[['description']]


oce2odfHeader <- function(obj){
  
  b <- gen_odfstruct()
  
  if (inherits(obj) == 'adp'){
    # for ( d in 1:length(obj[['distance']])){
    # b$ODF_HEADER$FILE_SPECIFICATION <- paste('MADCPS', '_', obj[['cruise_number']], '_', obj[['mooring_number']], '_', obj[['serial_number']] , '-', obj[['sensor_depth']] - obj[['distance']][[d]], '.ODF', sep = '')
    # }
    b$CRUISE_HEADER$COUNTRY_INSTITUTE_CODE <- obj[['country_code']]
    b$CRUISE_HEADER$CRUISE_NUMBER <- obj[['cruise_number']]
    b$CRUISE_HEADER$ORGANIZATION <- obj[['organization']]
    b$CRUISE_HEADER$CHIEF_SCIENTIST <- obj[['chief_scientist']]
    b$CRUISE_HEADER$START_DATE <- obj[['time_coverage_start']]
    b$CRUISE_HEADER$END_DATE <- obj[['time_coverage_end']]
    b$CRUISE_HEADER$PLATFORM <- obj[['platform']]
    b$CRUISE_HEADER$CRUISE_NAME <- obj[['cruise_name']]
    b$CRUISE_HEADER$CRUISE_DESCRIPTION <- obj[['cruise_description']]
    
    
    b$EVENT_HEADER$DATA_TYPE <- obj[['data_type']]
    b$EVENT_HEADER$EVENT_NUMBER <- obj[['mooring_number']]
    b$EVENT_HEADER$EVENT_QUALIFIER1 <- ''
    b$EVENT_HEADER$EVENT_QUALIFIER2 <- ''
    b$EVENT_HEADER$CREATION_DATE <- Sys.Date()
    b$EVENT_HEADER$ORIG_CREATION_DATE <- obj[['date_created']]
    b$EVENT_HEADER$START_DATE_TIME <- obj[['time_coverage_start']]
    b$EVENT_HEADER$END_DATE_TIME <- obj[['time_coverage_end']]
    b$EVENT_HEADER$INITIAL_LATITUDE <- obj[['latitude']]
    b$EVENT_HEADER$INITIAL_LONGITUDE <- obj[['longitude']]
    b$EVENT_HEADER$END_LATITUDE <- obj[['latitude']]
    b$EVENT_HEADER$END_LONGITUDE <- obj[['longitude']]
    b$EVENT_HEADER$MIN_DEPTH <- min(obj[['depth']])     #CAUTION THESE ARE ONLY PLACEHOLDERS, EACH FILE SHOULD HAVE 
    b$EVENT_HEADER$MAX_DEPTH <- max(obj[['depth']])     #THESE VALUES REPLACED BY BIN DEPTH IN THE OCE2ODF FUNCTION
    b$EVENT_HEADER$SAMPLING_INTERVAL <- obj[['sampling_interval']]
    b$EVENT_HEADER$SOUNDING <- obj[['sounding']]
    b$EVENT_HEADER$DEPTH_OFF_BOTTOM  <- max(obj[['depth']]) - obj[['depthMean']]
    b$EVENT_HEADER$EVENT_COMMENTS <- obj[['event_comments']]
    
    
    b$INSTRUMENT_HEADER$INST_TYPE <- obj[['instrumentType']]
    b$INSTRUMENT_HEADER$MODEL <- obj[['model']]
    b$INSTRUMENT_HEADER$SERIAL_NUMBER <- obj[['serialNumber']]
    b$INSTRUMENT_HEADER$DESCRIPTION <- obj[['description']]
    
    return(b)
    
    
    
  }
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @param obj oce object for data to be copied to odf
#' @param write whether or not to write out all the odf files produced, default
#'   is TRUE, if false please use binExport to select the bins for which you
#'   would like to produce ODFs



oce2odf <- function(obj, write = TRUE){
  #identify type of oce object
  if (inherits(obj, what = 'adp') ){
    #file names
    names <- list()
    for( d in 1:length(obj[['distance']] )){
      #caUTION: oce uses snake case, ADCP process uses '_'
      names[[d]] <- paste('MADCPS', '_', obj[['cruise_number']], '_', obj[['mooring_number']], '_', obj[['serialNumber']] , '-',  round(obj[['distance']][[d]], digits = 0), '.ODF', sep = '')
      
    }
    #name variables to export to ODF
    params <- list('u', 'v', 'w', 'errv', 'pgd', 'agc')
    
    u <- obj[['v']][,,1]
    v <- obj[['v']][,,2]
    w <- obj[['v']][,,3]
    errv <- obj[['v']][,,4]
    pgd <- obj[['g', 'numeric']][,,4]
    agc <- apply(X = obj[['a', 'numeric']], MARGIN = 1:2, FUN = mean, na.rm = TRUE)   
    #handle time separately
    sytm <- obj[['time']]
    
    #work on add_parameter to make easier
    #split each data variable into single depth time series
    
    #add each section of data as parameter in loop of odf files by depths
    
    #creates data array which matches dimensions of variables, 
    b <- NULL
    for (d in 1: length(obj[['distance']])){
      b[[d]] <- gen_odfstruct()
      b[[d]]$DATA <- matrix(NA,  nrow = length(adp[['time']]),  ncol = length(params))
    }
    
    for (i in 1:length(params)){
      for( d in 1: length(obj[['distance']])){
        eval(parse(text = paste0("b[[d]]$DATA[,i] <- ", params[[i]], "[,d] ")))
        
      }
    }
    
    
    #handle time separately
    for (d in 1:length(obj[['distance']])){
      for(p in params)
        as.data.frame(b[[d]]$DATA)
      
      colnames(b[[d]]$DATA)<- list('EWCT_01', 'NSCT_01','VCSP_01', 'ERRV_01', 'PGDP_01', 'BEAM_01')
    }
    if (!is.null(obj[['time']])){
      SYTM_01 <- as.character(as.POSIXct(obj[['time']], origin = '1970-01-01 00:00:00'))
      for (d in 1:length(obj[['distance']])){
        b[[d]]$DATA <- cbind.data.frame(b[[d]]$DATA, SYTM_01)
      }
    }
    
    gf3 <- list()
    for( p in params){
      gf3[[p]] <- as.gf3(p)
    }
    for ( d in 1:length(obj[['distance']])){
      
      length(b[[d]]$PARAMETER_HEADER) <- length(b[[d]]$PARAMETER_HEADER) + length(params)
      for (i in 1:length(params)){
        b[[d]]$PARAMETER_HEADER[[i]] <- list(
          TYPE= 'SING',
          NAME= gf3[[params[[i]]]]$def,
          UNITS= gf3[[i]]$units,
          CODE= paste(gf3[[i]]$code , '01', sep = '_'),
          NULL_VALUE= '-1000000',
          PRINT_FIELD_WIDTH= as.character(gf3[[i]]$width),
          PRINT_DECIMAL_PLACES= as.character(gf3[[i]]$prec),
          ANGLE_OF_SECTION= '-1000000',
          MAGNETIC_VARIATION= '-1000000',
          DEPTH= round(obj[['depthMean']] - obj[['distance']][[d]], digits = 0),
          MINIMUM_VALUE= as.character(eval(parse(text = paste0("min(", params[[i]], ", na.rm = TRUE)")))),
          MAXIMUM_VALUE= as.character(eval(parse(text = paste0("max(", params[[i]], ", na.rm = TRUE)")))),
          NUMBER_VALID= as.character(eval(parse(text = paste0("length(na.omit(", params[[i]], "[,1]))")))),
          NUMBER_NULL= as.character(eval(parse(text = paste0("length(", params[[i]], "[,1]) - length(na.omit(" ,params[[i]], "[,1]))"))))
        )
      }
      if ( !is.null(obj[['time']])){
        s <- as.gf3('sytm')
        length(b[[d]]$PARAMETER_HEADER) <- length(b[[d]]$PARAMETER_HEADER) + 1
        i <- length(b[[d]]$PARAMETER_HEADER)
        b[[d]]$PARAMETER_HEADER[[i]] <- list(
          TYPE= 'SYTM',
          NAME= s$def,
          UNITS= s$units,
          CODE=  'SYTM_01',
          NULL_VALUE= '-99',
          PRINT_FIELD_WIDTH= s$width,
          PRINT_DECIMAL_PLACES= s$prec,
          ANGLE_OF_SECTION= '-99',
          MAGNETIC_VARIATION= '-99',
          DEPTH= '0',
          MINIMUM_VALUE= toupper(strftime(min(as.character(SYTM_01), na.rm = TRUE),format='%d-%b-%Y %T.00',tz="UTC")),
          MAXIMUM_VALUE= toupper(strftime(max(as.character(SYTM_01), na.rm = TRUE),format='%d-%b-%Y %T.00',tz="UTC")),
          NUMBER_VALID= length(na.omit(SYTM_01)),
          NUMBER_NULL= length(SYTM_01) - length(na.omit(SYTM_01))
        )
      }
    }
    
    
    
    #parameter header, polynomial cal header (optional), compass cal header
    #FIXME: adds to history header with each action like oce processing log
    
    #add header block to each odf file (standard, same for each file)
    
    for( d in 1:length(obj[['distance']])){
      
      #ODF HEADER
      b[[d]]$ODF_HEADER$FILE_SPECIFICATION <- paste('MADCPS', '_', obj[['cruise_number']], '_', obj[['mooring_number']], '_', obj[['serialNumber']] , '-', (round(obj[['depthMean']] - obj[['distance']][[d]], digits = 0)), sep = '')
      
      #CRUISE HEADER
      b[[d]]$CRUISE_HEADER$COUNTRY_INSTITUTE_CODE <- obj[['country_code']]
      b[[d]]$CRUISE_HEADER$CRUISE_NUMBER <- obj[['cruise_number']]
      b[[d]]$CRUISE_HEADER$ORGANIZATION <- obj[['organization']]
      b[[d]]$CRUISE_HEADER$CHIEF_SCIENTIST <- obj[['chief_scientist']]
      b[[d]]$CRUISE_HEADER$START_DATE <- toupper(strftime(obj[['time_coverage_start']],format='%d-%b-%Y %T.00',tz="UTC"))
      b[[d]]$CRUISE_HEADER$END_DATE <- toupper(strftime(obj[['time_coverage_end']],format='%d-%b-%Y %T.00',tz="UTC"))
      b[[d]]$CRUISE_HEADER$PLATFORM <- obj[['platform']]
      b[[d]]$CRUISE_HEADER$CRUISE_NAME <- obj[['cruise_name']]
      b[[d]]$CRUISE_HEADER$CRUISE_DESCRIPTION <- obj[['cruise_description']]
      
      
      b[[d]]$EVENT_HEADER$DATA_TYPE <- obj[['data_type']]
      b[[d]]$EVENT_HEADER$EVENT_NUMBER <- obj[['mooring_number']]
      b[[d]]$EVENT_HEADER$EVENT_QUALIFIER1 <- paste(obj[['serialNumber']],'-',round(obj[['depthMean']] - obj[['distance']][[d]], digits = 0) )
      b[[d]]$EVENT_HEADER$EVENT_QUALIFIER2 <- obj[['sampling_interval']]
      b[[d]]$EVENT_HEADER$CREATION_DATE <- Sys.Date()
      b[[d]]$EVENT_HEADER$ORIG_CREATION_DATE <- toupper(strftime(Sys.Date(),format='%d-%b-%Y %T.00',tz="UTC"))
      b[[d]]$EVENT_HEADER$START_DATE_TIME <- toupper(strftime(obj[['time_coverage_start']],format='%d-%b-%Y %T.00',tz="UTC"))
      b[[d]]$EVENT_HEADER$END_DATE_TIME <- toupper(strftime(obj[['time_coverage_end']],format='%d-%b-%Y %T.00',tz="UTC"))
      b[[d]]$EVENT_HEADER$INITIAL_LATITUDE <- obj[['latitude']]
      b[[d]]$EVENT_HEADER$INITIAL_LONGITUDE <- obj[['longitude']]
      b[[d]]$EVENT_HEADER$END_LATITUDE <- obj[['latitude']]
      b[[d]]$EVENT_HEADER$END_LONGITUDE <- obj[['longitude']]
      b[[d]]$EVENT_HEADER$MIN_DEPTH <- round(obj[['distance']][d] , digits = 0)
      b[[d]]$EVENT_HEADER$MAX_DEPTH <- round(obj[['distance']][d] , digits = 0)    
      b[[d]]$EVENT_HEADER$SAMPLING_INTERVAL <- obj[['sampling_interval']]
      b[[d]]$EVENT_HEADER$SOUNDING <- obj[['sounding']]
      b[[d]]$EVENT_HEADER$DEPTH_OFF_BOTTOM  <- as.numeric(obj[['sounding']]) - obj[['distance']][d]
      b[[d]]$EVENT_HEADER$EVENT_COMMENTS <- paste(as.character(Sys.Date() , obj[['event_comments']]))
      
      # INSTRUMENT_HEADER
      
      b[[d]]$INSTRUMENT_HEADER$INST_TYPE <-'ADCP'
      b[[d]]$INSTRUMENT_HEADER$MODEL <- obj[['model']]
      b[[d]]$INSTRUMENT_HEADER$SERIAL_NUMBER <- obj[['serialNumber']]
      b[[d]]$INSTRUMENT_HEADER$DESCRIPTION <- obj[['description']]
      
      # RECORD_HEADER
      
      b[[d]]$RECORD_HEADER$NUM_CYCLE <- length(obj[['time']])
      b[[d]]$RECORD_HEADER$NUM_PARAM <- length(params) +1
      
      #delete null headers
      b[[d]]$POLYNOMIAL_CAL_HEADER <- NULL
      b[[d]]$COMPASS_CAL_HEADER <- NULL
      b[[d]]$RECORD_HEADER$NUM_CALIBRATION <- NULL
      b[[d]]$RECORD_HEADER$NUM_SWING <- NULL
    }
    
    
    save(b, file = paste0('MADCPS_', obj[['cruise_number']],'_',  obj[['mooring_number']], '_', obj[['sampling_interval']], '.Rd', sep = ''))
    
    
    #write odf sturctures to odf files
    
    #avoid exporting bins aboove surface
    export <- (round(obj[['depthMean']] - obj[['distance']], digits = 0))
    export[export<=0] <- NA
    
    #avoid exporting bins with less than 10% valid data
    for(d in 1:length(obj[['distance']])){
      bd <- length(b[[d]]$DATA)
      bv <- length(na.omit(b[[d]]$DATA))
      
      if( bv/bd < 0.9){
        export[[d]] <- NA
      }
    }
    
    
    if (write == TRUE){
      
      for(d in 1:length(obj[['distance']])){
        if (!is.na(export[[d]])){
        write_odf( b[[d]],   output_file =paste0(b[[d]]$ODF_HEADER$FILE_SPECIFICATION, '.ODF'))
        print(paste0("Bin", d,"of", length(obj[['distance']]),  "completed", sep = " "))
        
        }
        else{
          print(paste("Bin", d, "not exported to ODF!"))
        }
      }
      
    } else{
      return(b)
      
    }
  }
  
  if(inherits(obj, what = 'ctd') ){
    ;
    ;
  }
  
}




####ODF select bin export####


#' Select bin export
#'
#' @param obj an ODF structure object with multiple depth bins (produced from oce2odf)
#' @param bins the bin numbers you wish to export
#'
#' @return
#' @export
#'
#' @examples
#' ```
#' # oce2odf(adp, write = FALSE)
#' # bins <- list(12:34)
#' # binExport(obj = b, bins)
#' ````
binExport <- function(obj, bins){
  
  
  for(l in bins){
    write_odf( obj[[l]],   output_file =paste0(obj[[l]]$ODF_HEADER$FILE_SPECIFICATION))
    
  }
}


####add parameter####

#' add a parameter to an existing ODF structure array (b)
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
#' ```
#' b <- gen_odfstruct()
#' obj <- read.oce('MCTD****.ODF)
#' 
#' b <- add_parameter(b, obj, VARNAME = 'salinity', cal = TRUE)
#' ````


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
    b$DATA <- cbind(b$DATA, data)
  }
  i <- length(b$DATA[1,])
  
  colnames(b$DATA[[i]]) <- gf3$code
  
  b$DATA <- as.data.frame(b$DATA)
  
  return(b)
}


####as GF3####
#' Translate to GF3 codes
#'
#' @param VARNAME the common name for known variables or the GF3 param code if known
#'
#' @return list with GF3 code, def, width, prec and units
#' @export
#'
#' @examples
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


####add parameter####

#' add a parameter to an existing ODF structure array (b)
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
#' ```
#' b <- gen_odfstruct()
#' obj <- read.oce('MCTD****.ODF)
#' 
#' b <- add_parameter(b, obj, VARNAME = 'salinity', cal = TRUE)
#' ````


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

#' write_odf

#' Copyright (C) 2006-2014 DFO, Bedford Institute of Oceanography, Canada.
#' You may distribute under the terms of either the GNU General Public
#' License or the Apache v2 License, as specified in the README file.

#' Description:
#' takes an ODF object and an output location and writes an ODF compliant output file
#' 
#' @param odf_object Object to be written to file
#' @param output_file location and name of the file the ODF object is to be written to
#' 
#' @export 
#' 
#' @author Patrick Upson
#' 
#' @details 
#' date: July 4, 2014
#' 
#' Modified Jun 21, 2018.
#' #
#' R.Pettipas
#' 
#' Changed the accessing of PARAMETER_HEADER fields from
#' #
#'  odf_object$PARAMETER_HEADER[[field]][i] (which returns NULL)
#'  to:
#'  odf_object$PARAMETER_HEADER[i][[1]][[field]]
#' 
#' Changed 
#' prams <- odf_object$PARAMETER_HEADER$CODE
#' to:
#' or (a in 1:length(odf_object$PARAMETER_HEADER)) prams[a] <- B$PARAMETER_HEADER[a][[1]]$CODE
#' 
#'
#' Also added code to change the data type for the "SYTM" parameter to "SYTM" and other values in the parameter header
#' to properly reflect the data type (if not already set to type "SYTM"). Otherwise, the field is output as a floating point value.
#' 
#' Other changes indicated with comment lines "R.Pettipas"
#' 
#' CAVEAT: If the value of TYPE in the parameter header for an SYTM time field is 'SING' and
#' not 'SYTM', the output file will have an integer (POSIXct) format for time and not the expected "DD-MMM-YYYY HH:MM:SS.FF" format 
#' so it is advised to change it before running "write_odf"
#' 
write_odf <- function(odf_object, output_file) {
  
  ODF_HEADER <- define_ODF_header()
  
  DATA = "DATA"
  DATA_LINE <- " -- DATA --"
  DATA_TYPE <- "TYPE"
  
  #The input file is a field added to the ODF object by the read_odf function
  #it's used for interally tracking from what file the object was created
  #but isn't part of the ODF specifiction. It should be ignored when writing
  #ODF objects to file
  INPUT_FILE <- "INPUT_FILE"
  
  DEC_PLACE <- "PRINT_DECIMAL_PLACES"
  COL_WIDTH <- "PRINT_FIELD_WIDTH"
  
  TYPE_DOUB <- "DOUB"
  TYPE_SING <- "SING"
  TYPE_INTE <- "INTE"
  TYPE_SYTM <- "SYTM"
  
  CODE <- "CODE"
  
  if( file.exists(output_file) ) {
    file.remove(output_file)
  }
  
  file.create(output_file)
  
  con <- file(output_file, "wb")
  
  n <- names(odf_object)
  
  # Added R.Pettipas
  # If there is an "SYTM" parameter code, make sure it is of TYPE="SYTM". If not, change it. 
  # Otherwise, the value will be output as a floating point number
  #
  is_sytm <- grep("SYTM",odf_object$PARAMETER_HEADER)
  if (length(is_sytm) > 0) {
    if ((odf_object$PARAMETER_HEADER[is_sytm][[1]]$CODE=="SYTM_01" | odf_object$PARAMETER_HEADER[is_sytm][[1]]$CODE=="SYTM") & (odf_object$PARAMETER_HEADER[is_sytm][[1]]$TYPE != "SYTM")) {
      odf_object$PARAMETER_HEADER[is_sytm][[1]]$TYPE <- "SYTM"
      odf_object$PARAMETER_HEADER[is_sytm][[1]]$PRINT_FIELD_WIDTH <- 27
      odf_object$PARAMETER_HEADER[is_sytm][[1]]$PRINT_DECIMAL_PLACES <- 0
      odf_object$PARAMETER_HEADER[is_sytm][[1]]$MINIMUM_VALUE <-  toupper(strftime(min(odf_object$DATA[[is_sytm]]),format='%d-%b-%Y %T.00',tz="UTC"))
      odf_object$PARAMETER_HEADER[is_sytm][[1]]$MAXIMUM_VALUE <-  toupper(strftime(max(odf_object$DATA[[is_sytm]]),format='%d-%b-%Y %T.00',tz="UTC")) 
    }
  }
  # End R.Pettipas mods
  for( i in 1:length(n) ) {
    parameter = n[i]
    if( n[i] == DATA ) {
      #Print the "Data" containied in the ODF_Object
      
      writeUtf8(DATA_LINE, con)
      
      data <- odf_object$DATA
      rows <- NROW(odf_object$DATA[[1]])
      colNames <- colnames(odf_object$DATA)
      #prams <- odf_object$PARAMETER_HEADER$CODE
      # R.Pettipas, 5 lines below added.....
      n_parms <- length(odf_object$PARAMETER_HEADER)
      for (a in 1:n_parms) {
        if (a==1) prams=character(length=n_parms)
        prams[a] <- odf_object$PARAMETER_HEADER[a][[1]]$CODE
      }
      for( j in 1:rows ) {
        printStr = NULL
        dataStr = NULL
        for( i in 1:length(colNames) ) {
          #					pram_type <- odf_object$PARAMETER_HEADER[[DATA_TYPE]][i]
          pram_type <- odf_object$PARAMETER_HEADER[i][[1]][[DATA_TYPE]]
          #					colWidth <- odf_object$PARAMETER_HEADER[[COL_WIDTH]][i]
          colWidth <- odf_object$PARAMETER_HEADER[i][[1]][[COL_WIDTH]]
          
          #					data[j,i] <- gsub("^'|'$", "", data[j,i])
          
          if( pram_type == TYPE_DOUB || pram_type == TYPE_SING) {
            type <- "f"
            #if the type is a double or single precision value then
            #tack on the number of decimal points that are expected
            #						decimal <- odf_object$PARAMETER_HEADER[[DEC_PLACE]][i]
            decimal <- odf_object$PARAMETER_HEADER[i][[1]][[DEC_PLACE]]
            printStr <- paste("%", colWidth, ".", decimal, type, " ", sep="")
          } else if( pram_type == TYPE_INTE ) {
            #if the type is an integer 
            type <- "i"
            printStr <- paste("%", colWidth, type, " ", sep="")
          } else if( pram_type == TYPE_SYTM ) {
            #if the type is a system time SYTM then surround the value with quotes
            type <- "s"
            printStr <- paste("%", colWidth, type, " ", sep="")
            # R.Pettipas, added to create an SYTM like the conventional definition
            printVal <- paste0("'", toupper(strftime(data[j,i],format='%d-%b-%Y %T.00',tz="UTC")), "'")
          }
          # R.Pettipas Added to convert any NA or NaN values to the default value
          if (is.nan(data[j,i]) || is.na(data[j,i])) data[j,i] <- as.numeric(odf_object$PARAMETER_HEADER[i][[1]]$NULL_VALUE)
          
          #remove quotes from data with existing quotes to avoid double quotes.
          #R.Pettipas created a seperate "paste" for SYTM type
          if (pram_type != TYPE_SYTM) {
            dataStr <- paste(dataStr, sprintf(printStr, data[j, i]), sep=" ")
          } else {
            dataStr <- paste(dataStr, sprintf(printStr, printVal), sep= " ")
          }
        }
        writeUtf8(dataStr, con)
      }
    } else if( n[i] != INPUT_FILE) {
      var_head <- ODF_HEADER[[parameter]]
      #add a comma to the end of the parameter name to be
      #consistant with the current method for writing parameter names in files
      parameter <- paste(parameter, ",", sep="")
      
      #retrieve the variables for this parameter from the object list 
      variable <- odf_object[[n[i]]]
      
      #retrieve the colum names to be printed from the variables
      colName <- names(variable)
      
      if( is.null(colName) ) {
        # if there are no column names then this is a list of unnamed lists
        #so we have to print each sub list independently.
        for( l in 1:length(variable) ) {
          writeUtf8(parameter, con)
          #browser()
          subvar <- variable[[l]]
          
          subnames <- names(subvar)
          for( k in 1:length(subnames) ) {
            val <- subvar[[subnames[k]]]
            for( j in 1:length(val) ) {
              writeValue(subnames[k], val[j], var_head, con)
            }
          }
        }
      } else {
        for( k in 1:length(variable[[1]])) {
          writeUtf8(parameter, con)
          for( j in 1:length(colName) ) {
            
            writeValue(colName[j], variable[[colName[j]]][k], var_head, con)
          }
        }	
      }
    }
  }
  close(con)
}


#' writeValue
#' 
#' Description:
#'  used to set the type of a variable, format the output string and print
#'  the string to a file.
#' 
#' @param var_name ODF variable name being written
#' @param var_val value associated with the ODF varialbe
#' @param var_head ODF header containing information about the ODF variable
#' @param con location data is being written to
#' 
writeValue <- function(var_name, var_val, var_head, con) {
  
  var_row <- grep(var_name, var_head[,1], fixed=TRUE)
  if( length(var_row) > 1 ) {
    for( i in 1:length(var_row) ) {
      if(var_head[var_row[i], 1] == var_name) {
        
        var_row <- var_row[i]
        break
      }
    }
  }
  
  if( var_head[var_row, 2] == "char" ) {
    #browser()
    #if the type of the output is a character string add quotes around the value
    var_val <- paste( "'", var_val, "'", sep="")
  }
  out <- paste("  ", var_name, " = ", var_val, ",", sep="")
  writeUtf8(out, con)
  
}

#' writeUtf8
#' 
#' Description:
#'   Used to write special windows characters like fancy quotes to the file
#' 
#' @param out String to be written to the file
#' @param con connection to the ODF file being written to
#'
writeUtf8 <- function(out, con) {
  out <- paste(out, "\n")
  #browser()
  writeBin(charToRaw(out), con, endian="little")
}

#' Defines and returns the header section of an ODF file.
#' Output:
#'   ODF_HEADER is a cell array containing the definition.
#' 
#'   This tool usually is called by other tools such as ODF read,
#'   write and edit etc. It is one of the core tools and users 
#'   should not make any changes to it.
#'
#'   A new block can be added easily to this definition to extend an
#'   ODF header structure. The new block should be considered as
#'   'optional', otherwise all existing ODF files will have to be
#'   modified as well.
#'
#'   Report any bugs to DataServicesDonnees@@dfo-mpo.gc.ca
#' 
#' ODSToolbox Version: 2.0
#'
#' Last Updated: September 3, 2015
#' 
#' @export 
#' @examples
#' ODF_header <- define_ODF_header()
#' 
#' @details 
#' Source:
#'   Ocean Data and Information Services,
#'   Bedford Institute of Oceanography, DFO, Canada.
#'   DataServicesDonnees@@dfo-mpo.gc.ca
#' 
#' @author Yongcun Hu, Patrick Upson
#'
#' 
#' Copyright (C) 2006-2014 DFO, Bedford Institute of Oceanography, Canada.
#' You may distribute under the terms of either the GNU General Public
#' License or the Apache v2 License, as specified in the README file.

define_ODF_header <- function() {
  tmp = array("", c(2,4))
  tmp[1,] <- c('ODF_HEADER', 'char', 'mandatory', 'single')
  tmp[2,] <- c('FILE_SPECIFICATION', 'char', 'mandatory', 'single')
  ODF <- list(tmp)
  
  tmp = array("", c(10,4))
  tmp[1,] <- c('CRUISE_HEADER', 'char', 'mandatory', 'single')
  tmp[2,] <- c('COUNTRY_INSTITUTE_CODE', 'integer', 'mandatory', 'single')
  tmp[3,] <- c('CRUISE_NUMBER', 'char', 'mandatory', 'single')
  tmp[4,] <- c('ORGANIZATION', 'char', 'mandatory', 'single')
  tmp[5,] <- c('CHIEF_SCIENTIST', 'char', 'mandatory', 'single')
  tmp[6,] <- c('START_DATE', 'char', 'mandatory', 'single')
  tmp[7,] <- c('END_DATE', 'char', 'mandatory', 'single')
  tmp[8,] <- c('PLATFORM', 'char', 'mandatory', 'single')
  tmp[9,] <- c('CRUISE_NAME', 'char', 'mandatory', 'single')
  tmp[10,] <- c('CRUISE_DESCRIPTION', 'char', 'mandatory', 'single')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(19,4))
  tmp[1,] <- c('EVENT_HEADER', 'char', 'mandatory', 'single')
  tmp[2,] <- c('DATA_TYPE', 'char', 'mandatory', 'single')
  tmp[3,] <- c('EVENT_NUMBER', 'char', 'mandatory', 'single')
  tmp[4,] <- c('EVENT_QUALIFIER1', 'char', 'mandatory', 'single')
  tmp[5,] <- c('EVENT_QUALIFIER2', 'char', 'mandatory', 'single')
  tmp[6,] <- c('CREATION_DATE', 'char', 'mandatory', 'single')
  tmp[7,] <- c('ORIG_CREATION_DATE', 'char', 'mandatory', 'single')
  tmp[8,] <- c('START_DATE_TIME', 'char', 'mandatory', 'single')
  tmp[9,] <- c('END_DATE_TIME', 'char', 'mandatory', 'single')
  tmp[10,] <- c('INITIAL_LATITUDE', 'numeric', 'mandatory', 'single')
  tmp[11,] <- c('INITIAL_LONGITUDE', 'numeric', 'mandatory', 'single')
  tmp[12,] <- c('END_LATITUDE', 'numeric', 'mandatory', 'single')
  tmp[13,] <- c('END_LONGITUDE', 'numeric', 'mandatory', 'single')
  tmp[14,] <- c('MIN_DEPTH', 'numeric', 'mandatory', 'single')
  tmp[15,] <- c('MAX_DEPTH', 'numeric', 'mandatory', 'single')
  tmp[16,] <- c('SAMPLING_INTERVAL', 'numeric', 'mandatory', 'single')
  tmp[17,] <- c('SOUNDING', 'numeric', 'mandatory', 'single')
  tmp[18,] <- c('DEPTH_OFF_BOTTOM', 'numeric', 'mandatory', 'single')
  tmp[19,] <- c('EVENT_COMMENTS', 'char', 'mandatory', 'multiple')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(9,4))
  tmp[1,] <- c('METEO_HEADER', 'char', 'optional', 'single')
  tmp[2,] <- c('AIR_TEMPERATURE', 'numeric', 'mandatory', 'single')
  tmp[3,] <- c('ATMOSPHERIC_PRESSURE', 'numeric', 'mandatory', 'single')
  tmp[4,] <- c('WIND_SPEED', 'numeric', 'mandatory', 'single')
  tmp[5,] <- c('WIND_DIRECTION', 'numeric', 'mandatory', 'single')
  tmp[6,] <- c('SEA_STATE', 'numeric', 'mandatory', 'single')
  tmp[7,] <- c('CLOUD_COVER', 'numeric', 'mandatory', 'single')
  tmp[8,] <- c('ICE_THICKNESS', 'numeric', 'mandatory', 'single')
  tmp[9,] <- c('METEO_COMMENTS', 'char', 'mandatory', 'multiple')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(5,4))
  tmp[1,] <- c('INSTRUMENT_HEADER', 'char', 'mandatory', 'single')
  tmp[2,] <- c('INST_TYPE', 'char', 'mandatory', 'single')
  tmp[3,] <- c('MODEL', 'char', 'mandatory', 'single')
  tmp[4,] <- c('SERIAL_NUMBER', 'char', 'mandatory', 'single')
  tmp[5,] <- c('DESCRIPTION', 'char', 'mandatory', 'single')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(4,4))
  tmp[1,] <- c('QUALITY_HEADER', 'char', 'optional', 'single')
  tmp[2,] <- c('QUALITY_DATE', 'char', 'mandatory', 'single')
  tmp[3,] <- c('QUALITY_TESTS', 'char', 'mandatory', 'multiple')
  tmp[4,] <- c('QUALITY_COMMENTS', 'char', 'mandatory', 'multiple')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(9,4))
  tmp[1,] <- c('GENERAL_CAL_HEADER', 'char', 'optional', 'multiple')
  tmp[2,] <- c('PARAMETER_CODE', 'char', 'mandatory', 'single')
  tmp[3,] <- c('CALIBRATION_TYPE', 'char', 'mandatory', 'single')
  tmp[4,] <- c('CALIBRATION_DATE', 'char', 'mandatory', 'single')
  tmp[5,] <- c('APPLICATION_DATE', 'char', 'mandatory', 'single')
  tmp[6,] <- c('NUMBER_COEFFICIENTS', 'integer', 'mandatory', 'single')
  tmp[7,] <- c('COEFFICIENTS', 'numeric', 'mandatory', 'multiple')
  tmp[8,] <- c('CALIBRATION_EQUATION', 'char', 'mandatory', 'multiple')
  tmp[9,] <- c('CALIBRATION_COMMENTS', 'char', 'mandatory', 'multiple')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(6,4))
  tmp[1,] <- c('POLYNOMIAL_CAL_HEADER', 'char', 'optional', 'multiple')
  tmp[2,] <- c('PARAMETER_NAME', 'char', 'mandatory', 'single')
  tmp[3,] <- c('CALIBRATION_DATE', 'char', 'mandatory', 'single')
  tmp[4,] <- c('APPLICATION_DATE', 'char', 'mandatory', 'single')
  tmp[5,] <- c('NUMBER_COEFFICIENTS', 'integer', 'mandatory', 'single')
  tmp[6,] <- c('COEFFICIENTS', 'numeric', 'mandatory', 'multiple')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(6,4))
  tmp[1,] <- c('COMPASS_CAL_HEADER', 'char', 'optional', 'multiple')
  tmp[2,] <- c('PARAMETER_NAME', 'char', 'mandatory', 'single')
  tmp[3,] <- c('CALIBRATION_DATE', 'char', 'mandatory', 'single')
  tmp[4,] <- c('APPLICATION_DATE', 'char', 'mandatory', 'single')
  tmp[5,] <- c('DIRECTIONS', 'numeric', 'mandatory', 'multiple')
  tmp[6,] <- c('CORRECTIONS', 'numeric', 'mandatory', 'multiple')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(3,4))
  tmp[1,] <- c('HISTORY_HEADER', 'char', 'optional', 'multiple')
  tmp[2,] <- c('CREATION_DATE', 'char', 'mandatory', 'single')
  tmp[3,] <- c('PROCESS', 'char', 'mandatory', 'multiple')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(16,4))
  tmp[1,] <- c('PARAMETER_HEADER', 'char', 'mandatory', 'multiple')
  tmp[2,] <- c('TYPE', 'char', 'mandatory', 'single')
  tmp[3,] <- c('NAME', 'char', 'optional', 'single')
  tmp[4,] <- c('UNITS', 'char', 'optional', 'single')
  tmp[5,] <- c('CODE', 'char', 'mandatory', 'single')
  tmp[6,] <- c('WMO_CODE', 'char', 'optional', 'single')
  tmp[7,] <- c('NULL_VALUE', 'char', 'optional', 'single')
  tmp[8,] <- c('PRINT_FIELD_WIDTH', 'integer', 'optional', 'single')
  tmp[9,] <- c('PRINT_DECIMAL_PLACES', 'integer', 'optional', 'single')
  tmp[10,] <- c('ANGLE_OF_SECTION', 'numeric', 'mandatory', 'single')
  tmp[11,] <- c('MAGNETIC_VARIATION', 'numeric', 'mandatory', 'single')
  tmp[12,] <- c('DEPTH', 'numeric', 'mandatory', 'single')
  tmp[13,] <- c('MINIMUM_VALUE', 'numeric', 'optional', 'single')
  tmp[14,] <- c('MAXIMUM_VALUE', 'numeric', 'optional', 'single')
  tmp[15,] <- c('NUMBER_VALID', 'integer', 'optional', 'single')
  tmp[16,] <- c('NUMBER_NULL', 'integer', 'optional', 'single')
  ODF <- c(ODF, list(tmp))
  
  tmp = array("", c(6,4))
  tmp[1,] <- c('RECORD_HEADER', 'char', 'mandatory', 'single')
  tmp[2,] <- c('NUM_CALIBRATION', 'integer', 'optional', 'single')
  tmp[3,] <- c('NUM_SWING', 'integer', 'optional', 'single')
  tmp[4,] <- c('NUM_HISTORY', 'integer', 'optional', 'single')
  tmp[5,] <- c('NUM_CYCLE', 'integer', 'optional', 'single')
  tmp[6,] <- c('NUM_PARAM', 'integer', 'optional', 'single')
  ODF <- c(ODF, list(tmp))
  
  names(ODF) <- c('ODF_HEADER','CRUISE_HEADER','EVENT_HEADER', 'METEO_HEADER',
                  'INSTRUMENT_HEADER','QUALITY_HEADER','GENERAL_CAL_HEADER','POLYNOMIAL_CAL_HEADER',
                  'COMPASS_CAL_HEADER','HISTORY_HEADER','PARAMETER_HEADER','RECORD_HEADER')
  #	if(!is.null(ODF)) {
  #		save('definition_ODF_header.RData');
  #	}
  ODF
}
# end of define_ODF_header.R
# ========================================================================


gen_odfstruct <- function()
{
  #' '''
  #' ------------------------------------------------------------------------
  #' GEN_ODFSTRUCT: Build an empty ODF structured array.
  #' 
  #' ODS Toolbox Version: 2.0
  #' 
  #' Created: 19-OCT-2016
  #' Updated: 30-NOV-2016
  #' 
  #' @author: Jeff Jackson
  #' 
  #' @version: 1.0 
  #' 
  #' @copyright: 2016, Fisheries and Oceans Canada. All Rights Reserved.   
  #' 
  #' @summary: Build an empty ODF structured array.
  #' 
  #' Usage:  C = gen_odfstruct()
  #' 
  #' Input:  none
  #' 
  #' Output:  
  #'   C: An empty ODF structured array
  #' 
  #' Example:  C = gen_odfstruct()
  #' 
  #' Notes:
  #' 
  #' See also CRUISE_EVENT_TO_ORACLE, DATA_TO_ORACLE, ODF_FROM_ORACLE
  #'   
  #' Updates:
  #' 
  #'   Roger Pettipas (21-JUN-2018)
  #'   Changed the time data format to be similar to the classic ODF time string
  #'   
  #'   Jeff Jackson (20-OCT-2016)
  #'   - Finished converting the original Python code to R.
  #' 
  #'   Jeff Jackson (30-NOV-2016)
  #'   - Replaced call to lubridate function now() with the Sys.time() call.
  #' 
  #' Report any bugs to DataServicesDonnees@dfo-mpo.gc.ca
  #' ------------------------------------------------------------------------
  #' '''
  
  curDT = Sys.time();
  # Added R.Pettipas to look more like the classic ODF time specification
  curDT = toupper(strftime(as.POSIXct(curDT,format='%c'),format='%d-%b-%Y %T.00'))
  
  B = list(ODF_HEADER = NA, CRUISE_HEADER = NA, EVENT_HEADER = NA, INSTRUMENT_HEADER = NA,
           POLYNOMIAL_CAL_HEADER = NA, COMPASS_CAL_HEADER = NA, HISTORY_HEADER = NA, 
           PARAMETER_HEADER = NA, RECORD_HEADER = NA, DATA = NA)
  
  B$ODF_HEADER = list(FILE_SPECIFICATION = '')
  
  B$CRUISE_HEADER = list(COUNTRY_INSTITUTE_CODE = '', CRUISE_NUMBER = '', ORGANIZATION = '', 
                         CHIEF_SCIENTIST = '', START_DATE = '', END_DATE = '', PLATFORM = '', 
                         CRUISE_NAME = '', CRUISE_DESCRIPTION = '')
  
  B$EVENT_HEADER = list(DATA_TYPE = '', EVENT_NUMBER = '', EVENT_QUALIFIER1 = '', 
                        EVENT_QUALIFIER2 = '', CREATION_DATE = curDT, ORIG_CREATION_DATE = curDT, 
                        START_DATE_TIME = '', END_DATE_TIME = '', INITIAL_LATITUDE = -99.00, 
                        INITIAL_LONGITUDE = -99.00, END_LATITUDE = -99.00, END_LONGITUDE = -99.00, 
                        MIN_DEPTH = -99.00, MAX_DEPTH = -99.00, SAMPLING_INTERVAL = -99.00, 
                        SOUNDING = -99.00, DEPTH_OFF_BOTTOM = -99.00, EVENT_COMMENTS = '')
  
  B$INSTRUMENT_HEADER = list(INST_TYPE = '', MODEL = '', SERIAL_NUMBER = '', DESCRIPTION = '')
  
  B$POLYNOMIAL_CAL_HEADER = list() 
  
  B$COMPASS_CAL_HEADER = list()
  
  B$HISTORY_HEADER = list(CREATION_DATE = curDT, PROCESS = 'Generation')
  
  B$PARAMETER_HEADER = list()
  
  B$RECORD_HEADER = list(NUM_CALIBRATION = 0, NUM_SWING = 0, NUM_HISTORY = 1, 
                         NUM_CYCLE = 0, NUM_PARAM = 0)
  
  # Add the Data field even though no data will exists yet for the new ODF
  # structure. It is initialized as an empty numpy array.
  B$DATA = NA
  
  return(B)
}


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


