
  #' generate ODF structure
  #'
  #' creates an empty list structure to populate and export as ODF
  #'
  #' @version ODS Toolbox Version: 2.0
  #'
  #' @update
  #' Created: 19-OCT-2016
  #' Updated: 30-NOV-2016
  #'
  #' @author: Jeff Jackson
  #'
  #'
  #'
  #' @copyright: 2016, Fisheries and Oceans Canada. All Rights Reserved.
  #'
  #' @example
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
  #'   @update
  #'
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
  #' @export


gen_odfstruct <- function(){


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
