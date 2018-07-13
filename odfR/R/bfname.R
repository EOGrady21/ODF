# R.Pettipas
# Build an ODF filespec from an ODF object
#
bfname <- function(odf_obj) {
  fname <- paste0(odf_obj$EVENT_HEADER$DATA_TYPE,"_",odf_obj$CRUISE_HEADER$CRUISE_NUMBER,"_",odf_obj$EVENT_HEADER$EVENT_NUMBER,"_",
                  odf_obj$INSTRUMENT_HEADER$SERIAL_NUMBER,"_",as.character(odf_obj$EVENT_HEADER$SAMPLING_INTERVAL))
  return(fname)
}