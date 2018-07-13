##profiling code for speed


library(oce)
library(ncdf4)

source('C:/Users/ChisholmE/Documents/ADCP/R/adcpToolbox.R')
source('ODFtoolbox.R')

#       read in raw data and metadata
adp <- read.adp.easy('R:/Shared/ChisholmE/sample ADP process/M1996000.000', 'R:/Shared/ChisholmE/sample ADP process/metadataTemplateM1996.csv')

#       adjust depth values based on pressure and latitude
adp[['depth']] <- swDepth(adp[['pressure']], (adp[['latitude']]))

#       apply magnetic declination
adp <- applyMagneticDeclinationAdp(adp)

#       limit depth by recovery/deployment times
adp <- limit_depthbytime(adp)

#       limit time and other variables based on deployment/ recovery times
adp <- limit_time(adp)

#       add microcat pressure data if relevant
#file <- list.files(path = '.', pattern = "MCTD*...*.ODF")
#adp <- insertInst(adp, var = 'pressure', file = file)


#       flag data outside percent good/ error bounds
adp <- adpFlag(adp, 25, 0.46)

#       set flags to NA
adpClean <- handleFlags(adp, flags = 4, actions = list('NA'))

#       create saved adp object ###fix me: file too big to open, recompress?
save(adp, file =paste( 'adp', adp[['cruise_number']], adp[['mooring_number']], sep = '_'))

#       processingLog export
adp <-exportPL(adp)

#write event comments
adpClean <- oceSetMetadata(adpClean, 'event_comments', 'WRITE EVENT COMMENTS HERE')

#write out all 50 ODF files
#oce2odf(obj = adpClean, write = TRUE)

#export only certain bins
Rprof('C:/Users/ChisholmE/Documents/odf/Odf/timer2.Rd')
b <- oce2odf( adpClean, write = FALSE)
binExport(b, bins = list(1))
Rprof(NULL)

summaryRprof(filename = 'C:/Users/ChisholmE/Documents/odf/Odf/timer2.Rd')
