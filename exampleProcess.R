###example processing procedure
###input: RAW ADCP file (.000), plus any extra metadata from log sheets (.csv)
###output: processed ADCP data in netCDF file

library(oce)
library(ncdf4)

#source('adcpToolbox.R')

#       read in raw data and metadata
adp <- read.adp.easy('R:/Shared/ChisholmE/sample ADP process/M1996000.000', 'R:/Shared/ChisholmE/sample ADP process/metadataTemplateM1996.csv')


#general first look plots

plot(adp, which = 1)  #u
plot(adp, which = 2)  #v
plot(adp, which = 3)  #w
plot(adp, which = 4)  #error
plot(adp, which = 15) #pressure
plot(adp, which = 23) #progressive vector for u
plot(adp, which = 28) #uv scatter plot


#save bin plots to pdf
#paste('binbybinplot', adp[['mooring_number']], '.pdf', sep = '') #name pdf
pdf('test00006.pdf' , width = 8, height = 40 ) #save to pdf
par(mfrow = c(15, 1)) #set number of plots per page (rows, columns)
plotBin(adp@data$v[,,1])
dev.off() #close pdf

#       adjust depth values based on pressure and latitude
adp[['depth']] <- swDepth(adp[['pressure']], (adp[['latitude']]))

#       apply magnetic declination
adp <- applyMagneticDeclinationAdp(adp)


#check plots
#     looking for rotation compared to plots before magnetic declination applied
plot(adp, which = 23) #progressive vector for u
plot(adp, which = 28) #uv scatter plot

#       limit depth by recovery/deployment times
adp <- limit_depthbytime(adp)

#check plots
#     looking for any spikes on either end of dataset
plot(adp[['depth']])

#       limit time and other variables based on deployment/ recovery times
adp <- limit_time(adp)

#check plots
#     looking for pressure spikes on either end
plot(adp, which = 15) 


#       add microcat pressure data if relevant
#file <- list.files(path = '.', pattern = "MCTD*...*.ODF")
#adp <- insertInst(adp, var = 'pressure', file = file)
#compare pressure by plotting
#plot(adp[['pressure_alternate']], lty = 2, col = 'red', ylim = c(0, 200), xlab = 'Time', ylab = 'Pressure dBar')
#lines(adp[['pressure']], lty = 1, col = 'black', ylim = c(0,200), xlab = '', ylab = '')




#       flag data outside percent good/ error bounds
adp <- adpFlag(adp, 25, 0.46)

#       set flags to NA:
#       use adpClean to plot and check data quality, adp still maintains
#       complete data set integrity
adpClean <- handleFlags(adp, flags = 4, actions = list('NA'))

#check plots
#     plot velocity beams
plot(adpClean, which = 1)
plot(adpClean, which = 2)
plot(adpClean, which = 3)
plot(adpClean, which = 4)
#     plot echo intensity
plot_ei(adpClean)


#     check any other relvant plots to confirm QC before exporting
pdf('test00008.pdf' , width = 8, height = 40 ) #save to pdf
par(mfrow = c(15, 1)) #set number of plots per page (rows, columns)
plotQC(adp, QC = 'v')
dev.off() #close pdf


#       create saved adp object ###fix me: file too big to open, recompress?
save(adp, file =paste( 'adp', adp[['cruise_number']], adp[['mooring_number']], sep = '_'))

#       create standard name
name <- name.file(adp)

#       processingLog export
adp <-exportPL(adp)
adp <- oceSetMetadata(adp, 'history', 'INSERT SHORT SUMMARY OF HISTORY HERE')

#       export to netCDF file
oceNc_create(adp, 'test0000007', metadata = 'R:/Shared/ChisholmE/sample ADP process/metadataTemplateM1996.csv')
