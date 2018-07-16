####readodf2oce####
##using oce library

#read ctd
ctd <- read.ctd.odf(file = 'CTD_BCD2017666_01_01_DN.ODF')


#read current meter
o <- read.odf(file = 'mcm/MCM_HUD2013008_1844_602_3600.ODF')
mcm <- as.cm(o)




#read any object without oce class
d <- read.odf(file = 'mtr/MTR_HUD2015030_1898_10546422_1800.ODF')
oce <- new('oce')
oce@data <- d@data
oce@metadata <- d@metadata

