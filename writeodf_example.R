###producing ODFs in R EXAMPLE####


source('R:/Shared/ChisholmE/sample ADP process/adcpToolbox.R')
source('R:/Shared/ChisholmE/sample ADP process/exampleProcess.R')

source('R:/Shared/ChisholmE/ODF_R/add_parameter.R')
source('R:/Shared/ChisholmE/ODF_R/define_ODF_header.R')
source('R:/Shared/ChisholmE/ODF_R/gen_odfstruct.R')
source('R:/Shared/ChisholmE/ODF_R/write_odf.R')
source('R:/Shared/ChisholmE/ODF_R/oce2odf.R')


obj <- adpClean
obj <- oceSetMetadata(obj, 'event_comments', 'WRITE EVENT COMMENTS HERE')

oce2odf(obj = obj, write = TRUE)

#export only certain bins
#b <- oce2odf( obj, write = FALSE)
#binExport(b, bins = list(1, 2, 3))
