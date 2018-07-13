###producing ODFs in R EXAMPLE####



obj <- adpClean
obj <- oceSetMetadata(obj, 'event_comments', 'WRITE EVENT COMMENTS HERE')

oce2odf(obj = obj, write = TRUE)

#export only certain bins
#b <- oce2odf( obj, write = FALSE)
#binExport(b, bins = list(1, 2, 3))
