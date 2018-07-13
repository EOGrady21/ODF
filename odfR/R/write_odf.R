#' write_odf
#'
#'
#' takes an ODF object and an output location and writes an ODF compliant output file
#'
#' Copyright (C) 2006-2014 DFO, Bedford Institute of Oceanography, Canada.
#' You may distribute under the terms of either the GNU General Public
#' License or the Apache v2 License, as specified in the README file.
#'
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
