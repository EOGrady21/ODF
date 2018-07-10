
#' Copyright (C) 2006-2014 DFO, Bedford Institute of Oceanography, Canada.
#' You may distribute under the terms of either the GNU General Public
#' License or the Apache v2 License, as specified in the README file.

#' @author Patrick Upson


parameterReshape <- function(paramList) {
	nameArray = NULL
	param = NULL
	
	#if the parameter list has more than one element, but has no names
	#then it's a list of lists
	if( length(names(paramList) ) <= 0 ) {
		for( i in 1:length(paramList) ){
			nameArray <- c(nameArray, names(paramList[[i]]))
		}
		nameArray <- unique(nameArray)
	
		param = data.frame(matrix(1, nrow=length(paramList), ncol=length(nameArray)))
		
		for( i in 1:length(paramList) ) {
			for( j in 1:length(nameArray)) {
				param[i,j] <- paramList[[i]][[nameArray[j]]]
			}
		}
	} else {
		nameArray = names(paramList)
		param = data.frame(matrix(1, nrow=1, ncol=length(nameArray)))
		for( j in 1:length(nameArray)) {
			param[j] <- paramList[[nameArray[j]]]
		}
	}
	
	names(param) <- nameArray
	
	param
}
