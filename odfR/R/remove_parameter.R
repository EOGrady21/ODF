#'   REMOVE_PARAMETER
#'   Removes a parameter from the oce / ODF object.
#'
#'   ODSToolbox Version: 2.0
#'
#'   Created: 10-SEP-2015
#'   Updated: 22-SEP-2015
#'
#'   @author: Jeff Jackson
#'
#'   @version: 1.0
#'
#'   @copyright: Fisheries and Oceans Canada
#'
#'   @summary: Removes a parameter from the oce / ODF object.
#'
#'   @example
#'
#'   Usage:  remove_parameter(S, code)
#'
#'   Input:
#'           S: An oce object
#'        code: A valid GF3 code (ex: code='FFFF_01')
#'
#'   Output:  The modified oce object
#'
#'   Example:  A = remove_parameter(ODF, 'PSAL_02')
#'
#'
#'
#'
#'   @updates
#'
#'
#'     Jeff Jackson (21-SEP-2015)
#'     - Fixed a minor error.
#'
#'   Report any bugs to DataServicesDonnees@dfo-mpo.gc.ca


remove_parameter <- function(S, code) {

  # Clear variable values.
  param <- NULL
  cal <- NULL

  # Get the parameter headers.
  paramHeaders <- S@metadata$odfHeader$PARAMETER_HEADER

  # Get the number of parameters.
  np <- length(paramHeaders)

  # Phase A: Search the ODF object's to find the code to be removed.
  for (i in 1:np) {
    cp <- paramHeaders[[i]]$CODE
    wp <- paramHeaders[[i]]$WMO_CODE

    # Phase B: Remove the parameter if present.
    if (!is.null(cp)) {
      if (identical(cp, code)) {
        # Step 1) Remove data from oce object.
        S@data[i] <- NULL

        # Step 2) Remove the Parameter_Header from odfheader.
        S@metadata$odfHeader$PARAMETER_HEADER[i] <- NULL

        cat(paste("The code ", code, " has been removed.\n", sep = ""))
      }
    }
    if (!is.null(wp)) {
      if (identical(wp, substr(code,1,4))) {
          # Step 1) Remove data from oce object.
        S@data[i] <- NULL

        # Step 2) Remove the Parameter_Header from odfheader.
        S@metadata$odfHeader$PARAMETER_HEADER[i] <- NULL

        cat(paste("The code ", code, " has been removed.\n", sep = ""))
      }
    }
  }

  paramHeaders <- S@metadata$odfHeader$PARAMETER_HEADER

#   # Step 3) Remove the General_Cal_Header if present.
#   if isfield(S(i),'General_Cal_Header') && ~isempty(S(i).General_Cal_Header)
#     P = cat(1,S(i).General_Cal_Header{:});
#     try
#       names = cat(1,P.Parameter_Code);
#       I = find(strncmp(cellstr(names), code, length(code)) == 1);
#       J = setdiff((1:m),I);
#       [cal{1:length(J),1}] = deal(S(i).General_Cal_Header{J});
#       S(i).General_Cal_Header = {};
#       [S(i).General_Cal_Header{1:length(J)}] = deal(cal{:});
#       S(i).Record_Header.Num_Calibration = length(S(i).General_Cal_Header);
#     catch ME1
#       print('No data field %s ' % code)
#       rethrow(ME1);
#     end
#   end
#   '''
#
#   # Step 4) Remove the Polynomial_Cal_Header if present.
#   if S['POLYNOMIAL_CAL_HEADER'] is not None:
#     npch = len(S['POLYNOMIAL_CAL_HEADER'])
#     for x in range(0,npch):
#       print(S['POLYNOMIAL_CAL_HEADER'][x]['PARAMETER_NAME'])


#   if isfield(S(i),'Polynomial_Cal_Header') && ~isempty(S(i).Polynomial_Cal_Header)
#     P = cat(1,S(i).Polynomial_Cal_Header{:});
#     fields = cat(1,P.Parameter_Name);
#     I = find(strncmp(cellstr(fields), code, length(code)) == 1);
#     [m,~] = size(fields);
#     J = setdiff((1:m),I);
#     [cal{1:length(J),1}] = deal(S(i).Polynomial_Cal_Header{J});
#     S(i).Polynomial_Cal_Header = {};
#     [S(i).Polynomial_Cal_Header{1:length(J)}] = deal(cal{:});
#     S(i).Record_Header.Num_Calibration = length(S(i).Polynomial_Cal_Header);
#
#     # Step 5) Add a line to the history header to indicate that the parameter
#     # has been removed.
#     S = add_history(S, ['The following Parameter was removed from the ODF file: ', code]);
#   }

#   # Phase C: Try to remove QQQQ field if present.
#     # Step 1) Remove data.
#     clear names param;
#     S(i).Data = rmfield(S(i).Data,code2);
#
#     # Step 2) Remove the Parameter_Header.
#     for j = 1:length(S(i).Parameter_Header)
#       names{j} = char(S(i).Parameter_Header{j}.Code);
#     end
#     IQ = find(strncmp(cellstr(names), code2, length(code2)) == 1);
#     [m,~] = size(names);
#     J = setdiff((1:m),IQ);
#     [param{1:length(J),1}] = deal(S(i).Parameter_Header{J}); ##ok<*AGROW>
#     S(i).Parameter_Header = {};
#     [S(i).Parameter_Header{1:length(J),1}] = deal(param{:});
#     S(i).Record_Header.Num_Param = length(S(i).Parameter_Header);

  # Step 5) Add a line to the history header to indicate that the
  # parameter's accompanying QQQQ field has been removed.
  processingLog(S) <- paste("The following Parameter was removed from the ODF object: ", code, sep = "")

  return(S)
}
