#' @export
#' @title Unpack a submitted OPU Data Pack
#'
#' @description
#' Processes a submitted OPU Data Pack (in .xlsx format) by identifying integrity
#'     issues, checking data against DATIM validations, and extracting data.
#'
#' @param d datapackr sidecar
#' @param d2_session DHIS2 Session id
#'
#' @details
#' Once this function is called, it will present a dialog box where you can
#' select the file location of the Data Pack to be processed.
#'
#' If Operating Unit \code{name} and \code{id} in the Data Pack's \strong{Home}
#' tab do not match based on cross-reference with DATIM organization
#' hierarchies, you will be prompted to manually select the correct Operating
#' Unit associated with the Data Pack to be processed. Enter this information in
#' the Console as directed.
#'
#' The final message in the Console prints all warnings identified in the Data
#' Pack being processed.
#'
unPackOPUDataPack <- function(d,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {

  # Check integrity of Workbook tabs ####
  d <- checkStructure(d)
  
  # Unpack updated PSNUxIM data ####
  if (d$info$cop_year == 2020) {
    d <- unPackOPU_PSNUxIM(d)
  } else {
    d <- unPackSNUxIM(d)
  }
  
  # Prepare SNU x IM dataset for DATIM import & validation ####
  d <- packForDATIM(d, type = "OPU PSNUxIM")
  
  # Prepare data for sharing with other systems ####
  d <- createAnalytics(d, d2_session = d2_session)

  return(d)
  
}
