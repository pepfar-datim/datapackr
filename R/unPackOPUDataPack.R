#' @export
#' @title Unpack a submitted OPU Data Pack
#'
#' @description
#' Processes a submitted OPU Data Pack (in .xlsx format) by identifying integrity
#'     issues, checking data against DATIM validations, and extracting data.
#'
#' @param d datapackr sidecar
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

  # Grab datapack_name from Home Page
  d$info$datapack_name <- unPackDataPackName(
    submission_path = d$keychain$submission_path)  
  
  # Determine country uids ####
  if (is.null(d$info$country_uids)) {
    d$info$country_uids <- 
      unPackCountryUIDs(submission_path = d$keychain$submission_path,
                        tool = d$info$tool)
  }
  
  # Store schema ####
  if (d$info$cop_year == 2020) {
    d$info$schema <-  datapackr::cop20OPU_data_pack_schema
  } else {stop("That COP Year currently isn't supported for processing OPU Data Packs.")}
  
  # Check integrity of Workbook tabs ####
  d <- checkStructure(d)
  
  # Unpack updated PSNUxIM data ####
  d <- unPackOPU_PSNUxIM(d)
  
  # Prepare data for sharing with other systems ####
  d <- createAnalytics(d,
                       d2_session = d2_session)
  
  # Prepare SNU x IM dataset for DATIM import & validation ####
  d <- packForDATIM(d, type = "OPU PSNUxIM")
  
  return(d)
  
}
