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
                              pzns = NULL,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {

  #We must assume that the PSNUxIM tab is not needed.
  #This may be overridden if we are dealing with a DataPack + PSNUxIM.
  d$info$has_psnuxim <- TRUE
  d$info$needs_psnuxim <- FALSE
  #Use the existing prioritizations if one is supplied
  d$datim$prioritizations <- pzns


  #Keep the sheets since we are going to need the original targets

  d <- loadSheets(d)
  #TODO: Are we dealing with a PSNUxIM (season is COP)
  #or an OPU (Season is OPU)

  # Check integrity of Workbook tabs ####
  d <- checkToolStructure(d)

  # Check whether there exist any troublesome comments in the file
  d <- checkToolComments(d)

  # Check whether there exist any troublesome connections in the file
  d <- checkToolConnections(d)

  # Unpack updated PSNUxIM data ####
  d <- unPackSNUxIM(d)

  # Prepare SNU x IM dataset for DATIM import & validation ####
  d <- packForDATIM(d, type = "OPU PSNUxIM")

  # Prepare data for sharing with other systems ####
  d <- createAnalytics(d, d2_session = d2_session)

  # Check for invalid mechanisms
  d <- checkMechanisms(d, d2_session = d2_session)


  return(d)

}
