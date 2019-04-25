#' @export
#' @title Unpacks a Site Tool
#'
#' @description
#' Processes a submitted Site Tool (in .xlsx format) by identifying integrity
#'     issues, checking data against DATIM validations, and extracting data.
#'
#' @param d Datapackr object as passed from unPackTool.
#' 
#' @return d
#'     
unPackSiteTool <- function(d) {
  
  # Check integrity of tabs
  d <- checkStructure(d)
  
  # Unpack the Targets
  d <- unPackSheets(d)
  
  # Derive non-Site Tool targets
  d$data$targets <- deriveTargets(d$data$targets, type = "Site Tool")
  
  d <- packForDATIM(d, type = "Site")
   
  return(d)
  
}
