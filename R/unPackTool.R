#' @export
#' @title Unpack a submitted tool
#'
#' @description
#' Processes a submitted Data Pack, Site Tool, Mechanism Map, or Site Filter by
#' identifying integrity issues, checking data against DATIM validations, and
#' extracting data.
#'
#' @param submission_path Local path to the file to import.
#' @param output_path A local path directing to the folder where you would like
#' outputs to be saved. If not supplied, will output to working directory.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' Other options include "Site Tool", "Mechanism Map", and "Site Filter".
#' 
#' @details
#' Executes the following operations in relation to a submitted Site Tool
#' \enumerate{
#'     \item Performs integrity checks on file structure;
#' }
#'     
unPackTool <- function(submission_path = NA,
                       output_path = getwd(),
                       tool = "Data Pack") {
  
  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      submission_path = submission_path,
      output_path = output_path
    ),
    info = list(
      tool = tool
    )
  )
  
  # Check the submission file exists and prompt for user input if not
  if (d$info$tool %in% c("Data Pack", "Site Tool", "Mechanism Map")) {
    extension = "xlsx"
  } else if (d$info$tool == "Site Filter") {
    extension = "csv"
  } else {stop("Cannot process that kind of tool.")}
  
  d$keychain$submission_path <- handshakeFile(path = d$keychain$submission_path,
                                              type = "standard",
                                              extension = extension)
  
  # Start running log of all warning and information messages
  d$info$warning_msg <- NULL
  d$info$has_error <- FALSE
  
  if (d$info$tool == "Data Pack") {
    d <- unPackDataPack(d)
  } else if (d$info$tool == "Site Tool") {
    d <- unPackSiteTool(d)
  } else if (d$info$tool == "Mechanism Map") {
    d <- unPackMechanismMap(d)
  } else if (d$info$tool == "Site Filter") {
    d <- unPackSiteFilter(d)
  } else {stop("Cannot process that kind of tool.")}
  
  # If warnings, show all grouped by sheet and issue
  if (!is.null(d$info$warning_msg) & interactive()) {
    options(warning.length = 8170)
    
    messages <-
      paste(
        paste(
          seq_along(d$info$warning_msg),
          ": " ,
          stringr::str_squish(gsub("\n", "", d$info$warning_msg))
          ),
        sep = "",
        collapse = "\r\n")
    cat(crayon::red("WARNING MESSAGES: \r\n"))
    cat(crayon::red(messages))
    
  }
  
  return(d)
  
}
