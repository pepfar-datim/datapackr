createKeychainInfo <- function(submission_path = NULL,
                               tool = "Data Pack",
                               country_uids = NULL,
                               cop_year = NULL) {
  
  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      submission_path = submission_path
    ),
    info = list(
      datapack_name = NULL,
      tool = tool,
      country_uids = country_uids,
      cop_year = ifelse(is.null(cop_year),getCurrentCOPYear())
    )
  )
  
  # Start running log of all warning and information messages
  d$info$warning_msg <- NULL
  d$info$has_error <- FALSE
  d$info$newSNUxIM <- FALSE
  d$info$has_psnuxim <- FALSE
  d$info$missing_psnuxim_combos <- FALSE
  
  # Check the submission file exists and prompt for user input if not
  d$keychain$submission_path <- handshakeFile(path = d$keychain$submission_path,
                                              tool = d$info$tool)
  
  
  d
  
}

#' @export
#' @title Unpack a submitted tool
#'
#' @description
#' Processes a submitted Data Pack by identifying integrity issues, checking
#' data against DATIM validations, and extracting data.
#'
#' @param submission_path Local path to the file to import.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' @param country_uids List of 11 digit alphanumeric DATIM codes representing
#' countries. If not provided, will check file for these codes. If not in file,
#' will flag error.
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#' 
#' @details
#' Executes the following operations in relation to a submitted tool
#' \enumerate{
#'     \item Performs integrity checks on file structure;
#' }
#'     
unPackTool <- function(submission_path = NULL,
                       tool = "Data Pack",
                       country_uids = NULL,
                       cop_year = NULL) {

  d<-createKeychainInfo(submission_path,
                     tool,
                     country_uids,
                     cop_year)
  
  # unPack file based on type
  if (d$info$tool == "Data Pack") {
    d <- unPackDataPack(d)
  } else {stop("Please select correct file type: Data Pack.")}
  
  # If warnings, show all grouped by sheet and issue
  if (!is.null(d$info$warning_msg) & interactive()) {
    options(warning.length = 8170)
    
    messages <-
      paste(
        paste(
          seq_along(d$info$warning_msg),
          ": " , d$info$warning_msg
          #stringr::str_squish(gsub("\n", "", d$info$warning_msg))
        ),
        sep = "",
        collapse = "\r\n")
    
    key = paste0(
      "*********************\r\n",
      "KEY:\r\n",
      "- WARNING!: Problematic, but doesn't stop us from processing your tool.\r\n",
      "- ERROR!: You MUST address these issues and resubmit your tool.\r\n",
      "*********************\r\n\r\n")
    
    cat(crayon::red(crayon::bold("VALIDATION ISSUES: \r\n\r\n")))
    cat(crayon::red(key))
    cat(crayon::red(messages))
  }

  return(d)
}
