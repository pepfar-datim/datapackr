#' @export
#' @title Create Keychain Info for use in datapackr sidecar.
#'
#' @description
#' Creates Keychain info needed for use across most datapackr unPack functions.
#'
#' @param submission_path Local path to the file to import.
#' @param tool What type of tool is the submission file? Default is "Data Pack".
#' @param country_uids List of 11 digit alphanumeric DATIM codes representing
#' countries. If not provided, will check file for these codes. If not in file,
#' will flag error.
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#'
createKeychainInfo <- function(submission_path = NULL,
                               tool = "Data Pack",
                               country_uids = NULL,
                               cop_year = NULL) {


  #Attempt to bootstrap the tool type and COP year if it is not explicitly provided
  tool_name<-readxl::read_excel(
    path = submission_path,
    sheet = "Home",
    range = "B10", #May need to be a global variable
    col_types = "text", 
    col_names = FALSE) %>% 
    stringi::stri_split_fixed(pattern = " ",n=2 ) %>% 
    unlist()
  
  if (is.null(tool)) {
    
    tool<-tool_name[2]
  }
  
  if (is.null(cop_year)) {
    
    cop_year<-gsub("COP","20",tool_name[1])
  
    }
  
  
  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      submission_path = submission_path
    ),
    info = list(
      datapack_name = NULL,
      tool = tool,
      country_uids = country_uids,
      cop_year = ifelse(is.null(cop_year),getCurrentCOPYear(), cop_year)
    )
  )

  # Start running log of all warning and information messages
  d$info$warning_msg <- NULL
  d$info$has_error <- FALSE
  if (d$info$tool %in% c("Data Pack", "Data Pack Template") & d$info$cop_year %in% c("2020", "2021")) {
    d$info$newSNUxIM <- FALSE
    d$info$has_psnuxim <- FALSE
    d$info$missing_psnuxim_combos <- FALSE
    d$info$missing_DSNUs <- FALSE
  }

  # Check the submission file exists and prompt for user input if not
  d$keychain$submission_path <- handshakeFile(path = d$keychain$submission_path,
                                              tool = d$info$tool)

  return(d)

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
                       cop_year = NULL,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {
  d <- createKeychainInfo(submission_path,
                     tool,
                     country_uids,
                     cop_year)

  # unPack file based on type
  if (d$info$tool == "Data Pack") {
    d <- unPackDataPack(d)
  } else if (d$info$tool == "OPU Data Pack") {
    d <- unPackOPUDataPack(d,
                           d2_session = d2_session)
  } else {stop("Selected tool not currently supported.")}

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
