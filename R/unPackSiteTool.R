#' @export
#' @title unPackSiteToolData(submission_path,output_path,archive_results)
#'
#' @description
#' Processes a submitted Site Tool (in .xlsx format) by identifying integrity
#'     issues, checking data against DATIM validations, and extracting data.
#'
#' @param submission_path Local path to the file to import. 
#' @param output_path A local path directing to the folder where you would like
#' outputs to be saved. If not supplied, will output to working directory.
#' @param archive_results Results as an RDS object. 
#' 
#' @details
#' Executes the following operations in relation to a submitted Site Tool
#' \enumerate{
#'     \item Performs integrity checks on file structure;
#' }
#'     
unPackSiteToolData <- function(submission_path = NA,
                       output_path = NA ,
                       archive_results = FALSE) {
  
  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      submission_path = submission_path,
      output_path = output_path
    )
  )
  
  if (is.na(output_path)) {
    d$keychain$output_path = getwd()
  }
  
  can_read_import_file <- function(submission_path) {
    
    if (is.na(submission_path)) { return(FALSE)}
    
    file.access(submission_path,4) == 0
  }
  
  if ( !can_read_import_file( submission_path ) & interactive() ) {
    interactive_print("Please choose a submission file.")
    d$keychain$submission_path <- file.choose() } else
    {
      d$keychain$submission_path <- submission_path
    }
  
  msg<-"Checking the file exists..."
  interactive_print(msg)
  
  if (!can_read_import_file(d$keychain$submission_path)) {
    stop("Submission workbook could not be read!")
  }
  
  if ( tools::file_ext(d$keychain$submission_path) != "xlsx" ) {
    stop("File must be an XLSX file!")
  }
  
  # Start running log of all warning and information messages
  d$info$warningMsg <- NULL
  d$info$has_error<-FALSE
  
  # Check OU name and UID match up
  interactive_print("Checking the OU name and UID on HOME tab...")
  d <- checkSiteToolOUinfo(d)
  
  # # Check integrity of site tool tabs
  d <- checkSiteToolStructure(d)
  # 
  # # Unpack the Targets
  d <- unPackSiteToolSheets(d)
  
  # # Prepare SNU x IM dataset for DATIM validation checks
  d <- packForDATIM(d, type = "Site")
  
  # 
  # # If warnings, show all grouped by sheet and issue
   if (!is.null(d$info$warningMsg) & interactive()) {
     options(warning.length = 8170)
     
     messages <-
       paste(paste(
         seq_along(d$info$warningMsg),
         ": " ,
         stringr::str_squish(gsub("\n", "", d$info$warningMsg))
       ),
       sep = "",
       collapse = "\r\n")
     cat(crayon::red("WARNING MESSAGES: \r\n"))
     cat(crayon::red(messages))
     
   }

  
  if (archive_results == TRUE) {
     
     exportPackr(d,
                 d$keychain$output_path,
                 type = "Site Tool",
                 d$info$datapack_name)
   }
   
   return(d)
  
}
