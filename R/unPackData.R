
#' Unpack a submitted Data Pack
#'
#' @description
#' Take a Data Pack file (in .xlsx format) and process it by:
#'
#'   1. Performing integrity checks on file structure
#'   2. Running DATIM validation tests
#'   3. Extracting SUBNAT and IMPATT data as a DATIM import file.
#'   4. Extracting FAST data for use by the FAST Tool
#'   5. Extracting MER data for use by the \code{\link{packSiteTool}} function.
#'
#' @param support_files_path A local path directing to a folder containing
#' required support files.
#' @param output_path A local path directing to the folder where you would like
#' outputs to be saved.
#' @param secrets A local path directing to a file containing DATIM login
#' creds. See Details for more explanation.
#'
#' @details
#' If a Data Pack is submitted as an XLSB formatted document, you must open &
#' re-save as an XLSX in order to process it with this function.
#'
#' Once this function is called, it will present a dialog box where you can
#' select the file location of the Data Pack to be processed.
#'
#' If Operating Unit \code{name} and \code{id} in the Data Pack's \strong{Home}
#' tab do not match based on cross-reference with DATIM organization
#' hierarchies, you will be prompted to manually select the correct Operating
#' Unit associated with the Data Pack to be processed. Enter this information in
#' the console as directed.
#'
#' FAST and SUBNAT/IMPATT extracts are saved to \code{output_path} as CSV files.
#'
#' The final message in the console prints all warnings identified in the Data
#' Pack being processed.
#'
#' @section secrets file
#' To securely connect with DATIM, use a secrets JSON file structured as follows:
#'
#' \code{
#' {
#'   "dhis": {
#'       "baseurl": "https://www.datim.org/",
#'       "username": "example",
#'       "password": "3x@mpl3!"
#'    }
#'  }
#' }
#'
#'To log into other instances of DATIM, alter the \code{baseurl}.
#'
#' @examples
#' # Supply parameters and
#' round_trunc(0.5)
#' round_trunc(-0.5)
unPackData <- function(support_files_path, output_path, secrets) {

  # Create data train for use across remainder of program
  d <- list(
    keychain = list(
      support_files_path = support_files_path,
      output_path = output_path,
      secrets = secrets
    )
  )

  # Allow User to choose file
  d$keychain$submission_path <- file.choose()

  # Check the file exists
  print("Checking the file exists...")
  if (!file.exists(d$keychain$submission_path)) {
    stop("Submission workbook could not be read!")
  }

  # Start running log of all warning messages
  d$info$warningMsg <- NULL

  # Check OU name and UID match up
  print("Checking the OU name and UID on HOME tab...")
  d <- checkOUinfo(d)

  # Check integrity of Workbook tabs
  d <- checkWorkbookStructure(d)

  # Unpack the Targets
  d <- unPackSheets(d)

  # Unpack the SNU x IM sheet
  d <- unPackSNUxIM(d)

  # Combine Targets with SNU x IM for PSNU x IM level targets
  d <- rePackPSNUxIM(d)

  # Package FAST export
  d <- FASTforward(d)
  #TODO add parameter above for whether to export FAST data
  d$keychain$FAST_file_name <- paste0(
    d$keychain$output_path,
    if (is.na(stringr::str_extract(d$keychain$output_path,"/$"))) {"/"} else {},
    d$info$datapack_name,"_",
    "FASTExport_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    ".csv"
  )
  readr::write_csv(d$data$FAST, d$keychain$FAST_file_name)
  print(paste0("Successfully saved FAST export to ", d$keychain$FAST_file_name))

  # Package SUBNAT/IMPATT export
  d <- packSUBNAT_IMPATT(d)

  #TODO add parameter above for whether to export SUBNAT/IMPATT data
  d$keychain$SUBNAT_IMPATT_filename <- paste0(
    d$keychain$output_path,
    if (is.na(stringr::str_extract(d$keychain$output_path,"/$"))) {"/"} else {},
    d$info$datapack_name,"_",
    "SUBNAT_IMPATT_Export_",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    ".csv"
  )
  readr::write_csv(d$datim$SUBNAT_IMPATT, d$keychain$SUBNAT_IMPATT_filename)
  print(paste0("SUBNAT/IMPATT data is ready for DATIM import and available here: ", d$keychain$SUBNAT_IMPATT_filename))

  # If warnings, show all grouped by sheet and issue
  if(!is.null(d$info$warningMsg)) {
    options(warning.length = 8170)
    warning(paste0("
                   ",d$info$warningMsg))
  }

  #TODO Archive entire d object as rds for records

  }
