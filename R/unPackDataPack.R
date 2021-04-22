#' @export
#' @title Unpack a submitted Data Pack
#'
#' @description
#' Processes a submitted Data Pack (in .xlsx format) by identifying integrity
#'     issues, checking data against DATIM validations, and extracting data.
#'
#' @param d Datapackr object
#' @param d2_session DHIS2 Session ID
#'
#' @details
#' Executes the following operations in relation to a submitted Data Pack:
#' \enumerate{
#'     \item Performs integrity checks on file structure;
#'     \item Runs DATIM validation tests;
#'     \item Extracts SUBNAT and IMPATT data as a DATIM import file;
#' }
#'
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
#' the Console as directed.
#'
#' The final message in the Console prints all warnings identified in the Data
#' Pack being processed.
#'
unPackDataPack <- function(d,
                           d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {


  # Check whether there exist any troublesome comments in the file
  interactive_print("Checking comments...")
  d <- checkComments(d)

  # Check integrity of Workbook tabs ####
  interactive_print("Checking structure...")
  d <- checkStructure(d)

  # Unpack the Targets ####
  interactive_print("Unpacking sheets...")
  d <- unPackSheets(d)

  # Separate Data Sets ####
  interactive_print("Separating datasets...")
  d <- separateDataSets(d)

  # Unpack the SNU x IM sheet ####
  interactive_print("Unpacking the PSNUxIM tab...")
  d <- unPackSNUxIM(d)

  # Prepare undistributed import file for use in analytics if necessary ####
  d <- packForDATIM(d, type = "Undistributed MER")

  # Package SUBNAT/IMPATT DATIM import file ####
  d <- packForDATIM(d, type = "SUBNAT_IMPATT")

  # Prepare SNUxIM dataset for DATIM import & validation ####
  if (d$info$has_psnuxim) {
    d <- packForDATIM(d, type = "PSNUxIM")
  }
  
  # Create Analytics Function ####
  interactive_print("Creating analytics...")
  d <- createAnalytics(d, d2_session = d2_session )
  
  # TEST: Check that country_uids matches observed data
  observed_country_uids <-
    dplyr::bind_rows(d$datim) %>%
    dplyr::select(orgUnit) %>%
    dplyr::distinct() %>%
    dplyr::left_join(datapackr::valid_PSNUs %>%
                       dplyr::select(psnu_uid, country_name, country_uid),
                     by = c("orgUnit" = "psnu_uid")) %>%
    dplyr::pull(country_uid) %>%
    unique()

  if (!all(purrr::map_lgl(observed_country_uids, 
                         ~ .x %in% d$info$country_uids))) {
    stop("Deduced or provided Country UIDs do no match Country UIDs observed in submission.")
  }

  return(d)

}
