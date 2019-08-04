#' @export
#' @title Unpack a submitted Data Pack
#'
#' @description
#' Processes a submitted Data Pack (in .xlsx format) by identifying integrity
#'     issues, checking data against DATIM validations, and extracting data.
#'
#' @param d Datapackr object
#'
#' @details
#' Executes the following operations in relation to a submitted Data Pack:
#' \enumerate{
#'     \item Performs integrity checks on file structure;
#'     \item Runs DATIM validation tests;
#'     \item Extracts SUBNAT and IMPATT data as a DATIM import file;
#'     \item Extracts FAST data for use by the FAST Tool; and
#   \item Extracts MER data for use by the \code{\link{packSiteTool}}
#    function.
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
unPackDataPack <- function(d) {

  # Determine country uids
    if (is.na(d$info$country_uids)) {
      d$info$country_uids <- 
        unPackCountryUIDs(submission_path = d$keychain$submission_path,
                          tool = d$info$tool)
    }
  
  # Store schema
    d$info$schema <- datapackr::data_pack_schema
    
  # Check integrity of Workbook tabs
    d <- checkStructure(d)

  # Unpack the Targets
    d <- unPackSheets(d)
    
  # Separate Data Sets
    d <- separateDataSets(d)

  # Unpack the SNU x IM sheet
    d <- unPackSNUxIM(d)

  # Combine Targets with SNU x IM for PSNU x IM level targets
    d <- rePackPSNUxIM(d)
    
  # Double check country_uid info # TODO: Replace this with API call against SQL view of sites mapped to Countries.
    # site_uids <-
    #   datapackr::api_call("organisationUnits") %>%
    #   datapackr::api_filter("organisationUnitGroups.name:in:[Military,COP Prioritization SNU]") %>%
    #   datapackr::api_filter(
    #     paste0(
    #       "ancestors.id:in:[",
    #       paste0(d$info$country_uids,
    #              collapse = ","),
    #       "]")
    #   ) %>%
    #   datapackr::api_fields("id") %>%
    #   datapackr::api_get() %>%
    #   dplyr::pull(id)
    # 
    # dp_PSNU_uids <- d$data$distributedMER$psnuid %>%
    #   unique()
    # 
    # country_uid_check <- dp_PSNU_uids[site_uids]
    
    # Where data uids !%in% DATIM list, flag error (Need to provide correct uids in either param or DP home tab)
    
  # Prepare SNU x IM dataset for DATIM validation checks
    d <- packForDATIM(d, type = "PSNUxIM")

  # Package FAST export
    d <- FASTforward(d)
    
  # Package SUBNAT/IMPATT export
    d <- packForDATIM(d, type = "SUBNAT_IMPATT")
    
  return(d)

}
