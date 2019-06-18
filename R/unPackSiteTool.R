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
  
  # Determine country uids
  if (is.na(d$info$country_uids)) {
    d$info$country_uids <- 
      unPackCountryUIDs(submission_path = d$keychain$submission_path,
                        tool = d$info$tool)
  }
  
  # Check integrity of tabs
  d <- checkStructure(d)
  
  # Unpack the Targets
  d <- unPackSheets(d)
  
  # Tag Data Pack name & Country Names/uids
  d$info$datapack_name <- readxl::read_excel(
    d$keychain$submission_path,
    sheet = "Home",
    range = "B20") %>%
    names()
  
  site_uids <- d$data$targets %>%
    dplyr::pull(site_uid) %>%
    unique() %>%
    paste0(collapse = ",")
  
  countries <-
    datapackr::api_call("organisationUnits") %>%
    datapackr::api_filter("organisationUnitGroups.name:eq:Country") %>%
    datapackr::api_filter(paste0("children.id:in:[",site_uids,"]")) %>%
    datapackr::api_fields("id,name") %>%
    datapackr::api_get()
  
  d$info$country_names <- countries$name
  d$info$country_uids <- countries$id
  
  # Derive non-Site Tool targets
  d$data$targets <- deriveTargets(d$data$targets, type = "Site Tool")
  
  d <- packForDATIM(d, type = "Site")
  
  return(d)
  
}
