#' @export
#' @importFrom magrittr %>% %<>%
#' @title Compile Site List for Site Tool
#' 
#' @description
#' Extracts full list of sites from DATIM, including _Military nodes where
#' specified, and compiles these into the list as needed for use in the PEPFAR
#' Site Tool.
#'
#' @param country_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit uids will not be accepted
#' @param include_mil When TRUE, will append _Military organisation nodes to
#' site list.
#' @param secrets A local path directing to a file containing DATIM login
#' credentials. See \code{\link{loginToDATIM}} for more details.
#'
getSiteList <- function(country_uids,
                        include_mil = TRUE,
                        secrets) {
  
  loginToDATIM(secrets)
  
  # Check user has correct permissions to query against country_uids
    # TODO: Configure to allow non-global users to generate Site Tools
  
  # Loop through country uids
    for (i in 1:country_uids) {
      country_uid <- country_uids[i]
      
      # Is this country in a regional program?
        isRegion <- datapackr::configFile %>%
          dplyr::filter(countryUID == country_uid) %>%
          dplyr::pull(isRegion) %>%
          as.logical()
      
      # Pull Sites
        siteList <- 
          paste0(getOption("baseurl"), "api/",api_version(),
                 "/organisationUnits.json?paging=false&filter=ancestors.id:eq:",
                   country_uid,
                 "&filter=organisationUnitGroups.id:in:[POHZmzofoVx,PvuaP6YALSA]",
                  "&fields=id,name,level,ancestors[id,name],organisationUnitGroups[id,name]") %>%
          httr::GET() %>%
          httr::content(., "text") %>%
          jsonlite::fromJSON(., flatten = TRUE) %>%
          do.call(rbind.data.frame, .)
        
      # Tag Site Type
          sj <- siteList %>%
          dplyr::mutate(
            site_type = dplyr::case_when(stringr::str_detect(as.character(organisationUnitGroups), "PvuaP6YALSA") ~ "Community",
                                         stringr::str_detect(as.character(organisationUnitGroups), "POHZmzofoVx") ~ "Facility"))
        
      # Test Site Levels
        levels <- NULL
        
      
        
        
        
      # Tag PSNUs
        
        
        
        
        
    
  }
  
}