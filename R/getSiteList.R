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
#'
getSiteList <- function(country_uids,
                        include_mil = TRUE) {
  
  datapackr::loginToDATIM(getOption("secrets"))
  
  # Check user has correct permissions to query against country_uids
    # TODO: Configure to allow non-global users to generate Site Tools
  
  # Loop through country uids
    for (i in 1:country_uids) {
      country_id <- country_uids[i]
      
  # Is this country in a regional program?
      isRegion <- datapackr::dataPackMap %>%
        dplyr::filter(country_uid == country_id) %>%
        dplyr::pull(is_region)
      
  # Pull Sites
      siteList <- 
        paste0(getOption("baseurl"), "api/",api_version(),
               "/organisationUnits.json?paging=false&filter=ancestors.id:eq:",
                 country_id,
               "&filter=organisationUnitGroups.id:in:[POHZmzofoVx,PvuaP6YALSA]",
                "&fields=id,name,level,ancestors[id,name],organisationUnitGroups[id,name]") %>%
        httr::GET() %>%
        httr::content(., "text") %>%
        jsonlite::fromJSON(., flatten = TRUE) %>%
        do.call(rbind.data.frame, .)
        
  # Tag Site Type
      siteList %<>%
        dplyr::mutate(
          site_type =
            dplyr::case_when(
              stringr::str_detect(as.character(organisationUnitGroups), "PvuaP6YALSA") ~ "Community",
              stringr::str_detect(as.character(organisationUnitGroups), "POHZmzofoVx") ~ "Facility"))
        
  # Test Site Levels
      levels <- datapackr::dataPackMap %>%
        dplyr::filter(country_uid == country_id) %>%
        dplyr::select(country,prioritization,planning,community,facility,mil_level)
      
      if (levels$community != (siteList %>%
                                dplyr::filter(site_type == "Community") %>%
                                dplyr::pull(level) %>%
                                unique())) {
        warning("Oh my! How embarassing. There's a mismatch in Community level
                designations in DATIM. Better check it out.")
      }
      
      if (levels$facility != (siteList %>%
                               dplyr::filter(site_type == "Facility") %>%
                               dplyr::pull(level) %>%
                               unique())) {
        warning(
          paste0("Oh my! How embarassing. There's a mismatch in Facility level designations in DATIM for ",
                 (datapackr::dataPackMap %>%
                   dplyr::filter(country_uid == country_id) %>%
                   dplyr::pull(country_name)),
                ". Better check it out."))
      }

  # Tag PSNUs
      sj <- siteList %>%
        dplyr::mutate(
          psnu = purrr::map_chr(ancestors,
              function(x) magrittr::use_series(x, name) %>%
              magrittr::extract(3)
            )
          )
        
        
        
    
  }
  
}