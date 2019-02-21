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
      cntry_uid <- country_uids[i]
      cntry_name <- datapackr::dataPackMap %>%
        dplyr::filter(country_uid == cntry_uid) %>%
        dplyr::pull(country_name)
      
  # Is this country in a regional program?
      isRegion <- datapackr::dataPackMap %>%
        dplyr::filter(country_uid == cntry_uid) %>%
        dplyr::pull(is_region)
      
  # Pull Sites
      siteList <- 
        paste0(getOption("baseurl"), "api/",datapackr::api_version(),
               "/organisationUnits.json?paging=false&filter=ancestors.id:eq:",
               cntry_uid,
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
      
  # Add _Military sites
      if (include_mil) {
        milNode <- datapackr::dataPackMap %>%
          dplyr::filter(country_uid == cntry_uid) %>%
          dplyr::mutate(
            ancestors = list(list(name = NA_character_, id = NA_character_)),
            organisationUnitGroups = list(list(name = NA_character_, id = NA_character_)),
            site_type = "Military") %>%
          dplyr::select(level = mil_level,
                        name = mil_psnu,
                        id = mil_psnu_uid,
                        ancestors,
                        organisationUnitGroups,
                        site_type)
        
        siteList %<>%
          dplyr::bind_rows(milNode)
      }
        
  # Test Site Levels
      levels <- datapackr::dataPackMap %>%
        dplyr::filter(country_uid == cntry_uid) %>%
        dplyr::select(country,prioritization,planning,community,facility,mil_level)
      
      if (levels$community != (siteList %>%
                                dplyr::filter(site_type == "Community") %>%
                                dplyr::pull(level) %>%
                                unique())) {
        stop(paste0("Oh my! How embarassing. There's a mismatch in Community level designations in DATIM for ",
                    cntry_name,
                    ". Better check it out."))
      }
      
      if (levels$facility != (siteList %>%
                               dplyr::filter(site_type == "Facility") %>%
                               dplyr::pull(level) %>%
                               unique())) {
        stop(paste0("Oh my! How embarassing. There's a mismatch in Facility level designations in DATIM for ",
                    cntry_name,
                    ". Better check it out."))
      }

  # Tag PSNUs
      siteList %<>%
        dplyr::mutate(
          psnu = dplyr::case_when(
            site_type == "Military" ~ name,
            level == levels$prioritization ~ name,
            TRUE ~ purrr::map_chr(ancestors,
                                  function(x) magrittr::use_series(x, name) %>%
                                    magrittr::extract(levels$prioritization)
                                 )
            ),
          psnu_uid = dplyr::case_when(
            site_type == "Military" ~ id,
            level == levels$prioritization ~ id,
            TRUE ~ purrr::map_chr(ancestors,
                                  function(x) magrittr::use_series(x, id) %>%
                                    magrittr::extract(levels$prioritization)
                                 )
            ),
          site_tool_label = paste0(
            dplyr::case_when(isRegion ~ paste0(cntry_name," > "), TRUE ~ ""),
            dplyr::if_else(site_type == "Military","",paste0(psnu, " > ")),
            name,
            " (#",site_type,")",
            " (",id,")"
          )
        )
    }
  
  return(siteList)
  
}