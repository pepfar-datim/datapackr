#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom utils URLencode
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
      
  # Pull Community & Facility Sites ####
    siteList <- 
      datapackr::api_call("organisationUnits") %>%
      datapackr::api_filter(paste0(
        "ancestors.id:in:[", paste0(country_uids,collapse = ","),"]")) %>%
      datapackr::api_filter(paste0(
        "organisationUnitGroups.id:in:[POHZmzofoVx,PvuaP6YALSA",
  # Pull Military nodes if requested
         dplyr::if_else(include_mil, ",nwQbMeALRjL", ""),
         "]")) %>%
      datapackr::api_fields("id,name,level,ancestors[id,name],organisationUnitGroups[id,name]") %>%
      datapackr::api_get() %>%
      dplyr::mutate(
        country_uid =
          stringr::str_extract(as.character(ancestors),
                              paste0(country_uids,collapse = "|")))
    
  # Tag Site Type ####
    siteList %<>%
      dplyr::mutate(
        site_type =
          dplyr::case_when(
            stringr::str_detect(as.character(organisationUnitGroups), "PvuaP6YALSA") ~ "Community",
            stringr::str_detect(as.character(organisationUnitGroups), "POHZmzofoVx") ~ "Facility",
            stringr::str_detect(as.character(organisationUnitGroups), "nwQbMeALRjL") ~ "Military"
            )
        )
    
  # Pull levels ####
    countries <- datapackr::api_call("organisationUnits") %>%
      datapackr::api_filter("organisationUnitGroups.id:eq:cNzfcPWEGSH") %>%
      datapackr::api_filter(paste0(
        "path:like:",
        paste0(country_uids,collapse = ","))) %>%
      datapackr::api_fields("id,name") %>%
      datapackr::api_get() %>%
      dplyr::rename(country_name = name)
    
    levels <- datapackr::getIMPATTLevels() %>%
      dplyr::filter(country_name %in% countries$country_name)

  # Tag PSNUs ####
    siteList %<>%
      dplyr::left_join(countries, by = c("country_uid" = "id")) %>%
      dplyr::left_join(levels, by = c("country_name")) %>%
      dplyr::mutate(
        in_region = country_uid %in% 
          (datapackr::dataPackMap %>%
             dplyr::filter(is_region) %>%
             dplyr::pull(country_uid) %>%
             unique()),
        psnu = dplyr::case_when(
          site_type == "Military" ~ name,
          level == prioritization ~ name,
          TRUE ~ purrr::map2_chr(ancestors, prioritization,
                                function(x,y) magrittr::use_series(x, name) %>%
                                  magrittr::extract(y))),
        psnu_uid = dplyr::case_when(
          site_type == "Military" ~ id,
          level == prioritization ~ id,
          TRUE ~ purrr::map2_chr(ancestors, prioritization,
                                 function(x,y) magrittr::use_series(x, id) %>%
                                   magrittr::extract(y))),
        site_tool_label = paste0(
          dplyr::if_else(in_region == TRUE, paste0(country_name," > "), ""),
          dplyr::if_else(site_type == "Military" | prioritization == country,
                         "",
                         paste0(psnu, " > ")),
          name,
          " [#",site_type,"]",
          " [",id,"]"
        )
      )

  return(siteList)
  
}