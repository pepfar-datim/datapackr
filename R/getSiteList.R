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
#' @param org_unit_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit uids will be converted to a list of 
#' descendant country uids
#' @param include_mil When TRUE, will append _Military organisation nodes to
#' site list.
#'
getSiteList <- function(org_unit_uids,
                        include_mil = TRUE) {

  country_uids <- purrr::map(org_unit_uids, CountriesContained) %>% 
    purrr::reduce(c)  %>% 
    unique()
  
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
        "id:in:[",
        paste0(country_uids,collapse = ","),
        "]")) %>%
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

#' @importFrom magrittr %>% %<>%
#' @title CountriesContained
#' 
#' @description
#' Given an org unit uid, returns uids for all countries at or below the UID in the hierarchy. E.g. global
#' would return all countries' uids, but the main use case is getting a list of
#' countries in a region
#'
#' @param org_unit_uid single org unit uid as a character string
#' @param base_url DHIS2 URL to use as base query.
#' 
#' @return character vector of country uids, length of 0 if no countries at or below
#' the org unit
#'
CountriesContained <- function(org_unit_uid, base_url = getOption("baseurl")) {
#TODO have this make the api call using a standard function once developed such as getMetadata
# in dataPackCommons
  assertthat::assert_that(stringr::str_length(org_unit_uid) == 11)
# get list of countries using the country orgUnitGroup = cNzfcPWEGSH
  r <- paste0(base_url, "api/organisationUnitGroups/cNzfcPWEGSH.csv?fields=organisationUnits[name,id,path]") %>% 
    httr::GET(httr::timeout(180)) %>% 
    httr::content(as = "text") %>% 
    readr::read_csv(col_names = TRUE, col_types = readr::cols(.default = "c"))
  
  dplyr::filter(r, stringr::str_detect(path, org_unit_uid)) %>% .$id %>% as.vector()
}
