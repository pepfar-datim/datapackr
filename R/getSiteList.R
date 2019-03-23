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
  
  #loginToDATIM(getOption("secrets"))
  
  country_names <- datapackr::dataPackMap %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    dplyr::pull(country_name)
      
  # Is this country in a regional program? ####
    isRegion <- datapackr::dataPackMap %>%
      dplyr::filter(country_uid %in% country_uids) %>%
      dplyr::select(country_uid,is_region)
      
  # Pull Sites ####
    siteList <- 
      paste0(getOption("baseurl"), "api/",datapackr::api_version(),
             "/organisationUnits.json?paging=false&filter=ancestors.id:in:[",
             paste0(country_uids,collapse = ","),"]",
             "&filter=organisationUnitGroups.id:in:[POHZmzofoVx,PvuaP6YALSA]",
              "&fields=id,name,level,ancestors[id,name],organisationUnitGroups[id,name]") %>%
      utils::URLencode() %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      jsonlite::fromJSON(., flatten = TRUE) %>%
      do.call(rbind.data.frame, .) %>%
      dplyr::mutate(
        country_uid =
          stringr::str_extract(as.character(ancestors),
                              paste0(country_uids,collapse = "|")))
    
  # Add new new countries not in DATIM ####
    newCountries <- datapackr::dataPackMap %>%
      dplyr::filter(country_uid %in% stringr::str_subset(country_uids,"TBD")) %>%
      dplyr::mutate(
        id = country_uid,
        level = 4,
        ancestors = list(list(name = NA_character_, id = NA_character_)),
        organisationUnitGroups = list(list(name = NA_character_, id = NA_character_))
      ) %>%
      dplyr::select(level,
                    name = country_name,
                    id,
                    ancestors,
                    organisationUnitGroups,
                    country_uid)
    
    siteList %<>%
      dplyr::bind_rows(newCountries)
        
  # Tag Site Type ####
    siteList %<>%
      dplyr::mutate(
        site_type =
          dplyr::case_when(
            stringr::str_detect(id,"^TBD000000\\d{2}") ~ "National",
            stringr::str_detect(as.character(organisationUnitGroups), "PvuaP6YALSA") ~ "Community",
            stringr::str_detect(as.character(organisationUnitGroups), "POHZmzofoVx") ~ "Facility"))
    
  # Test Site Types ####
    levels <- datapackr::dataPackMap %>%
      dplyr::filter(country_uid %in% country_uids) %>%
      dplyr::select(country_uid,country,prioritization,planning,community,facility,mil_level)
    
    if (!isTRUE(dplyr::all_equal(
          (levels %>%
              tidyr::drop_na(community) %>%
              dplyr::mutate(community = as.double(community)) %>%
              dplyr::select(country_uid,level = community)),
          (siteList %>%
            dplyr::filter(site_type == "Community") %>%
            dplyr::select(country_uid,level) %>%
            unique()
           )
          ))
        ) {
      stop(paste0("Oh my! How embarassing. There's a mismatch in Community level designations in DATIM for: ",
                  paste0(country_names, collapse = ", "),
                  ". Better check it out."))
    }
    
    if (!isTRUE(dplyr::all_equal(
          (levels %>%
             tidyr::drop_na(facility) %>%
             dplyr::mutate(facility = as.double(facility)) %>%
             dplyr::select(country_uid,level = facility)),
          (siteList %>%
            dplyr::filter(site_type == "Facility") %>%
            dplyr::select(country_uid,level) %>%
            unique()
          )
        ))
      ) {
        stop(paste0("Oh my! How embarassing. There's a mismatch in Facility level designations in DATIM for: ",
                    paste0(country_names, collapse = ", "),
                    ". Better check it out."))
        }
      
  # Add _Military sites ####
    if (include_mil) {
      milNode <- datapackr::dataPackMap %>%
        dplyr::filter(country_uid %in% country_uids) %>%
        dplyr::mutate(
          ancestors = list(list(name = NA_character_, id = NA_character_)),
          organisationUnitGroups = list(list(name = NA_character_, id = NA_character_)),
          site_type = "Military") %>%
        dplyr::select(level = mil_level,
                      name = mil_psnu,
                      id = mil_psnu_uid,
                      ancestors,
                      organisationUnitGroups,
                      country_uid,
                      site_type)
      
      siteList %<>%
        dplyr::bind_rows(milNode)
    }

  # Tag PSNUs ####
    siteList %<>%
      dplyr::left_join(levels, by = c("country_uid")) %>%
      dplyr::left_join((datapackr::dataPackMap %>%
                          dplyr::select(country_name, country_uid, is_region)),
                       by = c("country_uid")) %>%
      dplyr::mutate(
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
          dplyr::case_when(is_region == TRUE ~ paste0(country_name," > "), TRUE ~ ""),
          dplyr::if_else(site_type == "Military","",paste0(psnu, " > ")),
          name,
          " [#",site_type,"]",
          " [",id,"]"
        )
      )

  return(siteList)
  
}