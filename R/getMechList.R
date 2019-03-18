#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom utils URLencode
#' @title Compile Mechanism List for Site Tool
#' 
#' @description
#' Extracts full list of valid mechanisms from DATIM for Fiscal Year & countries
#' specified and compiles these into the list as needed for use in the PEPFAR
#' Site Tool.
#'
#' @param country_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit uids will not be accepted. If not
#' supplied, returns entire mechanism list, trimmed to user's DATIM permissions.
#' @param include_dedupe Logical. If TRUE will include deduplication mechanisms.
#' Default is FALSE.
#' @param COP_FY Numeric value of COP Fiscal Year to filter mechanism list by.
#' Ex: For mechanisms active in FY 2020, pertaining to COP 2019, enter
#' \code{2019}. If a FY is not supplied, returns entire mechanism list.
#' 
#' @return A dataframe of mechanisms, including start and end dates, mechanism
#' code, partner name, funding agency, and related country.
#'
getMechList <- function(country_uids = NA,
                        include_dedupe = FALSE,
                        COP_FY = NA) {
  
  # Check user has correct permissions to query against country_uids ####
    # TODO: Configure to allow non-global users to generate Site Tools
  
  # Pull Mechs ####
    mechList <- 
      paste0(getOption("baseurl"), "api/",datapackr::api_version(),
             "/categoryOptionCombos.json",
             "?paging=false",
             "&filter=categoryCombo.name:eq:Funding%20Mechanism",
             "&fields=id,name,code,categoryOptions[startDate,endDate,categoryOptionGroups[id,name,groupSets[id,name]],organisationUnits[id,name]]"
             ) %>%
      utils::URLencode() %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      jsonlite::fromJSON(., flatten = TRUE) %>%
      do.call(rbind.data.frame, .)
    
  # Tag Metadata ####
    mechList %<>%
      dplyr::mutate(
        start_date = as.Date(
          purrr::map_chr(categoryOptions, "startDate", .default = NA)),
        end_date = as.Date(
          purrr::map_chr(categoryOptions, "endDate", .default = NA)),
        organisation_unit_name =
          purrr::map_chr(categoryOptions,
                         list("organisationUnits",1,"name"),
                         .default = NA),
        organisation_unit_id =
          purrr::map_chr(categoryOptions,
                         list("organisationUnits",1,"id"),
                         .default = NA),
        categoryOptionGroups =
          purrr::map(categoryOptions,
                     list("categoryOptionGroups",1),
                     .default = NA)) %>%
      dplyr::filter(!is.na(categoryOptionGroups)) %>%
      dplyr::mutate(
        partner = 
          purrr::map_chr(categoryOptionGroups,
            function(x) dplyr::filter(x,stringr::str_detect(as.character(groupSets),"Partner")) %>%
              magrittr::use_series(name)),
        agency = 
          purrr::map_chr(categoryOptionGroups,
             function(x) dplyr::filter(x,stringr::str_detect(as.character(groupSets),"Agency")) %>%
               magrittr::use_series(name))
      ) %>%
      dplyr::select(-categoryOptions, -categoryOptionGroups)
    
  # Filter country_uids ####
    if (!is.na(country_uids)) {
      mechList %<>%
        dplyr::filter(organisation_unit_id %in% country_uids
                        | code %in% c("00000","00001"))
    }
    
  # Filter by Fiscal Year ####
    if (!is.na(COP_FY)) {
      mechList %<>%
        dplyr::filter((start_date < paste0(COP_FY+1,"-10-01") &
                         end_date > paste0(COP_FY,"-09-30"))
                      | code %in% c("00000","00001"))
    }
    
  # Handle Dedupes ####
    if (!include_dedupe) {
      mechList %<>%
        dplyr::filter(!code %in% c("00000","00001"))
    }
    
  return(mechList)
  
}