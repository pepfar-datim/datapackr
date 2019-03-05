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
#' @param FY Numeric value of Fiscal Year to filter mechanism list by. If a FY
#' is not supplied, returns entire mechanism list.
#' 
#' @return A dataframe of mechanisms, including start and end dates, mechanism
#' code, partner name, funding agency, and related country.
#'
getMechList <- function(country_uids = NA,
                        FY = NA) {
  
  datapackr::loginToDATIM(getOption("secrets"))
  
  # Check user has correct permissions to query against country_uids ####
    # TODO: Configure to allow non-global users to generate Site Tools
  
  # Pull Sites ####
    mechList <- 
      paste0(getOption("baseurl"), "api/",datapackr::api_version(),
             "/categoryOptionCombos.json",
             "?paging=false",
             "&filter=categoryCombo.name:eq:Funding%20Mechanism",
             dplyr::if_else(
               !all(is.na(country_uids)),
               paste0("&filter=categoryOptions.organisationUnits.id:in:[",
                      paste0(country_uids,collapse = ","),"]"),
                      ""),
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
        start_date = as.Date(purrr::map_chr(categoryOptions,
                                     function(x) magrittr::use_series(x, startDate))),
        end_date = as.Date(purrr::map_chr(categoryOptions,
                                  function(x) magrittr::use_series(x, endDate))),
        organisation_unit_name =
          purrr::map_chr(categoryOptions,
            function(x) magrittr::use_series(x, organisationUnits) %>%
              magrittr::extract2(1) %>%
              magrittr::use_series(name)),
        organisation_unit_id =
          purrr::map_chr(categoryOptions,
            function(x) magrittr::use_series(x, organisationUnits) %>%
              magrittr::extract2(1) %>%
              magrittr::use_series(id)),
        categoryOptionGroups =
          purrr::map(categoryOptions,
            function(x) magrittr::use_series(x, categoryOptionGroups) %>%
              magrittr::extract2(1) %>%
              dplyr::filter(stringr::str_detect(as.character(groupSets),"Partner|Agency"))),
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
  
  # Filter by Fiscal Year ####
    if (!is.na(FY)) {
      mechList %<>%
        dplyr::filter(start_date < paste0(FY,"-10-01"),
                      end_date > paste0(FY-1,"-09-30"))
    }
    
  return(mechList)
  
}