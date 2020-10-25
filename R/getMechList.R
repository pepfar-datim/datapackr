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
#' code, partner name, funding agency, and related OU.
#'
getMechList <- function(country_uids = NULL,
                        include_dedupe = FALSE,
                        COP_FY = NULL) {
  
  # Pull Mechs ####
    mechList <- 
      paste0(getOption("baseurl"), "api/",datapackr::api_version(),
             "/sqlViews/fgUtV6e9YIX/data.csv") %>%
      utils::URLencode() %>%
      httr::GET() %>%
      httr::content(., "text") %>%
      readr::read_csv()
    
  # Filter OU ####
    if (!all(is.null(country_uids))) {
      ous <- datapackr::dataPackMap %>%
        dplyr::filter(country_uid %in% country_uids) %>%
        dplyr::pull(level3name) %>%
        unique()
      
      mechList %<>%
        dplyr::filter(ou %in% ous
                      | code %in% c("00000","00001"))
    }
    
  # Filter by Fiscal Year ####
    if (!is.null(COP_FY)) {
      mechList %<>%
        dplyr::filter((startdate < paste0(COP_FY+1,"-10-01") &
                         enddate > paste0(COP_FY,"-09-30"))
                      | code %in% c("00000","00001"))
    }
    
  # Handle Dedupes ####
    if (!include_dedupe) {
      mechList %<>%
        dplyr::filter(!code %in% c("00000","00001"))
    }
    
  return(mechList)
  
}
