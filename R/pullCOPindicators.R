#' @export
#' @title Extract COP indicators list from DATIM
#' 
#' @description
#' Queries DATIM to return list of COP indicators for given COP year.
#' 
#' @param cop_year Year of COP for which to return indicator list. (e.g., for 
#' COP19 enter 19.) If left blank, will use COP Year as stored in datapackr.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Dataframe of COP indicators retrieved from DATIM
#'
pull_COPindicators <- function(cop_year = datapackr::getCurrentCOPYear(),
                               d2_session = dynGet("d2_default_session",
                                                   inherits = TRUE)) {
  indicators <- datapackr::api_call("indicatorGroups",
                                    d2_session = d2_session) %>%
    datapackr::api_filter(field = "name",
                          operation = "eq",
                          match = paste("COP",stringr::str_sub(cop_year,start = -2),"DataPack Indicators")) %>%
    datapackr::api_fields("indicators[code,id,name,numeratorDescription,numerator,denominatorDescription,denominator,indicatorType[id,name]]") %>%
    datapackr::api_get(d2_session = d2_session) %>% 
    purrr::pluck("indicators") %>% 
    purrr::pluck(1)
  
  row.names(indicators) <- NULL
  
  return(indicators)
  
}