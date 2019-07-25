#' @export
#' @title Extract COP indicators list from DATIM
#' 
#' @description
#' Queries DATIM to return list of COP indicators for given COP year.
#' 
#' @param cop_year Year of COP for which to return indicator list. (e.g., for 
#' COP19 enter 19.) If left blank, will use COP Year as stored in datapackr.
#' 
#' @return Dataframe of COP indicators retrieved from DATIM
#'
pull_COPindicators <- function(cop_year = datapackr::cop_year()) {
  indicators <- datapackr::api_call("indicators") %>%
    datapackr::api_filter(field = "indicatorGroups.name",
                          operation = "eq",
                          match = paste("COP",cop_year,"indicators")) %>%
    datapackr::api_fields("code,id,name,numeratorDescription,numerator,denominatorDescription,denominator,indicatorType[id,name],indicatorGroups[id,name]") %>%
    datapackr::api_get()
  
}