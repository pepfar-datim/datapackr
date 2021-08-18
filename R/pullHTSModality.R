#' @export
#' @title getHTSModality
#'
#' @description
#' Map HTS modality to dataElement id
#'
#' @param dataElements List of dataElements to filter against. (Optional)
#' @param cop_year Specifies COP year for selection of correct modalities.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Dataframe of HTS modalities mapped to dataElements
#'
getHTSModality <- function(cop_year = getCurrentCOPYear(), dataElements = NULL,
                           d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {
  if (cop_year %in% c(2020,2021)) {
    groupSet = "ra9ZqrTtSQn"
  } else if (cop_year == 2019) {
    groupSet = "Jm6OwL9IqEa"
  } else if (cop_year == 2018) {
    groupSet = "CKTkg8dLlr7"}

  modality_map <- api_call(paste0("dataElementGroupSets/",groupSet),
                           d2_session = d2_session) %>%
    api_fields("dataElementGroups[name,dataElements[id]]") %>%
    api_get(d2_session = d2_session) %>%
    tidyr::unnest(cols = dataElements) %>%
    dplyr::distinct() %>%
    dplyr::select(dataElement = id,
                  hts_modality = name ) %>%
    dplyr::mutate(hts_modality = stringr::str_trim(stringr::str_remove(hts_modality,"FY\\d{2},\\d{2}R/FY\\d{2},\\d{2}T")))
    

  if (!is.null(dataElements)) {
    modality_map %<>%
      dplyr::filter(dataElement %in% dataElements)
  }

  return(modality_map)
}
