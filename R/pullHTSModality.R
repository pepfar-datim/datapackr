#' @export
#' @title pullHTSModality
#' 
#' @description 
#' Map HTS modality to dataElement id
#' 
#' @param dataElements List of dataElements to filter against. (Optional)
#' @param cop_year Specifies COP year for selection of correct modalities.
#' 
#' @return Dataframe of HTS modalities mapped to dataElements
#' 
pullHTSModality <- function(cop_year = cop_year(), dataElements = NULL) {
  if (cop_year == 2020) {
    groupSet = "ra9ZqrTtSQn"
  } else if (cop_year == 2019) {
    groupSet = "Jm6OwL9IqEa"
  } else if (cop_year == 2018) {
    groupSet = "CKTkg8dLlr7"}
  
  modality_map <- api_call(paste0("dataElementGroupSets/",groupSet)) %>%
    api_fields("dataElementGroups[name,dataElements[id]]") %>%
    api_get() %>%
    tidyr::unnest() %>%
    dplyr::distinct() %>%
    dplyr::select(dataElement = id,
                  hts_modality = name ) %>%
    dplyr::mutate(hts_modality = stringr::str_remove(hts_modality,"FY\\d{2}R/FY\\d{2}T"))
  
  return(modality_map)   
}
