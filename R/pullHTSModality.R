#' @export
#' @title getHTSModality
#' 
#' @description 
#' Map HTS modality to dataElement id
#' 
#' @param dataElements List of dataElements to filter against. (Optional)
#' @param cop_year Specifies COP year for selection of correct modalities.
#' 
#' @return Dataframe of HTS modalities mapped to dataElements
#' 

getHTSModality <- function(cop_year = getCurrentCOPYear(), dataElements = NULL) {

      if (cop_year == 2020) {
    groupSet <- "ra9ZqrTtSQn"
  } else if (cop_year == 2019) {
    groupSet <- "Jm6OwL9IqEa"
  } else if (cop_year == 2018) {
    groupSet <- "CKTkg8dLlr7"}

  modality_map <- datimutils::getDataElementGroupSets(groupSet,
                                    by = "id",
                                    fields = "dataElementGroups[name,dataElements[id]]",
                                    base_url = "https://www.datim.org/")

  modality_map <- modality_map$dataElementGroups[[1]]

  modality_map <- modality_map %>% tidyr::unnest(cols = dataElements) %>%
    dplyr::distinct() %>%
    dplyr::select(dataElement = id,
                  hts_modality = name ) %>%
    dplyr::mutate(hts_modality = stringr::str_remove(hts_modality,"FY\\d{2}R/FY\\d{2}T"))

  if (!is.null(dataElements)) {
    modality_map %<>%
      dplyr::filter(dataElement %in% dataElements)
  }

  return(modality_map)
}