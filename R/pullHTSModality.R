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
  groupSet <- switch(
    as.character(cop_year),
    "2022" = "bEktFhmEKn6",
    "2021" = "ra9ZqrTtSQn",
    "2020" = "ra9ZqrTtSQn",
    "2019" = "Jm6OwL9IqEa",
    "2018" = "CKTkg8dLlr7"
  )

  fy_pattern <- "(FY)?\\d{2}(R|T)?|,|/"

  modality_map <- api_call(paste0("dataElementGroupSets/", groupSet),
                           d2_session = d2_session) %>%
    api_fields("dataElementGroups[name,dataElements[id]]") %>% #nolint
    api_get(d2_session = d2_session) %>%
    tidyr::unnest(cols = dataElements) %>%
    dplyr::distinct() %>%
    dplyr::select(dataElement = id,
                  hts_modality = name) %>%
    dplyr::mutate(
      hts_modality =
        stringr::str_trim(
          stringr::str_remove_all(hts_modality, fy_pattern)))


  if (!is.null(dataElements)) {
    modality_map %<>%
      dplyr::filter(dataElement %in% dataElements)
  }

  modality_map
}
