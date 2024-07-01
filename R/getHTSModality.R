#' @export
#' @title Get HTS Modalities map
#'
#' @description
#' Map HTS modality to dataElement id
#'
#' @inheritParams datapackr_params
#' @return Dataframe of HTS modalities mapped to dataElements
#'
getHTSModality <- function(cop_year = getCurrentCOPYear(),
                           d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {
  groupSet <- switch(
    as.character(cop_year),
    "2024" = "Bm4JmNS8ciD",
    "2023" = "fmxSIyzexmb",
  )

  stopifnot("Requested COP year is not supported." = !is.null(groupSet))

  fy_pattern <- "(FY)?\\d{2}(R|T)?|,|/"

  modality_map <-
    datimutils::getDataElementGroupSets(
      groupSet,
      fields = "dataElementGroups[name,dataElements[id]]",
      d2_session = d2_session) %>%
    dplyr::select(dataElementGroups) %>%
    tidyr::unnest(cols = dataElementGroups) %>%
    tidyr::unnest(cols = dataElements) %>%
    dplyr::distinct() %>%
    dplyr::rename(dataElement = id,
                  hts_modality = name) %>%
    dplyr::mutate(
      hts_modality =
        stringr::str_trim(
          stringr::str_remove_all(hts_modality, fy_pattern)))

  modality_map
}
