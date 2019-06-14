#' @export
#' @importFrom magrittr %>% %<>%
#' @title packSUBNAT_IMPATT(d)
#'
#' @description Takes the outputs of the \code{\link{unPackSheets}} function and
#'     recompiles the dataframe containing SUBNAT and IMPATT data,
#'     \code{d$data$SUBNAT_IMPATT} into a standard DATIM import file.
#'
#' @param data SUBNAT/IMPATT dataframe to pack for DATIM.
#' 
#' @return Dataframe of SUBNAT & IMPATT data ready for DATIM ingestion.
#' 
packSUBNAT_IMPATT <- function(data) {
  
  SUBNAT_IMPATT <- data %>%
    dplyr::left_join((
      datapackr::indicatorMap %>%
        dplyr::filter(dataset %in% c("SUBNAT", "IMPATT")) %>%
        dplyr::rename(indicator_code = indicatorCode) %>%
        dplyr::select(
          sheet_name,
          indicator_code,
          Age = validAges,
          Sex = validSexes,
          KeyPop = validKPs,
          dataelementuid,
          categoryoptioncombouid
        )
    )) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid, value) %>%
    dplyr::mutate(
      period = datapackr::periodInfo$iso,
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
    dplyr::filter(value > 0) %>%
    dplyr::select(
      dataElement = dataelementuid,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo,
      value
    ) %>%
    dplyr::group_by(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = as.character(round_trunc(value)))
  
  return(SUBNAT_IMPATT)
}
