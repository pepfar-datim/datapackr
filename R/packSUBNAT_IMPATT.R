#' @importFrom magrittr %>% %<>%
#' @title packSUBNAT_IMPATT(d)
#'
#' @description Takes the outputs of the \code{\link{unPackSheets}} function and
#'     recompiles the dataframe containing SUBNAT and IMPATT data,
#'     \code{d$data$SUBNAT_IMPATT} into a standard DATIM import file.
#'
#' @param d Datapackr list object
#' 
#' @return Dataframe of SUBNAT & IMPATT data ready for DATIM ingestion.
#' 
packSUBNAT_IMPATT <- function(d) {
  
  d$datim$SUBNAT_IMPATT <- d$data$SUBNAT_IMPATT %>%
    dplyr::left_join((
      datapackr::indicatorMap %>%
        dplyr::filter(dataset %in% c("SUBNAT", "IMPATT")) %>%
        dplyr::select(
          sheet_name,
          indicatorCode,
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
  
  return(d)
}
