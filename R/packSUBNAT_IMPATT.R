#' @export
#' @importFrom magrittr %>% %<>%
#' @title packSUBNAT_IMPATT(data)
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
  
  # Confirm data structure is as expected.
  SUBNAT_IMPATT.schema.names <-
    c("PSNU", "psnuid", "sheet_name", "indicator_code", "Age", "Sex",
      "KeyPop", "value")
  
  if (any(names(data) != SUBNAT_IMPATT.schema.names)) {
    error_msg <- "ERROR occurred while preparing SUBNAT/IMPATT data for DATIM. Columns not as expected."
    
    stop(error_msg)
  }
  
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
          categoryoptioncombouid)
      )) %>%
    #tidyr::drop_na(dataelementuid, categoryoptioncombouid, value) %>%
    dplyr::mutate(
      period = datapackr::periodInfo$iso,
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
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
  
  
  # TEST: Whether any NAs in any columns
  if (any(is.na(SUBNAT_IMPATT))) {
    stop("ERROR occurred. NAs remained when preparing SUBNAT/IMPATT data for DATIM import.")
  }
  
  # TEST: Any Negative values? (not allowed for SUBNAT/IMPATT dataset)
  if (any(SUBNAT_IMPATT$value < 0)) {
    stop("ERROR occurred. Negative values present in SUBNAT/IMPATT data.")
  }
  
  return(SUBNAT_IMPATT)
}
