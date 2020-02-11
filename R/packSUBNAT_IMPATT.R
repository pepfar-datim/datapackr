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
  #TODO: Why is this hardcoded here and not present in the schema?
  SUBNAT_IMPATT <- data %>%
    dplyr::left_join(., ( datapackr::map_DataPack_DATIM_DEs_COCs %>% 
                        dplyr::rename(Age = valid_ages.name,
                                      Sex = valid_sexes.name,
                                      KeyPop = valid_kps.name) )) %>% 
    dplyr::mutate(
      period = paste0( d$info$cop_year ,"Oct" ),
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
    dplyr::select(
      dataElement = dataelement,
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
                    attributeOptionCombo ) %>%
    dplyr::summarise(value = sum(value,na.rm=TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = as.character(datapackr::round_trunc(value))) %>% 
    #TODO: Not sure where the NAs are coming from here...
    tidyr::drop_na()
  
  
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
