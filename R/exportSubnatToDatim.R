#' @export
#' @importFrom magrittr %>% %<>%
#' @title exportSubnatToDATIM(d)
#'
#' @description Takes the outputs of the \code{\link{unPackSheets}} function and
#'     adds  a dataframe containing SUBNAT and IMPATT data,
#'     \code{d$data$SUBNAT_IMPATT} into a standard DATIM import file.
#'
#' @param data SUBNAT/IMPATT dataframe to pack for DATIM.
#' 
#' @return Datapackr d object
#' 
exportSubnatToDATIM <- function(d) {
  
  
  d$datim$subnat_impatt <- d$data$SUBNAT_IMPATT %>%
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
  
  
  #@Scott: TODO. DO NOT throw an error here. This needs to be handled with a message. 
  
  # TEST: Whether any NAs in any columns
  if (any(is.na(d$datim$subnat_impatt))) {
     warning("ERROR occurred. NAs remained when preparing SUBNAT/IMPATT data for DATIM import.")
  }
  
  # TEST: Any Negative values? (not allowed for SUBNAT/IMPATT dataset)
  if (any(d$datim$subnat_impatt < 0)) {
    warning("ERROR occurred. Negative values present in SUBNAT/IMPATT data.")
  }
  
  return(d)
}
