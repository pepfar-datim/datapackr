#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom stats complete.cases
#' @title exportDistributedDataToDATIM(data)
#'
#' @description Packs distributed MER data prepared from unPackSNUxIM for import to DATIM.
#'
#' @param d
#' 
#' @return Modified d object with  a DATIM compatible data frame for import id d$datim$MER
#' 
exportDistributedDataToDATIM <- function(d, keep_dedup = FALSE) {
  
  if(keep_dedup == TRUE){
    d$datim$MER <- d$data$distributedMER  
  } else {
  #Filter the pseudo-dedupe mechanism data out
  d$datim$MER <- d$data$distributedMER %>%
    dplyr::filter(mechanism_code != '99999') 
  }
  
  # Readjust for PMTCT_EID
  d$datim$MER %<>% dplyr::mutate(
      Age =
        dplyr::case_when(
          indicator_code %in% c("PMTCT_EID.N.Age.T.2mo","PMTCT_EID.N.Age.T.2to12mo")
            ~ NA_character_,
          TRUE ~ Age)
    ) %>%
    
  # Pull in all dataElements and categoryOptionCombos
    dplyr::left_join(., ( datapackr::map_DataPack_DATIM_DEs_COCs %>% 
                            dplyr::rename(Age = valid_ages.name,
                                          Sex = valid_sexes.name,
                                          KeyPop = valid_kps.name) )) %>% 
    
  # Add period
    dplyr::mutate(
      period = paste0(d$info$cop_year,"Oct") ) %>% 
    # Under COP19 requirements, after this join, TX_PVLS N will remain NA for dataelementuid and categoryoptioncombouid
    # Select and rename based on DATIM protocol
    dplyr::select(
      dataElement = dataelement,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo = mechanism_code,
      value) %>%
    
  # Make sure no duplicates
    dplyr::group_by(dataElement, period, orgUnit,categoryOptionCombo,
                    attributeOptionCombo) %>% #TODO: Coordinate with self-service on this name change
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    
  # Remove anything which is NA here. Under COP19 guidance, this will include only TX_PVLS.N.Age/Sex/Indication/HIVStatus.20T.Routine
    dplyr::filter(complete.cases(.))
  
  return(d)
  
}