#' @export
#' @importFrom magrittr %>% %<>%
#' @title packForDATIM_MER(d)
#'
#' @description Packs extracted PSNUxIM data from COP Data Pack for DATIM import.
#'
#' @param d Datapackr object
#'
#' @return Modified d object with a DATIM compatible data frame for import id d$datim$MER
#'
packForDATIM_MER <- function(d) {
  
  # Combine PSNUxIM distributed data with undistributed AGYW_PREV
  agyw_data <- d$data$MER %>%
    dplyr::filter(stringr::str_detect(indicator_code,"^AGYW_PREV")) %>%
    dplyr::mutate(
      support_type = "No Support Type",
      mech_code = datapackr::default_catOptCombo()
    ) %>%
    dplyr::select(names(d$data$SNUxIM))
    
  d$datim$MER <- d$data$SNUxIM %>%
    dplyr::bind_rows(agyw_data) %>%
  # Add dataElement & categoryOptionCombo ####
    dplyr::left_join(., (datapackr::map_DataPack_DATIM_DEs_COCs %>%
                           dplyr::rename(Age = valid_ages.name,
                                         Sex = valid_sexes.name,
                                         KeyPop = valid_kps.name)),
                     by = c("indicator_code", "Age", "Sex", "KeyPop", "support_type")) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid) %>%
    
    # Add period ####
  dplyr::mutate(
    period = paste0(FY-1,"Oct")) %>%
    
    # Add PSNU uid ####
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)")
    ) %>%
    
    # Select and rename based on DATIM protocol ####
  dplyr::select(
    dataElement = dataelementuid,
    period,
    orgUnit = psnuid,
    categoryOptionCombo = categoryoptioncombouid,
    attributeOptionCombo = mech_code,
    value)
  
  return(d)
  
}
