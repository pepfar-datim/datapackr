#' @export
#' @importFrom magrittr %>% %<>%
#' @title packForDATIM_UndistributedMER(d)
#'
#' @description Packs undistributed MER data from Data Pack for use in analytics (NOT IMPORT).
#'
#' @param d Datapackr object
#'
#' @return Modified d object with a DATIM compatible data frame for analytics id d$datim$UndistributedMER
#'
packForDATIM_UndistributedMER <- function(d) {
  
  # Add dataElement & categoryOptionCombo ####
  d$datim$UndistributedMER <- d$data$MER %>%
    dplyr::mutate(
      support_type = "DSD",
      mech_code = "999999"
    ) %>%
    dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, psnuid,
                  mech_code, support_type, value) %>%
    dplyr::left_join(., (datapackr::map_DataPack_DATIM_DEs_COCs %>%
                           dplyr::rename(Age = valid_ages.name,
                                         Sex = valid_sexes.name,
                                         KeyPop = valid_kps.name)),
                     by = c("indicator_code", "Age", "Sex", "KeyPop", "support_type")) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid) %>%
    
    # Add period ####
  dplyr::mutate(
    period = paste0(FY,"Oct")) %>%
    
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
