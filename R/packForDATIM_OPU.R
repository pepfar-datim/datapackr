#' @export
#' @importFrom magrittr %>% %<>%
#' @title packForDATIM_OPU(d)
#'
#' @description Packs extracted PSNUxIM data from OPU Data Pack for DATIM import.
#'
#' @param d Datapackr object
#'
#' @return Modified d object with a DATIM compatible data frame for import id d$datim$OPU
#'
packForDATIM_OPU <- function(d) {
  if (d$info$cop_year == 2020){
    map_DataPack_DATIM_DEs_COCs_local <- 
      datapackr::cop20_map_DataPack_DATIM_DEs_COCs
  } else {
    stop("The COP year provided is not supported by packForDATIM_OPU")
  }
  
  # Add dataElement & categoryOptionCombo ####
  d$datim$OPU <- d$data$extract %>%
    dplyr::left_join(., (map_DataPack_DATIM_DEs_COCs_local %>%
                            dplyr::rename(Age = valid_ages.name,
                                          Sex = valid_sexes.name,
                                          KeyPop = valid_kps.name)),
                     by = c("indicator_code", "Age", "Sex", "KeyPop", "support_type")) %>%
    tidyr::drop_na(dataelement, categoryoptioncombouid) %>%
  
  # Add period ####
    dplyr::mutate(
      period = paste0(d$info$cop_year,"Oct")) %>%

  # Select and rename based on DATIM protocol ####
    dplyr::select(
      dataElement = dataelement,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo = mech_code,
      value)
  
  return(d)
  
}
