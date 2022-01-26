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
  if (!d$info$cop_year %in% c(2021)) {
    stop("The COP year provided is not supported by packForDATIM_OPU")
  }

  map_des_cocs_local <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year)
  data <- d$data$SNUxIM

  # Add dataElement & categoryOptionCombo ####
  d$datim$OPU <- data %>%
    dplyr::left_join(., (map_des_cocs_local %>%
                            dplyr::rename(Age = valid_ages.name,
                                          Sex = valid_sexes.name,
                                          KeyPop = valid_kps.name)),
                     by = c("indicator_code", "Age", "Sex", "KeyPop", "support_type")) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid) %>%

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
