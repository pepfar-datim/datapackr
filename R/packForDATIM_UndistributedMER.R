#' @export
#' @title packForDATIM_UndistributedMER(d)
#'
#' @description Packs undistributed MER data from Data Pack for use in analytics (NOT IMPORT).
#'
#' @param d Datapackr object
#' @param datim_map datim map object for the cop year
#' @return Modified d object with a DATIM compatible data frame for analytics id d$datim$UndistributedMER
#'
packForDATIM_UndistributedMER <- function(d, datim_map) {

  # Add dataElement & categoryOptionCombo ----
  d$datim$UndistributedMER <- d$data$MER %>%
    dplyr::mutate(
      support_type = dplyr::case_when(
        stringr::str_detect(indicator_code, "AGYW_PREV") ~ "No Support Type",
      TRUE ~ support_type
    )) %>%
    dplyr::left_join(
      .,
      (
        datim_map %>%
          dplyr::rename(Age = valid_ages.name,
                        Sex = valid_sexes.name,
                        KeyPop = valid_kps.name)
      ),
      by = c("indicator_code", "Age", "Sex", "KeyPop", "support_type")
    ) %>%
    map_datim_join(datim_map) %>%

  # Add PSNU uid ----
  dplyr::mutate(
    psnuid = addPsnuid(PSNU)
  ) %>%

  # Select and rename based on DATIM protocol ----
  set_datim_protocol()

  return(UndistributedMER)

}
