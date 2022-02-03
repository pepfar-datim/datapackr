#' @export
#' @importFrom magrittr %>% %<>%
#' @title packForDATIM_UndistributedMER(d, datim_map)
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
      support_type = "DSD",
      mech_code = default_catOptCombo()
    ) %>%
    map_datim_join(datim_map) %>%

  # Add PSNU uid ----
  dplyr::mutate(
    psnuid = addPsnuid(PSNU)
  ) %>%

  # Select and rename based on DATIM protocol ----
  set_datim_protocol()

  return(d)

}
