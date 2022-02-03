#' @export
#' @importFrom magrittr %>% %<>%
#' @title packForDATIM_OPU(d, datim_map)
#'
#' @description Packs extracted PSNUxIM data from OPU Data Pack for DATIM import.
#'
#' @param d Datapackr object
#' @param datim_map datim map object for the cop year
#' @return Modified d object with a DATIM compatible data frame for import id d$datim$OPU
#'
packForDATIM_OPU <- function(d, datim_map) {

  # select data to work with -----
  if (d$info$cop_year == 2020) {
    data <- d$data$extract
    datim_map %<>%
      dplyr::rename(dataelementuid = dataelement) %>%
      dplyr::mutate(
        period = paste0(d$info$cop_year, "Oct"))
  } else {
    data <- d$data$SNUxIM
  }

  # Add dataElement & categoryOptionCombo ----
  d$datim$OPU <- data %>%
   map_datim_join(datim_map) %>%
    # Select and rename based on DATIM protocol ----
    set_datim_protocol()

  return(d)

}
