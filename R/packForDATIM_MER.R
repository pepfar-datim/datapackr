#' @export
#' @title packForDATIM_MER(d)
#'
#' @description Packs extracted PSNUxIM data from COP Data Pack for DATIM import.
#'
#' @param d Datapackr object
#' @param datim_map datim map object for the cop year
#' @return Modified d object with a DATIM compatible data frame for import id d$datim$MER
#'
packForDATIM_MER <- function(d, datim_map) {

  # Combine PSNUxIM distributed data with undistributed AGYW_PREV -----
  agyw_data <- d$data$MER %>%
    dplyr::filter(stringr::str_detect(indicator_code, "^AGYW_PREV")) %>%
    dplyr::mutate(
      support_type = "No Support Type",
      mech_code = datapackr::default_catOptCombo()
    ) %>%
    dplyr::select(names(d$data$SNUxIM))

  # Add dataElement & categoryOptionCombo and bind rows PSNuIM -----
  d$datim$MER <- d$data$SNUxIM %>%
    dplyr::bind_rows(agyw_data) %>%
    map_datim_join(datim_map) %>%

  # Round value ----
    dplyr::mutate(
    value =
      dplyr::case_when(
        value_type == "integer" ~ datapackr::round_trunc(value),
        TRUE ~ value),

  # Add PSNU uid ----
      psnuid = addPsnuid(PSNU)
    ) %>%

  # Select and rename based on DATIM protocol ----
  set_datim_protocol() %>%

  # Drop any rows with NA in any col to prevent breakage in iHub ----
  tidyr::drop_na()

  return(d)

}
