#' @export
#' @title Pack Undistributed MER Data for DATIM (NOT IMPORT)
#'
#' @description Packs undistributed MER data from Data Pack for use in analytics (NOT IMPORT).
#'
#' @param data Dataframe to pack for DATIM. Any mechanism attribution will be ignored.
#' @inheritParams datapackr_params
#'
#' @return MER data formatted as standard DATIM import file, with all data
#' allocated against the default mechanism UID.
#'
packForDATIM_UndistributedMER <- function(data,
                                          cop_year) {

  # Check params
  params <- check_params(cop_year = cop_year)

  ps <- c("cop_year")

  for (p in ps) {
    assign(p, purrr::pluck(params, p))
  }

  rm(params, ps, p)

  # Check dataset ####
  if (!all(names(data) == c("PSNU", "psnuid", "sheet_name", "indicator_code",
                            "Age", "Sex", "KeyPop", "value"))) {
    stop(paste0("Provided dataset should have the following columns: PSNU, ",
          "psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value"))
  }

  # Add dataElement & categoryOptionCombo ####
  datim_map <- getMapDataPack_DATIM_DEs_COCs(cop_year = cop_year)
  UndistributedMER <- data %>%
    dplyr::mutate(
      support_type = "DSD",
      mech_code = default_catOptCombo()
    ) %>%
    dplyr::left_join(., (datim_map %>%
                           dplyr::rename(Age = valid_ages.name,
                                         Sex = valid_sexes.name,
                                         KeyPop = valid_kps.name)),
                     by = c("indicator_code", "Age", "Sex", "KeyPop", "support_type")) %>%
    tidyr::drop_na(dataelementuid, categoryoptioncombouid) %>%

    # Add period ####
    dplyr::mutate(
      period = paste0(FY - 1, "Oct")) %>%

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
      value) %>%
  # Aggregate across 50+ age bands ####
    dplyr::group_by(dplyr::across(c(-value))) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  return(UndistributedMER)

}
