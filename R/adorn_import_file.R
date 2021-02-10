#' @export
#' @title Convert PSNU-level DATIM import file into analytics-friendly object
#'
#' @description Convert PSNU-level DATIM import file into analytics-friendly
#' object, similar to the MER Structured Datasets
#'
#' @param psnu_import_file DHIS2 import file to convert
#' @param cop_year
#' @param d2_session
#' 
#' @return data
#'
adorn_import_file <- function(psnu_import_file,
                              cop_year = getCurrentCOPYear(),
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {
  #TODO: Generalize this outside the context of COP
  data <- psnu_import_file %>%
    
  # Adorn PSNUs
    dplyr::rename(psnu_uid = orgUnit) %>%
    dplyr::left_join(
      (valid_PSNUs %>%
        add_dp_psnu() %>%
        dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id,
                      psnu, psnu_uid, dp_psnu, psnu_type, DREAMS)),
      by = c("psnu_uid" = "psnu_uid"))
  
  # Adorn Mechanisms ####
  country_uids <- unique(data$country_uid)
  
  mechs <-
    getMechanismView(
      country_uids = country_uids,
      cop_year = NULL,
      include_dedupe = TRUE,
      include_MOH = TRUE,
      d2_session = d2_session) %>%
    dplyr::select(-ou, -startdate, -enddate)
  
  # Allow mapping of either numeric codes or alphanumeric uids
  data_codes <- data %>%
    dplyr::filter(stringr::str_detect(attributeOptionCombo, "\\d{4,}")) %>%
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "mechanism_code")) %>%
    dplyr::rename(mechanism_code = attributeOptionCombo,
                  attributeOptionCombo = attributeOptionCombo.y)
  
  data_ids <- data %>%
    dplyr::filter(
      stringr::str_detect(
        attributeOptionCombo,
        "[A-Za-z][A-Za-z0-9]{10}")) %>%
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "attributeOptionCombo"))
  
  data <- dplyr::bind_rows(data_codes, data_ids)
  
  # Adorn dataElements & categoryOptionCombos ####
  if (cop_year == 2021) {
    map_DataPack_DATIM_DEs_COCs_local <- datapackr::map_DataPack_DATIM_DEs_COCs
  } else if (cop_year == 2020) {
    map_DataPack_DATIM_DEs_COCs_local <- datapackr::cop20_map_DataPack_DATIM_DEs_COCs
    
    map_DataPack_DATIM_DEs_COCs_local$valid_sexes.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                         map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Male PWID"] <- "Male"
    map_DataPack_DATIM_DEs_COCs_local$valid_sexes.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                         map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Female PWID"] <- "Female"
    map_DataPack_DATIM_DEs_COCs_local$valid_kps.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                       map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Male PWID"] <- NA_character_
    map_DataPack_DATIM_DEs_COCs_local$valid_kps.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                       map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Female PWID"] <- NA_character_
  } else {
    stop("That COP Year currently isn't supported for processing by createAnalytics.")
  }
  
  data %<>%
    dplyr::mutate(
      FY = as.numeric(stringr::str_replace(period, "Oct", "")) + 1) %>%
    dplyr::left_join(
      (map_DataPack_DATIM_DEs_COCs_local %>%
          dplyr::rename(
            Age = valid_ages.name,
            Sex = valid_sexes.name,
            KeyPop = valid_kps.name)),
      by = c("dataElement" = "dataelementuid",
             "categoryOptionCombo" = "categoryoptioncombouid",
             "FY" = "FY")
    )
  
  return(data)
  
}
