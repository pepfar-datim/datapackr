#' @export
#' @title Convert PSNU-level DATIM import file into analytics-friendly object
#'
#' @description Convert PSNU-level DATIM import file into analytics-friendly
#' object, similar to the MER Structured Datasets
#'
#' @param psnu_import_file PSNU-level DATIM import file to convert
#' 
#' @return data
#'
adorn_import_file <- function(psnu_import_file) {
  #TODO: Generalize this outside the context of COP
  data <- psnu_import_file %>%
  
  # Adorn PSNUs ####
    dplyr::rename(psnu_uid = orgUnit) %>%
    dplyr::left_join(
      dplyr::select(
        valid_PSNUs %>% add_dp_psnu(),
        country_name, country_uid, psnu, psnu_uid, dp_psnu),
      by = c("psnu_uid" = "psnu_uid"))
  
  # Adorn Mechanisms ####
  mechs <- getMechList(include_dedupe = TRUE, include_MOH = TRUE) %>% 
    dplyr::select(-ou, -startdate, -enddate)
  
  data_codes <- data %>%
    dplyr::filter(stringr::str_detect(attributeOptionCombo, "\\d{4,6}")) %>%
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
  data %<>% 
    dplyr::left_join(
    (
      datapackr::map_DataPack_DATIM_DEs_COCs %>%
        dplyr::rename(
          Age = valid_ages.name,
          Sex = valid_sexes.name,
          KeyPop = valid_kps.name
        )
    ),
    by = c("dataElement" = "dataelement", "categoryOptionCombo" = "categoryoptioncombouid")
    )
  
  
  return(data)
  
}
