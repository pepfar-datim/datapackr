#' @export
#' @title Convert PSNU-level DATIM import file into analytics-friendly object
#'
#' @description Convert PSNU-level DATIM import file into analytics-friendly
#' object, similar to the MER Structured Datasets
#'
#' @param psnu_import_file DHIS2 import file to convert
#' @param cop_year COP Year
#' @param psnu_prioritizations List of orgUnit, value containing prioritization
#' values for each PSNU. If not included, blank prioritizations shown.
#' @param filter_rename_output T/F Should this function output the final data in
#' the new, more complete format?
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' 
#' @return data
#'
adorn_import_file <- function(psnu_import_file,
                              cop_year = getCurrentCOPYear(),
                              psnu_prioritizations = NULL,
                              filter_rename_output = TRUE,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {
  
  # TODO: Generalize this outside the context of COP
  data <- psnu_import_file %>%
    
  # Adorn PSNUs
    dplyr::left_join(
      (valid_PSNUs %>%
        add_dp_psnu() %>%
        dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id,
                      psnu, psnu_uid, dp_psnu, psnu_type, DREAMS)),
      by = c("orgUnit" = "psnu_uid"))
  
  # Add Prioritizations ####
  if (is.null(psnu_prioritizations)) {
    data %<>%
      dplyr::mutate(
        prioritization = NA_character_
      )
  } else {
    prio_defined <- prioritization_dict() %>%
      dplyr::select(value, prioritization = name)
    
    prio <- psnu_prioritizations %>%
      dplyr::select(orgUnit, value) %>%
      dplyr::left_join(prio_defined, by = "value") %>%
      dplyr::select(-value)
    
    data %<>%
      dplyr::left_join(prio, by = "orgUnit") %>%
      dplyr::mutate(
        prioritization =
          dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                           TRUE ~ prioritization))
  }
  
  # Adorn Mechanisms ####
  country_uids <- unique(data$country_uid)
  
  mechs <-
    getMechanismView(
      country_uids = country_uids,
      cop_year = cop_year,
      include_dedupe = TRUE,
      include_MOH = TRUE,
      d2_session = d2_session) %>%
    dplyr::select(-ou, -startdate, -enddate)
  
  # Allow mapping of either numeric codes or alphanumeric uids
  data_codes <- data %>%
    dplyr::filter(stringr::str_detect(attributeOptionCombo, "\\d{4,}")) %>%
    dplyr::rename(mechanism_code = attributeOptionCombo) %>%
    dplyr::left_join(mechs, by = c("mechanism_code" = "mechanism_code"))

  data_ids <- data %>%
    dplyr::filter(
      stringr::str_detect(
        attributeOptionCombo,
        "[A-Za-z][A-Za-z0-9]{10}")) %>%
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "attributeOptionCombo"))
  
  data <- dplyr::bind_rows(data_codes, data_ids)
  
  map_DataPack_DATIM_DEs_COCs_local <- getMapDataPack_DATIM_DEs_COCs(cop_year)

  # Adorn dataElements & categoryOptionCombos ####
   
  # TODO: Is this munging still required with the map being a function of fiscal year?
   if (cop_year == 2020) {
     map_DataPack_DATIM_DEs_COCs_local$valid_sexes.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                          map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Male PWID"] <- "Male"
     map_DataPack_DATIM_DEs_COCs_local$valid_sexes.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                          map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Female PWID"] <- "Female"
     map_DataPack_DATIM_DEs_COCs_local$valid_kps.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                        map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Male PWID"] <- NA_character_
     map_DataPack_DATIM_DEs_COCs_local$valid_kps.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                        map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Female PWID"] <- NA_character_
     #TODO: Fix inconsistent naming of dataelement/dataelementuid
     map_DataPack_DATIM_DEs_COCs_local %<>% 
       dplyr::rename(dataelementuid = dataelement,
                     dataelementname = dataelement.y,
                     categoryoptioncomboname = categoryoptioncombo) %>% 
       dplyr::mutate(FY = 2021,
                       period = paste0(cop_year,"Oct"))
     } 
  
  data %<>%
    dplyr::mutate(
      upload_timestamp = format(Sys.time(),"%Y-%m-%d %H:%M:%S", tz = "UTC"),
      fiscal_year = suppressWarnings(dplyr::if_else(stringr::str_detect(period, "Oct"),
                                                    as.numeric(stringr::str_replace(period, "Oct", "")) + 1,
                                                    as.numeric(stringr::str_replace(period, "Q3", ""))
                                                    )
                                     )
      ) %>%
    dplyr::left_join(
      (map_DataPack_DATIM_DEs_COCs_local %>%
          dplyr::rename(
            Age = valid_ages.name,
            Sex = valid_sexes.name,
            KeyPop = valid_kps.name)),
      by = c("dataElement" = "dataelementuid",
             "categoryOptionCombo" = "categoryoptioncombouid",
             "fiscal_year" = "FY",
             "period" = "period")
    )
  
  # Select/order columns ####
  if (filter_rename_output) {
    data %<>%
      dplyr::select( ou,
                     ou_id,
                     country_name,
                     country_uid,
                     snu1,
                     snu1_id,
                     psnu,
                     psnu_uid = orgUnit,
                     prioritization,
                     mechanism_code,
                     mechanism_desc,
                     partner_id,
                     partner_desc,
                     funding_agency  = agency,
                     fiscal_year,
                     dataelement_id  = dataElement,
                     dataelement_name = dataelementname,
                     indicator = technical_area,
                     numerator_denominator,
                     support_type,
                     hts_modality,
                     categoryoptioncombo_id = categoryOptionCombo,
                     categoryoptioncombo_name = categoryoptioncomboname,
                     age = Age,
                     sex = Sex,
                     key_population = KeyPop,
                     resultstatus_specific = resultstatus,
                     upload_timestamp,
                     disagg_type,
                     resultstatus_inclusive,
                     top_level,
                     target_value = value,
                     indicator_code)
  }
  
  return(data)
  
}
