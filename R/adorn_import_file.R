#' @export
#' @title Convert PSNU-level DATIM import file into analytics-friendly object
#'
#' @description Convert PSNU-level DATIM import file into analytics-friendly
#' object, similar to the MER Structured Datasets
#'
#' @param import_file DHIS2 import file to convert
#' @param cop_year COP Year
#' @param psnu_prioritizations List of orgUnit, value containing prioritization
#' values for each PSNU. If not included, blank prioritizations shown.
#' @param fill_prioritizations Logical. If TRUE and psnu_prioritizations provided,
#' psnu_prioritizations will take precedence where present. If TRUE and an org
#' unit in import_file does not have a matched prioritization in
#' psnu_prioritizations, will fill in based on DATIM. If TRUE and no 
#' psnu_prioritizations file provided, will fill in all based on DATIM.
#' @param d2_session
#' 
#' @return data
#'
adorn_import_file <- function(import_file,
                              cop_year = getCurrentCOPYear(),
                              psnu_prioritizations = NULL,
                              fill_prioritizations = TRUE,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {
  
  # TODO: Generalize this outside the context of COP
  
  data <- import_file
    
  # Adorn PSNUs ####
  countries <- 
    datimutils::getOrgUnitGroups("cNzfcPWEGSH", 
                                 fields = "organisationUnits[id,name]") %>%
    dplyr::rename(country_name = name)
  
  levels <- datapackr::getIMPATTLevels() %>%
    dplyr::filter(country_name %in% countries$country_name)
  
  org_units <- unique(data$orgUnit) %>%
    datimutils::getOrgUnits(
      fields = "id,name,level,organisationUnitGroups[id,name],ancestors[id,name]") %>%
    dplyr::mutate(
      country_uid =
        stringr::str_extract(as.character(ancestors),
                             paste0(countries$id, collapse = "|"))) %>%
    dplyr::left_join(levels, by = c("country_uid" = "country_uid")) %>%
    dplyr::mutate(
      psnu_uid = dplyr::case_when(
        level == prioritization ~ id,
        level > prioritization ~ 
          purrr::map2_chr(ancestors, prioritization,
                          function(x,y) magrittr::use_series(x, id) %>%
                            magrittr::extract(y)))
      ) %>%
    dplyr::left_join(
      (valid_PSNUs %>%
        add_dp_psnu() %>%
        dplyr::select(ou, ou_id, snu1, snu1_id,
                      psnu, psnu_uid, dp_psnu, psnu_type, DREAMS)),
      by = c("psnu_uid" = "psnu_uid"))
  
  # Add Prioritizations ####
  if (is.null(psnu_prioritizations)) {
    psnu_prioritizations <- tibble::tribble(~orgUnit, ~value, NA_character_, NA_real_) %>%
      tidyr::drop_na()
  }
  
  org_units %<>%
    dplyr::left_join(psnu_prioritizations %>%
                       dplyr::rename(pzn_value = value),
                     by = c("psnu_uid" = "orgUnit"))
  
  if (fill_prioritizations) {
    country_uids <- unique(org_units$country_uid)
      
    pzns <- datapackr::getPrioritizations(country_uids = country_uids, cop_year = cop_year)
    
    # If no prioritizations for cop_year, look back one year
    if (NROW(pzns) == 0) {
      pzns <- datapackr::getPrioritizations(country_uids = country_uids, cop_year = (cop_year-1))
    }
    
    # Fill in missing prioritizations
    org_units %<>%
      dplyr::left_join(
        pzns %>% dplyr::rename(pzn_value_datim = value),
        by = c("psnu_uid" = "orgUnit")
      ) %>%
      dplyr::mutate(
        pzn_value = dplyr::if_else(is.na(pzn_value), pzn_value_datim, pzn_value)) %>%
      dplyr::select(-pzn_value_datim)
  }
  
  # If prioritizations still missing, fill with "No Prioritization"
  org_units %<>%
    dplyr::mutate(
      pzn_value =
        dplyr::if_else(is.na(pzn_value), 0, pzn_value))
  
  # Translate Prioritizations
  prio_defined <- prioritization_dict() %>%
    dplyr::select(value, pzn_translation = name)
  
  org_units %<>%
    dplyr::left_join(prio_defined, by = c(pzn_value = "value")) %>%
    dplyr::select(
      orgUnit = id,
      ou,
      ou_id,
      country_name,
      country_uid,
      snu1,
      snu1_id,
      psnu,
      psnu_uid,
      prioritization = pzn_translation,
    )
  
  data %<>%
    dplyr::left_join(org_units, by = "orgUnit")
  
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
    dplyr:::rename(mechanism_code = attributeOptionCombo) %>%
    dplyr::left_join(mechs, by = c("mechanism_code" = "mechanism_code"))

  data_ids <- data %>%
    dplyr::filter(
      stringr::str_detect(
        attributeOptionCombo,
        "[A-Za-z][A-Za-z0-9]{10}")) %>%
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "attributeOptionCombo"))
  
  data <- dplyr::bind_rows(data_codes, data_ids)
  
  # Add upload_timestamp ####
  data %<>%
    dplyr::mutate(
      upload_timestamp = format(Sys.time(),"%Y-%m-%d %H:%M:%S", tz = "UTC"))
  
  # Adorn dataElements & categoryOptionCombos ####
   if (cop_year == 2021) {
    map_DataPack_DATIM_DEs_COCs_local <- datapackr::map_DataPack_DATIM_DEs_COCs
    
    data %<>%
      dplyr::left_join(
        (map_DataPack_DATIM_DEs_COCs_local %>%
           dplyr::rename(
             Age = valid_ages.name,
             Sex = valid_sexes.name,
             KeyPop = valid_kps.name)),
        by = c("dataElement" = "dataelementuid",
               "categoryOptionCombo" = "categoryoptioncombouid",
               "period" = "period")
      ) %>%
      dplyr::rename(fiscal_year = FY)
    
  # TODO: Is this munging still required with the map being a function of fiscal year?
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
     #TODO: Fix inconsistent naming of dataelement/dataelementuid
     map_DataPack_DATIM_DEs_COCs_local %<>% 
       dplyr::rename(dataelementuid = dataelement,
                     dataelementname = dataelement.y,
                     categoryoptioncomboname = categoryoptioncombo) %>% 
       dplyr::mutate(FY = 2021)
     
     data %<>%
       dplyr::mutate(
         fiscal_year = suppressWarnings(dplyr::if_else(stringr::str_detect(period, "Oct"),
                                                    as.numeric(stringr::str_replace(period, "Oct", "")) + 1,
                                                    as.numeric(stringr::str_replace(period, "Q3", ""))
                                                    ))) %>%
       dplyr::left_join(
         (map_DataPack_DATIM_DEs_COCs_local %>%
            dplyr::rename(
              Age = valid_ages.name,
              Sex = valid_sexes.name,
              KeyPop = valid_kps.name)),
         by = c("dataElement" = "dataelementuid",
                "categoryOptionCombo" = "categoryoptioncombouid",
                "fiscal_year" = "FY")
       )

   } else {
     #TODO: Do we need to throw an error here? 
     stop("That COP Year currently isn't supported for processing by createAnalytics.")
   }
  
  # Select/order columns ####
  data %<>%
    dplyr::select( ou,
                   ou_id,
                   country_name,
                   country_uid,
                   snu1,
                   snu1_id,
                   psnu,
                   psnu_uid,
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
  
  return(data)
  
}
