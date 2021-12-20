#' @export
#' @title Convert a 'PSNU-level' DATIM import file into an analytics-friendly
#'  object.
#'
#' @description Convert a 'PSNU-level' DATIM import file into an
#'  analytics-friendly object, similar to the MER Structured Datasets
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
                              cop_year = getCurrentCOPYear(),#packageSetup.R
                              psnu_prioritizations = NULL,
                              filter_rename_output = TRUE,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {

  # TODO: Generalize this outside the context of COP
  data <- psnu_import_file %>%

  # Adorn PSNUs
    dplyr::left_join(
      (valid_PSNUs %>% #Comes from file data/valid_PSNUs.rda
        add_dp_psnu() %>% #Found in getPSNUs.R
        dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id,
                      psnu, psnu_uid, dp_psnu, psnu_type, DREAMS)),#cols to keep
      by = c("orgUnit" = "psnu_uid")) #Columns to join on

  # Add Prioritizations ####
  if (is.null(psnu_prioritizations)) {#IF no psnu_prioritizations are found
    data %<>%
      dplyr::mutate(
        prioritization = NA_character_# Then fill prioritization column with NA
      )
  } else { #IF psnu_prioritizations are found
    prio_defined <- prioritization_dict() %>%# Dict found in utilities.R
      dplyr::select(value, prioritization = name)

    prio <- psnu_prioritizations %>%
      dplyr::select(orgUnit, value) %>% #Columns to keep
      dplyr::left_join(prio_defined, by = "value") %>% #Columns to join on
      dplyr::select(-value)# Drop 'value' column

    data %<>%
      dplyr::left_join(prio, by = "orgUnit") %>%# Join data and prio
      dplyr::mutate(
        prioritization =# If col prioritization is 'na' replace with a value
          dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                           TRUE ~ prioritization))
  }

  # Adorn Mechanisms ####
  country_uids <- unique(data$country_uid) #Creates a list of country uids

  mechs <-
    getMechanismView(# details can be found in adornMechanism.R
      country_uids = country_uids,
      cop_year = cop_year,
      include_dedupe = TRUE,
      include_MOH = TRUE,
      d2_session = d2_session) %>%
    dplyr::select(-ou, -startdate, -enddate)

  # Allow mapping of either numeric codes or alphanumeric uids
  data_codes <- data %>%
    # Filter column attribute Option combo based on if it has 4 digits
    dplyr::filter(stringr::str_detect(attributeOptionCombo, "\\d{4,}")) %>%
    #Rename column
    dplyr::rename(mechanism_code = attributeOptionCombo) %>%
    # Join data with mechs
    dplyr::left_join(mechs, by = c("mechanism_code" = "mechanism_code"))

  data_ids <- data %>%
    dplyr::filter(
      stringr::str_detect(
        attributeOptionCombo,
        # Filter letter a-z ignore caps,followed by alphanumeric value, and must
        # be 10 characters in length
        "[A-Za-z][A-Za-z0-9]{10}")) %>%
    #Join data with mechs based on column attributeOptionCombo
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "attributeOptionCombo"))
  
  # Stack data_codes and data_ids on top of one another. 
  data <- dplyr::bind_rows(data_codes, data_ids)

  map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(cop_year)# Found in utilities.R

  # Adorn dataElements & categoryOptionCombos ####

  # TODO: Is this munging still required with the map being a function of fiscal year?
   if (cop_year == 2020) {# If cop year equal 2020 modify entries in
                            # valid_sexes.name as follows
     map_des_cocs$valid_sexes.name[map_des_cocs$indicator_code == "KP_MAT.N.Sex.T" &
                                                          map_des_cocs$valid_kps.name == "Male PWID"] <- "Male"
     map_des_cocs$valid_sexes.name[map_des_cocs$indicator_code == "KP_MAT.N.Sex.T" &
                                                          map_des_cocs$valid_kps.name == "Female PWID"] <- "Female"
     map_des_cocs$valid_kps.name[map_des_cocs$indicator_code == "KP_MAT.N.Sex.T" &
                                                        map_des_cocs$valid_kps.name == "Male PWID"] <- NA_character_
     map_des_cocs$valid_kps.name[map_des_cocs$indicator_code == "KP_MAT.N.Sex.T" &
                                                        map_des_cocs$valid_kps.name == "Female PWID"] <- NA_character_
     #TODO: Fix inconsistent naming of dataelement/dataelementuid
     map_des_cocs %<>%
       #rename the columns in map_des_cocs
       dplyr::rename(dataelementuid = dataelement,
                     dataelementname = dataelement.y,
                     categoryoptioncomboname = categoryoptioncombo) %>%
       # Modify period based upon FY column
       dplyr::mutate(FY = 2021,period = paste0(cop_year, "Oct"))
     }

  data %<>%
    dplyr::mutate(
      #Create a time stamp column based on the the servers system time
      upload_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      #Create a fiscal year column based on the period column values using regex 
      fiscal_year = suppressWarnings(dplyr::if_else(stringr::str_detect(period, "Oct"),
                                                    as.numeric(stringr::str_replace(period, "Oct", "")) + 1,
                                                    as.numeric(stringr::str_replace(period, "Q3", ""))
                                                    )
                                     )
      ) %>%#Join to map_des_cocs
    dplyr::left_join(
      (map_des_cocs %>%
         #Rename columns
          dplyr::rename(
            Age = valid_ages.name,
            Sex = valid_sexes.name,
            KeyPop = valid_kps.name)),
      #Columns to match on
      by = c("dataElement" = "dataelementuid",
             "categoryOptionCombo" = "categoryoptioncombouid",
             "fiscal_year" = "FY",
             "period" = "period")
    )

  # Select/order columns ####
  #Flag set in original function, approx line 20
  if (filter_rename_output) {# IF flag is true, Keep the below columns from data
                              # and rename where necessary with =. 
    data %<>%
      dplyr::select(ou,
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
