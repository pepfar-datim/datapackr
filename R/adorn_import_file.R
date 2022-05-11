#' @title imputePrioritizations
#'
#' @description Utility function to handle situations where DREAMS PSNUs
#' do not have an explicitly defined prioritization level. The prioritization
#' of the parent organisation unit will be used.
#' @param psnu_import_file DHIS2 import file to convert
#' @param prio Data frame consisting of orgUnit (as a uid) and prioritization
#' as a character.
#' @return prio
#'

  imputePrioritizations <- function(prio, psnu_import_file) {
    #Special handling for prioritization of PSNUs which are not at
    #the same level as the defined prioritization.
    #This can occur when DREAMS PSNUs are at a different
    #level than the PSNU level.
    dreams_orgunits <- valid_PSNUs %>%
      dplyr::filter(DREAMS == "Y") %>%
      dplyr::filter(psnu_uid %in% psnu_import_file$orgUnit) %>%
      dplyr::select(psnu_uid, ancestors) %>%
      dplyr::filter(!psnu_uid %in% prio$orgUnit)

    if (NROW(dreams_orgunits) > 0) {

      dreams_out <- data.frame(matrix(ncol = 2, nrow = NROW(dreams_orgunits)))
      names(dreams_out) <- names(prio)

      for (i in seq_len(NROW(dreams_orgunits))) {
        #Get all of the ancestors
        foo <- dreams_orgunits[i, "ancestors"][[1]]$id
        #Fetch the prioritization
        dreams_prio <- prio %>%
          dplyr::filter(orgUnit %in% foo) %>%
          dplyr::pull(prioritization) %>%
          unique(.)
        # This should never happen. The DREAMS orgunit
        # should have a unique parent prioritization.
        # If its not unique, take the first one, warn and move on.
        if (length(dreams_prio) > 1) {
          warning("Multiple parent prioritizations detected")
          dreams_prio <- dreams_prio[1]
        }
        # This should never happen. The DREAMS parent orgunit
        # prioritization level should exist. If not, issue
        # a warning and assign "No Prioritization"
        if (length(dreams_prio) == 0) {
          warning("No parent prioritizations detected")
          dreams_prio <- "No Prioritization"
        }

        dreams_out$orgUnit[i] <- dreams_orgunits$psnu_uid[i]
        dreams_out$prioritization <- dreams_prio

      }
      prio <- dplyr::bind_rows(prio, dreams_out)
    }
    prio
  }


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
#' @param include_default Should default mechanisms be included?
#'
#' @return data
#'
adorn_import_file <- function(psnu_import_file,
                              cop_year = getCurrentCOPYear(), #packageSetup.R
                              psnu_prioritizations = NULL,
                              filter_rename_output = TRUE,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE),
                              include_default = FALSE) {
  # Establishes the number of rows in the import file to use downstream.
  start_rows <- NROW(psnu_import_file)

  # TODO: Generalize this outside the context of COP
  data <- psnu_import_file %>%
    # Adorn PSNUs
    dplyr::left_join(
      (valid_PSNUs %>% # Comes from file data/valid_PSNUs.rda
         dplyr::filter(psnu_uid %in% psnu_import_file$orgUnit) %>%
         add_dp_psnu() %>% #Found in getPSNUs.R
         dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id,
                       psnu, psnu_uid, dp_psnu, psnu_type, DREAMS)), #cols to keep
      by = c("orgUnit" = "psnu_uid")) #Columns to join on

  # Utilizes start_rows to ensure the join worked as expected
  assertthat::are_equal(NROW(data), start_rows)

  # Add Prioritizations ####
  if (is.null(psnu_prioritizations)) {
    # If no psnu_prioritizations are found
    # Then fill prioritization column with NA
    data %<>%
      dplyr::mutate(
        prioritization = NA_character_
      )
  } else {
    # If psnu_prioritizations are found
    prio_defined <- prioritization_dict() %>% # Dict found in utilities.R
      dplyr::select(value, prioritization = name)

    prio <- psnu_prioritizations %>%
      dplyr::select(orgUnit, value) %>% # Columns to keep
      dplyr::left_join(prio_defined, by = "value") %>% # Columns to join on
      dplyr::select(-value) %>%  # Drop 'value' column
      imputePrioritizations(., psnu_import_file)

    data %<>%
      dplyr::left_join(prio, by = "orgUnit") %>% # Join data and prio
      dplyr::mutate(
        prioritization = # If col prioritization is 'na' replace with a value
          dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                           TRUE ~ prioritization))

    # Utilizes start_rows to ensure the join worked as expected
    assertthat::are_equal(NROW(data), start_rows)
  }

  # Adorn Mechanisms ####
  mechs <-
    # details can be found in adornMechanism.R
    getMechanismView(
      country_uids = unique(data$country_uid),
      cop_year = cop_year,
      include_dedupe = TRUE,
      include_MOH = TRUE,
      d2_session = d2_session,
      include_default = TRUE) %>%
    dplyr::select(-ou, -startdate, -enddate)

  # Allow mapping of either numeric codes or alphanumeric uids
  data_codes <- data %>%
    # Filter column attribute Option combo based on if it has 4 digits
    dplyr::filter(stringr::str_detect(attributeOptionCombo, "\\d{4,}")) %>%
    # Rename column
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

  #Handle data which has been assigned to the default mechanism
  #like AGWY_PREV

  data_default <- data %>%
    dplyr::filter(
      stringr::str_detect(
        attributeOptionCombo, "default|HllvX50cXC0")) %>%
    dplyr::mutate(attributeOptionCombo = "HllvX50cXC0") %>%
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "attributeOptionCombo"))

  # Stack data_codes and data_ids on top of one another.
  data <- dplyr::bind_rows(data_codes, data_ids, data_default) %>% dplyr::distinct()
  # Utilizes start_rows to ensure the join,filter,stack worked as expected
  assertthat::are_equal(NROW(data), start_rows)

  # Adorn dataElements & categoryOptionCombos ####

  map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(cop_year) # Found in utilities.R

  # TODO: Is this munging still required with the map being a function of fiscal year?
  if (cop_year == 2022) {
    map_des_cocs <- datapackr::cop22_map_adorn_import_file
  }

  data %<>%
    dplyr::mutate(
      # Create a time stamp column based on the the servers system time
      # Mon Mar 28 13:53:50 2022 --- Removed due to duplicates being created
      #upload_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      # Create a fiscal year column based on the period column values using regex
      fiscal_year = suppressWarnings(dplyr::if_else(stringr::str_detect(period, "Oct"),
                                                    as.numeric(stringr::str_replace(period, "Oct", "")) + 1,
                                                    as.numeric(stringr::str_replace(period, "Q3", ""))
      )
      )
    ) %>% #Join to map_des_cocs
    dplyr::left_join(
      (map_des_cocs %>%
         # Rename columns
         dplyr::rename(
           Age = valid_ages.name,
           Sex = valid_sexes.name,
           KeyPop = valid_kps.name)),
      # Columns to match on
      by = c("dataElement" = "dataelementuid",
             "categoryOptionCombo" = "categoryoptioncombouid",
             "fiscal_year" = "FY",
             "period" = "period"))
  # Utilizes start_rows to ensure the join worked as expected
  assertthat::are_equal(NROW(data), start_rows)
  # Select/order columns ####
  # Flag set in original function, approx line 20
  if (filter_rename_output) {# If flag is true, Keep the below columns from data
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
                    # Mon Mar 28 13:56:08 2022 Removed due to duplication
                    #upload_timestamp,
                    disagg_type,
                    resultstatus_inclusive,
                    top_level,
                    target_value = value,
                    indicator_code)
  }
  # Utilizes start_rows to ensure the join,filter,stack worked as expected
  assertthat::are_equal(NROW(data), start_rows)
  data

}
