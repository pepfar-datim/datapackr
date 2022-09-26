#'
#' @description Certain special PSNUs (like DREAMS) are part of the
#' target setting process, but may exist at a level in the
#' organisation unit hierarchy other than the COP Prioritization level.
#' For organisation units which exist at the prioritization level,
#' their prioritization should be left as is. For organisation units
#' which do not exist at the level at which prioritization is set,
#' the parent prioritization should be used.
#'
#'
#'
#'

  imputePrioritizations <- function(prio, psnu_import_file) {
    #TODO: Deprecate
    #Special handling for prioritization of PSNUs which are not at
    #the same level as the defined prioritization.
    #This can occur when DREAMS PSNUs are at a different
    #level than the PSNU level.
    dreams_orgunits <- valid_OrgUnits %>%
      dplyr::filter(DREAMS == "Y") %>%
      dplyr::filter(uid %in% psnu_import_file$orgUnit) %>%
      dplyr::select(psnu_uid = uid, ancestors) %>%
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

#' @description Utility function to create a map of all PSNUs
#' and their prioritization level regardless of whether they
#' are at the  COP Prioritization level or not.
#' @return A data frame consisting of organisation unit UIDs
#' and prioritization as text.
getPrioritizationMap <- function(snus, psnu_prioritizations) {

   snus  %>%
    dplyr::select(psnu_uid, id) %>%
    dplyr::left_join(psnu_prioritizations, by = c("psnu_uid" = "orgUnit")) %>%
    #Remove any invalid prioritization
    dplyr::mutate(value = ifelse(value %in% prioritization_dict()$value, value, NA_real_)) %>%
    dplyr::left_join(prioritization_dict() %>%
                       dplyr::select(value, prioritization = name),
                     by = c("value")) %>%
    dplyr::mutate(prioritization = ifelse(is.na(prioritization), "No Prioritization", prioritization)) %>%
    dplyr::select(id, prioritization)

}

#' @export
#' @title Convert a 'PSNU-level' DATIM import file into an analytics-friendly
#'  object.
#'
#' @description Convert a 'PSNU-level' DATIM import file into an
#'  analytics-friendly object, similar to the MER Structured Datasets
#'
#' @param psnu_import_file DHIS2 import file to convert
#' @inheritParams datapackr_params
#' @param psnu_prioritizations List of orgUnit, value containing prioritization
#' values for each PSNU. If not included, blank prioritizations shown.
#' @param filter_rename_output T/F Should this function output the final data in
#' the new, more complete format?
#' @param include_default Should default mechanisms be included?
#'
#' @return psnu_import_file
#'
adorn_import_file <- function(psnu_import_file,
                              cop_year = NULL,
                              psnu_prioritizations = NULL,
                              filter_rename_output = TRUE,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE),
                              include_default = FALSE) {

  cop_year %<>% check_cop_year()

  # Adorn orgunits ----
    # TODO: Maybe move some of this handling to getPSNUs and store right in valid_PSNUs?
  snus <- valid_OrgUnits %>% # Comes from file data/valid_PSNUs.rda
    dplyr::filter(uid %in% psnu_import_file$orgUnit) %>%
    add_dp_label() %>% #Found in getPSNUs.R
    dplyr::rename(id = uid)

  ancestor_ids <- purrr::map(snus$ancestors,
                             function(x)
                               x[["id"]])

  psnu_lvl <-
    purrr::map(
      snus$ancestors,
      function(x)
        purrr::map_lgl(
          x[["organisationUnitGroups"]],
          ~ "AVy8gJXym2D" %in% purrr::pluck(.x, "id")))

  snus$psnu_uid <-
    purrr::map2(
      ancestor_ids,
      psnu_lvl,
      function(x, y)
        ifelse(length(x[y]) == 0, c(NA_character_), x[y])) %>%
    purrr::reduce(.x = ., .f = c)

  snus %<>%
    dplyr::mutate(
      psnu_uid = dplyr::case_when(is.na(psnu_uid) ~ id,
                                   TRUE ~ psnu_uid)) %>%
    dplyr::select(ou, ou_uid, country_name, country_uid, snu1, snu1_uid, psnu_uid,
                  psnu = name, id, dp_label, org_type, DREAMS)

  psnu_import_file %<>%
    dplyr::left_join(snus, by = c("orgUnit" = "id"))

  # Add Prioritizations ####
  if (is.null(psnu_prioritizations)) {

    psnu_import_file %<>%
      addcols("prioritization")

    interactive_warning(
      paste0("Prioritizations not provided to adorn_import_file, so not included",
             " in analytics dataset."))

  } else {

    # If psnu_prioritizations are found
    prio_defined <- prioritization_dict() %>% # Dict found in utilities.R
      dplyr::select(value, prioritization = name)

    psnu_prioritizations %<>%
      dplyr::select(orgUnit, value) %>%
    # Remove anything not a PSNU & remove Mil
      dplyr::filter(orgUnit %in% snus$psnu_uid[snus$org_type != "Military"],
    # Remove invalid values
                    value %in% prioritization_dict()$value) %>%
    # Translate values into descriptions
      dplyr::left_join(prioritization_dict() %>%
                         dplyr::select(value, prioritization = name),
                       by = c("value")) %>%
      dplyr::select(-value)

    # Check for and impute prioritizations below PSNU level
    psnu_import_file %<>%
      dplyr::left_join(psnu_prioritizations, by = c("psnu_uid" = "orgUnit")) %>%
      dplyr::mutate(
        prioritization =
          dplyr::case_when(
            stringr::str_sub(psnu, 1, 9) == "_Military" ~ "No Prioritization",
            TRUE ~ prioritization))

    blank_pzns <- psnu_import_file$dp_label[is.na(psnu_import_file$prioritization)] %>%
      unique() %>%
      sort()

    if (length(blank_pzns) > 0) {
      interactive_warning(
        paste0("Prioritizations not provided to adorn_import_file for the",
               " following Org Units: \n\t* ",
               paste(blank_pzns, collapse = "\n\t* ")))
    }

    psnu_import_file %<>%
      dplyr::mutate(
        prioritization =
          dplyr::case_when(is.na(prioritization) ~ "Unknown",
                           TRUE ~ prioritization))

  }

  psnu_import_file %<>% dplyr::rename(psnu = name)

  # Adorn Mechanisms ####
  mechs <-
    # details can be found in adornMechanism.R
    getMechanismView(
      country_uids = unique(psnu_import_file$country_uid),
      cop_year = cop_year,
      include_dedupe = TRUE,
      include_MOH = TRUE,
      d2_session = d2_session,
      include_default = TRUE) %>%
    dplyr::select(-ou, -startdate, -enddate)

  # Allow mapping of either numeric codes or alphanumeric uids
  data_codes <- psnu_import_file %>%
    # Filter column attribute Option combo based on if it has 4 digits
    dplyr::filter(stringr::str_detect(attributeOptionCombo, "\\d{4,}")) %>%
    # Rename column
    dplyr::rename(mechanism_code = attributeOptionCombo) %>%
    # Join psnu_import_file with mechs
    dplyr::left_join(mechs, by = c("mechanism_code" = "mechanism_code"))

  data_ids <- psnu_import_file %>%
    dplyr::filter(
      stringr::str_detect(
        attributeOptionCombo,
        # Filter letter a-z ignore caps,followed by alphanumeric value, and must
        # be 10 characters in length
        "[A-Za-z][A-Za-z0-9]{10}")) %>%
    #Join psnu_import_file with mechs based on column attributeOptionCombo
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "attributeOptionCombo"))

  #Handle data which has been assigned to the default mechanism
  #like AGWY_PREV

  data_default <- psnu_import_file %>%
    dplyr::filter(
      stringr::str_detect(
        attributeOptionCombo, "default|HllvX50cXC0")) %>%
    dplyr::mutate(attributeOptionCombo = "HllvX50cXC0") %>%
    dplyr::left_join(mechs, by = c("attributeOptionCombo" = "attributeOptionCombo"))

  # Stack data_codes and data_ids on top of one another.
  psnu_import_file <- dplyr::bind_rows(data_codes, data_ids, data_default) %>% dplyr::distinct()
  # Utilizes row_num to ensure the join,filter,stack worked as expected
  #assertthat::are_equal(NROW(psnu_import_file), row_num)

  # Adorn dataElements & categoryOptionCombos ####

  map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(cop_year) # Found in utilities.R

  # TODO: Is this munging still required with the map being a function of fiscal year?
  if (cop_year == 2022) {
    map_des_cocs <- datapackr::cop22_map_adorn_import_file
  }

  psnu_import_file %<>%
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

  # Select/order columns ####
  # Flag set in original function, approx line 20
  if (filter_rename_output) {# If flag is true, Keep the below columns from data
    # and rename where necessary with =.
    psnu_import_file %<>%
      dplyr::select(ou,
                    ou_uid,
                    country_name,
                    country_uid,
                    snu1,
                    snu1_uid,
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

  psnu_import_file

}
