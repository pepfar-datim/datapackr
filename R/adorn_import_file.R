#' @export
#' @title getPSNUInfo
#' @description Certain special PSNUs (like DREAMS) are part of the
#' target setting process, but may exist at a level in the
#' organisation unit hierarchy other than the COP Prioritization level.
#' For organisation units which exist at the prioritization level,
#' their prioritization should be left as is. For organisation units
#' which do not exist at the level at which prioritization is set,
#' the parent prioritization should be used.
#'
#' @param snu_uids List of UIDs corresponding to DATIM organisation units.
#' @inheritParams datapackr_params
#'
#' @return Tibble of orgunits mapped to orgunit name, PSNU name, & PSNU uid.
getPSNUInfo <- function(snu_uids,
                        d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {

  orgunits <- valid_OrgUnits %>%
    dplyr::filter(uid %in% unique(snu_uids)) %>%
    dplyr::select(name, uid, ancestors, organisationUnitGroups)

  uids_not_cached <- snu_uids[!snu_uids %in% valid_OrgUnits$uid]

  if (length(uids_not_cached) > 0) {
    orgunits <-
      datimutils::getMetadata(
        end_point = "organisationUnits",
        paste0("id:in:[", paste(uids_not_cached, collapse = ","), "]"),
        fields =
          "id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]",
        d2_session = d2_session) %>%
      tibble::as_tibble(.) %>%
      dplyr::rename(uid = id) %>%
      dplyr::bind_rows(orgunits, .)
  }

  # Find position of the PSNU. Note that this will take the first match only.
  # There should never be multiple ancestors, but we are not really protected
  # anywhere against it anyway if it happens.
  psnu_lvl <-
    lapply(orgunits$ancestors,
           function(x) {
             lapply(x[["organisationUnitGroups"]],
                    function(x) any(x$id %in% "AVy8gJXym2D"))})
  psnu_lvl_index <- unlist(Map(function(x) Position(isTRUE, x), psnu_lvl))

  # ID the PSNU
  ancestor_ids <- lapply(orgunits$ancestors, function(x) x[["id"]])
  orgunits$psnu_uid <- mapply(function(x, y) x[y], ancestor_ids, psnu_lvl_index)
  ancestor_names <- lapply(orgunits$ancestors, function(x) x[["name"]])
  orgunits$psnu <- mapply(function(x, y) x[y], ancestor_names, psnu_lvl_index)

  # If orgunit is PSNU or Military, then use itself.
  orgunits %<>%
    dplyr::mutate(
      psnu_uid = dplyr::case_when(
        stringr::str_detect(as.character(organisationUnitGroups), "AVy8gJXym2D|nwQbMeALRjL") ~ uid,
        TRUE ~ psnu_uid),
      psnu = dplyr::case_when(
        stringr::str_detect(as.character(organisationUnitGroups), "AVy8gJXym2D|nwQbMeALRjL") ~ name,
        TRUE ~ psnu)) %>%
    dplyr::select(name, uid, psnu, psnu_uid)

  return(orgunits)
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
#' values for each PSNU.
#' @param filter_rename_output Should this function output the final data in
#' the new, more complete format?
#' @param map_des_cocs Can be used to supply a specific data element/COC map.
#' If not specified, results will be obtained from default methods.
#' @param include_default Should default mechanisms be included?
#'
#' @return psnu_import_file
#'
adorn_import_file <- function(psnu_import_file,
                              cop_year = NULL,
                              psnu_prioritizations = NULL,
                              map_des_cocs = NULL,
                              filter_rename_output = TRUE,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE),
                              include_default = FALSE) {

  row_num <- NROW(psnu_import_file)

  cop_year %<>% check_cop_year()

  # Adorn orgunits ----
  psnu_import_file %<>%
    dplyr::left_join(dplyr::select(valid_OrgUnits, -lastUpdated),
                     by = c("orgUnit" = "uid"))

  # Utilizes row_num to ensure the join worked as expected
  assertthat::are_equal(NROW(psnu_import_file), row_num)
  # TODO: Convert to test

  # Add Prioritizations ####
  if (is.null(psnu_prioritizations)) {
    psnu_import_file %<>%
      addcols("prioritization")
    #TODO: Rename this everywhere to something specifying it means psnu
    # prioritization, instead of facility/community
  } else {
    # Check prioritizations
    psnu_prioritizations %<>%
      dplyr::left_join(prioritization_dict() %>%
                         dplyr::select(value, prioritization = name),
                       by = c("value")) %>%
      dplyr::filter(!is.na(prioritization)) %>%
      dplyr::select(-value) %>%
      dplyr::semi_join(valid_OrgUnits %>%
                         dplyr::filter(org_type %in% c("PSNU", "Country")),
                       by = c("orgUnit" = "uid"))

    unknown_psnu <- psnu_import_file %>%
      dplyr::filter(org_type == "DSNU" | is.na(org_type)) %>%
      dplyr::pull(orgUnit) %>%
      unique()

    if (length(unknown_psnu) > 0) {
      psnus <- getPSNUInfo(unknown_psnu, d2_session = d2_session) %>%
        dplyr::select(-name)

      psnu_import_file %<>%
        dplyr::left_join(psnus, by = c("orgUnit" = "uid"))
    } else {
      psnu_import_file %<>%
        addcols(c("psnu", "psnu_uid"))
    }

    psnu_import_file %<>%
      dplyr::mutate(
        psnu = dplyr::case_when(
          is.na(psnu_uid) & !is.na(name) ~ name,
          TRUE ~ psnu),
        psnu_uid = dplyr::case_when(
          is.na(psnu_uid) & !is.na(name) ~ orgUnit,
          TRUE ~ psnu_uid)) %>%
      dplyr::left_join(psnu_prioritizations,
                       by = c("psnu_uid" = "orgUnit")) %>%
      dplyr::mutate(
        prioritization =
          ifelse(is.na(prioritization),
                 "No Prioritization",
                 prioritization)) %>%
      dplyr::select(-psnu, -psnu_uid)
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
  assertthat::are_equal(NROW(psnu_import_file), row_num)

  # Adorn dataElements & categoryOptionCombos ####
  # Use the default DE/COC map if none has been supplied.
  if (is.null(map_des_cocs)) {
    map_des_cocs <- getMapDataPack_DATIM_DEs_COCs(cop_year) # Found in utilities.R
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

  assertthat::are_equal(NROW(psnu_import_file), row_num)
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
