#' @export
#' @importFrom magrittr %>% %<>%
#' @title Create a test Targets dataset
#'
#' @description
#' Compiles a test DATIM import file as would be output by a Data Pack.
#'
#' @param country_uids UIDs for Countries to tailor data for.
#' @param cop_year COP year for dating as well as selection of
#' templates.
#' @param d2_session datimutils d2Session object
#'
#' @return wb with specified sheet packed with data
#'
createTestDataset <- function(country_uids,
                              cop_year,
                              d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {
    if (!(cop_year %in% c(2020, 2021))) {
    stop("Not yet set up to produce a test dataset for that COP Year.")
    }

  schema <- datapackr::pick_schema(cop_year, tool = "Data Pack")
  DATIM_map <- datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year)

  # Get PSNUs to test against ####
  org_units <- datapackr::valid_PSNUs %>%
    dplyr::filter(
      country_uid %in% country_uids,
      !is.na(psnu_type)) %>%
    dplyr::select(orgUnit = psnu_uid) %>%
    dplyr::distinct()

  # Get dataElements and categoryOptionCombos to test against ####
  DATIM_map %<>%
    dplyr::filter(
      !is.na(indicator_code),
      !is.na(dataelementuid),
      !is.na(categoryoptioncombouid)) %>%
    dplyr::select(
      dataElement = dataelementuid, categoryOptionCombo = categoryoptioncombouid,
      FY, col_type, value_type, dataset, indicator_code, period) %>%
    dplyr::distinct()

  des_cocs.subnat_impatt <- DATIM_map %>%
    dplyr::filter(stringr::str_detect(dataset, "impatt|subnat"),
                  !stringr::str_detect(indicator_code, "PRIORITY_SNU")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type)

  des_cocs.prioritization <- DATIM_map %>%
    dplyr::filter(stringr::str_detect(indicator_code, "PRIORITY_SNU")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type)

  des_cocs.MER <- DATIM_map %>%
    dplyr::filter(!stringr::str_detect(dataset,"impatt|subnat"),
                  !indicator_code %in% c("AGYW_PREV.D.T", "AGYW_PREV.N.T")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type)

  des_cocs.DREAMS <- DATIM_map %>%
    dplyr::filter(indicator_code %in% c("AGYW_PREV.D.T", "AGYW_PREV.N.T")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type)

  # Get Mech list ####
  mechs <-
    getMechanismView(
      country_uids = country_uids,
      cop_year = cop_year,
      include_dedupe = FALSE,
      include_MOH = FALSE,
      d2_session = d2_session) %>%
    dplyr::filter(
      !stringr::str_detect(agency, "State|DOL|NIH|HRSA|Commerce|SAMHSA")
    ) %>%
    dplyr::select(attributeOptionCombo = mechanism_code)

  # Combine ####
  test_dataset.MER <- org_units %>%
    tidyr::crossing(des_cocs.MER,
                     mechs) %>%
    dplyr::mutate(value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value / 100,
        attributeOptionCombo %in% c("00000", "00001") ~ value * -1,
        TRUE ~ value
      ))

  test_dataset.subnat_impatt <- org_units %>%
    tidyr::crossing(des_cocs.subnat_impatt) %>%
    dplyr::mutate(
      attributeOptionCombo = datapackr::default_catOptCombo(),
      value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value / 100,
        TRUE ~ value
      ))

  test_dataset.prioritizations <- org_units %>%
    tidyr::crossing(des_cocs.prioritization) %>%
    dplyr::mutate(
      attributeOptionCombo = datapackr::default_catOptCombo(),
      value = sample(0:8, dplyr::n(), replace = TRUE))

  DREAMS_org_units <- datapackr::valid_PSNUs %>%
    dplyr::filter(
      country_uid %in% country_uids,
      !is.na(psnu_type),
      DREAMS == "Y") %>%
    dplyr::select(orgUnit = psnu_uid) %>%
    dplyr::distinct()

  test_dataset.DREAMS <- DREAMS_org_units %>%
    tidyr::crossing(des_cocs.DREAMS) %>%
    dplyr::mutate(
      attributeOptionCombo = datapackr::default_catOptCombo(),
      value = as.double(100))

  test_dataset <- dplyr::bind_rows(test_dataset.MER,
                                   test_dataset.DREAMS,
                                   test_dataset.subnat_impatt,
                                   test_dataset.prioritizations)

  return(test_dataset)
}
