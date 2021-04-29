#' @export
#' @importFrom magrittr %>% %<>%
#' @title Create a multi-country, holistic test Targets dataset
#'
#' @description
#' Compiles a test DATIM import file that tests all major import DATIM assumptions.
#'
#' @param cop_year COP year for dating as well as selection of
#' templates.
#' @param d2_session datimutils d2Session object
#'
#' @return wb with specified sheet packed with data
#'
createTestDataset.holistic <- function(cop_year,
                                       d2_session = dynGet("d2_default_session",
                                                           inherits = TRUE)) {
  
  userOrgUnit <- 
    datimutils::getMetadata(end_point = "me",
                            fields = "organisationUnits[id]",
                            d2_session = d2_session)
  
  if (userOrgUnit != "ybg3MO3hcf4") {
    stop("This function only works for users with Global access in DATIM. To produce a dataset for an individual OU, use createTestDataset instead.")
  }
  
  if (cop_year == 2021) {
    schema <- datapackr::cop21_data_pack_schema
    DATIM_map <- datapackr::map_DataPack_DATIM_DEs_COCs
  } else if (cop_year == 2020) {
    schema <- datapackr::cop20_data_pack_schema
    DATIM_map <- datapackr::cop20_map_DataPack_DATIM_DEs_COCs
  } else {stop("Not yet set up to produce a test dataset for that COP Year.")}
  
  # Get orgUnits to test against ####
  PSNUs <- datapackr::valid_PSNUs %>%
    dplyr::filter(!is.na(psnu_type)) %>%
    dplyr::select(ou, ou_id, country_uid, orgUnit = psnu_uid, psnu_type) %>%
    dplyr::distinct()
  
  DSNUs <- datapackr::valid_PSNUs %>%
    dplyr::filter(!is.na(psnu_type),
                  DREAMS == "Y") %>%
    dplyr::select(ou, ou_id, country_uid, orgUnit = psnu_uid, psnu_type) %>%
    dplyr::distinct()
  
  # Get dataElements and categoryOptionCombos to test against ####
  DATIM_map %<>%
    dplyr::filter(
      !is.na(indicator_code),
      !is.na(dataelementuid),
      !is.na(categoryoptioncombouid)) %>%
    dplyr::select(
      dataElement = dataelementuid, categoryOptionCombo = categoryoptioncombouid,
      FY, col_type, value_type, dataset, indicator_code, age = valid_ages.name,
      sex = valid_sexes.name, kp = valid_kps.name, period) %>%
    dplyr::distinct()
  
  des_cocs.subnat_impatt <- DATIM_map %>%
    dplyr::filter(stringr::str_detect(dataset,"impatt|subnat"),
                  !stringr::str_detect(indicator_code,"PRIORITY_SNU")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type)
  
  des_cocs.prioritization <- DATIM_map %>%
    dplyr::filter(stringr::str_detect(indicator_code,"PRIORITY_SNU")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type)
  
  des_cocs.MER <- DATIM_map %>%
    dplyr::filter(!stringr::str_detect(dataset,"impatt|subnat"),
                  !indicator_code %in% c("AGYW_PREV.D.T","AGYW_PREV.N.T")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type,
                  indicator_code, age, sex, kp)
  
  des_cocs.DREAMS <- DATIM_map %>%
    dplyr::filter(indicator_code %in% c("AGYW_PREV.D.T","AGYW_PREV.N.T")) %>%
    dplyr::select(dataElement, categoryOptionCombo, period, value_type)
  
  # Get Mech list ####
  mechs <-
    getMechanismView(
      cop_year = cop_year,
      include_dedupe = FALSE,
      include_MOH = FALSE,
      d2_session = d2_session) %>%
    dplyr::filter(
      !stringr::str_detect(agency, "State|DOL|NIH|HRSA|Commerce|SAMHSA")
    ) %>%
    dplyr::select(ou, mechanism_desc, agency, attributeOptionCombo = mechanism_code)
  
  # Test that all MER DEs/COCs allow for import ####
    # Pick one mech per OU
  test_dataset.DEsCOCs.MER <- mechs %>%
    dplyr::group_by(ou) %>%
    dplyr::mutate(occurrence = 1:dplyr::n()) %>%
    dplyr::filter(occurrence == 1) %>%
    dplyr::select(-occurrence) %>%
    dplyr::ungroup() %>%
    # Combine with one PSNU per Country
    dplyr::right_join(PSNUs %>%
                        dplyr::filter(psnu_type != "Military") %>%
                        dplyr::group_by(country_uid) %>%
                        dplyr::mutate(occurrence = 1:dplyr::n()) %>%
                        dplyr::filter(occurrence == 1) %>%
                        dplyr::select(-occurrence) %>%
                        dplyr::ungroup(),
                      by = c("ou" = "ou")) %>%
    dplyr::select(-psnu_type, -ou, -ou_id, -country_uid) %>%
    # Combine with all DEs/COCs
    tidyr::crossing(des_cocs.MER) %>%
    dplyr::mutate(value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value/100,
        attributeOptionCombo %in% c("00000","00001") ~ value*-1,
        TRUE ~ value
      )) %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)
  
  # Test mech-dataSet assignments allow for import of MER ####
    # Pick one PSNU per OU
  test_dataset.mechs.MER <- PSNUs %>%
    dplyr::filter(psnu_type != "Military") %>%
    dplyr::group_by(country_uid) %>%
    dplyr::mutate(occurrence = 1:dplyr::n()) %>%
    dplyr::filter(occurrence == 1) %>%
    dplyr::select(-occurrence) %>%
    dplyr::ungroup() %>%
    # Combine with all Mechs
    dplyr::right_join(mechs,
                      by = c("ou" = "ou")) %>%
    # Hand pick DEs/COCs
    tidyr::crossing(des_cocs.MER %>%
                      dplyr::filter(indicator_code == "TX_CURR.T",
                                    age == "25-29",
                                    sex == "Female")) %>%
    dplyr::mutate(value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value/100,
        attributeOptionCombo %in% c("00000","00001") ~ value*-1,
        TRUE ~ value
      )) %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)
  
  # Test PSNUs ####
  test_dataset.PSNUs.MER <- mechs %>%
    dplyr::group_by(ou) %>%
    dplyr::mutate(occurrence = 1:dplyr::n()) %>%
    dplyr::filter(occurrence == 1) %>%
    dplyr::select(-occurrence) %>%
    dplyr::ungroup() %>%
    # Hand pick DEs/COCs
    tidyr::crossing(des_cocs.MER %>%
                      dplyr::filter(indicator_code == "TX_CURR.T",
                                    age == "20-24",
                                    sex == "Male")) %>%
    # Combine with all PSNUs
    dplyr::right_join(PSNUs %>%
                      dplyr::filter(psnu_type != "Military"),
                      by = c("ou" = "ou")) %>%
    dplyr::mutate(value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value/100,
        attributeOptionCombo %in% c("00000","00001") ~ value*-1,
        TRUE ~ value
      )) %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)
    
  # Test dataSet-DSNU assignments allow for import of DREAMS ####
    # Pick one PSNU per OU
  test_dataset.DSNUs.DREAMS <- DSNUs %>%
    # Combine with DEs/COCs
    tidyr::crossing(des_cocs.DREAMS) %>%
    dplyr::mutate(value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value/100,
        TRUE ~ value),
      attributeOptionCombo = datapackr::default_catOptCombo()
      ) %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)
  
  # Test SUBNAT/IMPATT ####
    # Get one PSNU per country
  test_dataset.SUBNAT_IMPATT <- PSNUs %>%
    dplyr::filter(psnu_type != "Military") %>%
    dplyr::group_by(country_uid) %>%
    dplyr::mutate(occurrence = 1:dplyr::n()) %>%
    dplyr::filter(occurrence == 1) %>%
    dplyr::select(-occurrence) %>%
    dplyr::ungroup() %>%
    # Combine with DEs/COCs
    tidyr::crossing(des_cocs.subnat_impatt) %>%
    dplyr::mutate(value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value/100,
        TRUE ~ value),
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)
  
  # Test Military ####
    # Get 1 sample mechanism per agency per ou
  test_dataset.Military <- mechs %>%
    dplyr::group_by(ou, agency) %>%
    dplyr::mutate(occurrence = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(occurrence == 1) %>%
    # Combine with all _Military PSNUs
    dplyr::right_join(PSNUs %>%
                        dplyr::filter(psnu_type == "Military"),
                      by = c("ou" = "ou")) %>%
    # Combine with all DEs/COCs
    tidyr::crossing(des_cocs.MER) %>%
    dplyr::mutate(value = as.double(100)) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        value_type == "percentage" ~ value/100,
        attributeOptionCombo %in% c("00000","00001") ~ value*-1,
        TRUE ~ value
      )) %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)
  
  # Test Prioritizations ####
  test_dataset.prioritizations <- PSNUs %>%
    tidyr::crossing(des_cocs.prioritization) %>%
    dplyr::mutate(
      attributeOptionCombo = datapackr::default_catOptCombo(),
      value = sample(0:8, dplyr::n(), replace = TRUE)
    ) %>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)
  
  #TODO: Functionalize repetitive code above
  #TODO: Also produce dataset to test Dedupes
    
  test_dataset <-
    list(prioritizations = test_dataset.prioritizations,
         subnat_impat = test_dataset.SUBNAT_IMPATT,
         all_DEsCOCs_mer = test_dataset.DEsCOCs.MER,
         all_mechs_mer = test_dataset.mechs.MER,
         all_PSNUs_mer = test_dataset.PSNUs.MER,
         military = test_dataset.Military,
         DREAMS = test_dataset.DSNUs.DREAMS)

  return(test_dataset)  
}
