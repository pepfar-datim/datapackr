# This script produces a test dataset for use in downstream testing
packCOP22TestDataset <- function(country_uids,
                                cop_year,
                                snu_n = 6,
                                im_allocated = TRUE,
                                im_fully_allocated = TRUE,
                                include_dedupe = TRUE,
                                invalid_values = FALSE,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  
  # dataElements, categoryOptionCombos, periods ####
  des_mil <- datimutils::getSqlView(sql_view_uid = "DotdxKrNZxG",
                                    variable_keys = "dataSets",
                                    variable_values = "cihuwjoY5xP",
                                    d2_session = d2_session) %>%
    dplyr::pull(dataelementuid) %>%
    unique()
  
  de_coc_pd <- datapackr::cop22_map_DataPack_DATIM_DEs_COCs %>%
    dplyr::filter(!is.na(indicator_code),
                  !(targets_results == "results" & dataset == "subnat")) %>%
    dplyr::select(indicator_code, dataelementuid, categoryoptioncombouid, dataset, period)
  
  de_coc_pd.mer_nondreams <- de_coc_pd %>%
    dplyr::filter(dataset == "mer", indicator_code != "OVC_SERV.DREAMS.T")
  
  de_coc_pd.mil <- de_coc_pd %>%
    dplyr::filter(dataelementuid %in% des_mil,
                  indicator_code != "OVC_SERV.DREAMS.T")
  
  de_coc_pd.dreams <- de_coc_pd %>%
    dplyr::filter(dataset == "dreams")
  
  de_coc_pd.dreams_ovc <- de_coc_pd %>%
    dplyr::filter(indicator_code == "OVC_SERV.DREAMS.T")
  
  de_coc_pd.dreams_ovc_mil <- de_coc_pd.dreams_ovc %>%
    dplyr::filter(dataelementuid %in% des_mil)
  
  de_coc_pd.subnat_impatt <- de_coc_pd %>%
    dplyr::filter(dataset %in% c("impatt", "subnat"),
                  indicator_code != "IMPATT.PRIORITY_SNU.T")
  
  de_coc_pd.pzns <- de_coc_pd %>%
    dplyr::filter(indicator_code == "IMPATT.PRIORITY_SNU.T")
  
  # organisationUnits ####
  orgunits.all <- datapackr::cop22_valid_PSNUs %>%
    dplyr::filter(country_uid %in% country_uids) %>%
    tibble::as_tibble() %>%
    dplyr::select(ou, ou_id, country_name, country_uid,
                  psnu_uid, psnu_type, DREAMS)
  
  orgunits.mil <- orgunits.all %>%
    dplyr::filter(psnu_type == "Military")
  
  orgunits.nondsnu_nonmil <- orgunits.all %>%
    dplyr::filter(psnu_type == "SNU", is.na(DREAMS)) %>%
    head(., snu_n)
  
  orgunits.dreams <- orgunits.all %>%
    dplyr::filter(DREAMS == "Y") %>%
    head(., snu_n)
  
  has_dreams <- NROW(orgunits.dreams) != 0
  
  # TODO: How to handle crossing in multi-country file?
  
  # attributeOptionCombos ####
  aocs.dedupe <- c("X8hrDf6bLDC", "YGT1o7UxfFu")
  aocs.default <-
    tibble::tribble(
      ~attributeOptionCombo, ~agency,
      datapackr::default_catOptCombo(), "Unallocated Targets")
  
  if (im_allocated) {
    im_yr <- cop_year-1 # Replace this after FI-NG & DATIM updated with new mechs
    if (!include_dedupe) {
      dedupe <- ""
    } else {
      dedupe <- aocs.dedupe
    }
    ous <- datapackr::valid_PSNUs %>%
      dplyr::filter(country_uid %in% country_uids) %>%
      dplyr::pull(ou) %>%
      unique()
    
    aocs <- getMechanismView(country_uids = NULL,
                             cop_year = NULL,
                             include_dedupe = TRUE,
                             include_MOH = FALSE,
                             d2_session = d2_session) %>%
      dplyr::filter(
        (startdate < paste0(im_yr + 1, "-10-01") & enddate > paste0(im_yr, "-09-30"))
        
  # Drop dedupes if include_dedupe = FALSE
        | attributeOptionCombo %in% dedupe) %>%
      dplyr::filter(ou %in% ous | attributeOptionCombo %in% dedupe) %>%
  
  # Drop State IMs
      dplyr::filter(!stringr::str_detect(agency,"State"))
    
    aocs.filtered <- aocs %>%
      dplyr::filter(!attributeOptionCombo %in% aocs.dedupe) %>%
      dplyr::mutate(is_tbd = partner_id == "000000000") %>%
      dplyr::group_by(agency, is_tbd) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(
        aocs %>% dplyr::filter(attributeOptionCombo %in% aocs.dedupe),
        .) %>%
      dplyr::select(attributeOptionCombo, agency)
    
    aocs.mil <- aocs.filtered %>%
      dplyr::filter(agency == "DOD" | attributeOptionCombo %in% aocs.dedupe)
    
    aocs.nonmil <- aocs.filtered %>%
      dplyr::filter(agency != "DOD")
      
  } else {
    aocs.nonmil <- aocs.default
    aocs.mil <- aocs.default
  }
  
  # Combine: MER ####
  aocs.nonmil_allocation <- aocs.nonmil
  if (!im_fully_allocated) {
    aocs.nonmil_allocation %<>% dplyr::bind_rows(aocs.default, .)
  }
  
  aoc_ou_de_coc_pd.mer <-
    tidyr::crossing(aocs.nonmil_allocation, # Non Mil IMs
                    orgunits.nondsnu_nonmil, # Non DREAMS/Mil PSNUs
                    de_coc_pd.mer_nondreams) # Non DREAMS MER
  
  # !Combine: DOD ####
  aocs.mil_allocation <- aocs.mil
  if (!im_fully_allocated) {
    aocs.mil_allocation %<>% dplyr::bind_rows(aocs.default, .)
  }
  
  aoc_ou_de_coc_pd.mil <-
    tidyr::crossing(aocs.mil_allocation, # Mil IMs
                    orgunits.mil, # Mil SNUs
                    de_coc_pd.mil) # Mil MER
  
  # Combine: SUBNAT IMPATT ####
  aoc_ou_de_coc_pd.subnat_impatt <-
    tidyr::crossing(aocs.default, # No IMs
                    orgunits.nondsnu_nonmil, # Non DREAMS/Mil PSNUs
                    de_coc_pd.subnat_impatt) # SUBNAT / IMPATT DEs
  
  # Combine: Prioritizations ####
  aoc_ou_de_coc_pd.pzns <-
    tidyr::crossing(aocs.default, # No IMs
                    orgunits.nondsnu_nonmil, # Non DREAMS/Mil PSNUs
                    de_coc_pd.pzns) # Prioritization DE
  
  aoc_ou_de_coc_pd <-
    dplyr::bind_rows(aoc_ou_de_coc_pd.mer,
                     aoc_ou_de_coc_pd.mil,
                     aoc_ou_de_coc_pd.subnat_impatt,
                     aoc_ou_de_coc_pd.pzns) %>%
    dplyr::distinct()
  
  # Combine: DREAMS ####
  if (has_dreams) {
    aoc_ou_de_coc_pd.dreams <-
      tidyr::crossing(aocs.default, # No IMs
                      orgunits.dreams, # DREAMS SNUs
                      de_coc_pd.dreams) # DREAMS DEs
    
  # Combine: OVC_SERV DREAMS ####
    aoc_ou_de_coc_pd.dreams_ovc <-
      tidyr::crossing(aocs.nonmil, # Non Mil IMs
                      orgunits.dreams, # DREAMS SNUs
                      de_coc_pd.dreams_ovc) # DREAMS OVC DEs
    
    aoc_ou_de_coc_pd.dreams_ovc_mil <-
      tidyr::crossing(aocs.mil, # Mil IMs
                      orgunits.mil, # Mil SNUs
                      de_coc_pd.dreams_ovc_mil) # DREAMS OVC DEs
    
    aoc_ou_de_coc_pd %<>%
      dplyr::bind_rows(aoc_ou_de_coc_pd.dreams,
                       aoc_ou_de_coc_pd.dreams_ovc,
                       aoc_ou_de_coc_pd.dreams_ovc_mil) %>%
      dplyr::distinct()
  }
  
  # Values ####
  pzns <- datapackr::prioritizations %>%
    dplyr::filter(value != 8) %>%
    dplyr::pull(value)
  
  aoc_ou_de_coc_pd_val <- aoc_ou_de_coc_pd %>%
    dplyr::mutate(
      value = dplyr::case_when(
        attributeOptionCombo %in% aocs.dedupe ~ -100,
        indicator_code == "IMPATT.PRIORITY_SNU.T"
          ~ sample(pzns, NROW(.), replace = TRUE),
        indicator_code == "HIV_PREV.T_1" ~ runif(NROW(.), 0, 0.15),
        TRUE ~ 100))
  
  # TODO: how to manage dedupe
  
  # Introduce invalid values ####
  if (invalid_values) {
    aoc_ou_de_coc_pd_val %<>%
    # Pos for Dedupe, Neg for Non-Dedupe
      dplyr::mutate(is_dedupe = attributeOptionCombo %in% aocs.dedupe) %>%
      dplyr::group_by(is_dedupe) %>%
      dplyr::mutate(line = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        value = dplyr::case_when(line == 1 ~ value * -1,
    
    # NAs
                                 line == 2 ~ NA_real_,
    
    # Decimal in non-HIV_PREV
                                 line == 3 ~ 3.14159265359,
                                 TRUE ~ value),
    
    # Strings
        value = as.character(value),
        value = dplyr::case_when(line == 4 ~ "foo",
                                 TRUE ~ value)) %>%
      dplyr::select(-line, -is_dedupe)
  }
  
  # Cols Selection ####
  import_file <- aoc_ou_de_coc_pd_val %>%
    dplyr::mutate(value = as.character(value)) %>%
    dplyr::select(dataElement = dataelementuid,
                  period,
                  orgUnit = psnu_uid,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo,
                  value)
  
  import_file
  
}


library(magrittr)
library(datapackr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "cop-test.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "Test Data/")
cop_year = 2022

# Scenario A: Vanilla case ####
datapack_names <- c("Cameroon")
country_uids <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% datapack_names) %>%
  dplyr::pull(country_uids) %>%
  unlist()

scenario_a <- packCOP22TestDataset(country_uids = country_uids,
                                   cop_year = cop_year,
                                   im_allocated = TRUE,
                                   im_fully_allocated = TRUE,
                                   include_dedupe = TRUE,
                                   invalid_values = FALSE)

# Scenario B: Partially allocated ####
scenario_b <- packCOP22TestDataset(country_uids = country_uids,
                                   cop_year = cop_year,
                                   im_allocated = TRUE,
                                   im_fully_allocated = FALSE,
                                   include_dedupe = TRUE,
                                   invalid_values = FALSE)

# Scenario C: Unallocated ####
scenario_c <- packCOP22TestDataset(country_uids = country_uids,
                                   cop_year = cop_year,
                                   im_allocated = FALSE,
                                   im_fully_allocated = FALSE,
                                   include_dedupe = FALSE,
                                   invalid_values = FALSE)

# Scenario E: Invalid Values ####
scenario_e <- packCOP22TestDataset(country_uids = country_uids,
                                   cop_year = cop_year,
                                   im_allocated = TRUE,
                                   im_fully_allocated = TRUE,
                                   include_dedupe = TRUE,
                                   invalid_values = TRUE)

# Scenario F: Unallocated ####
datapack_names <- c("Caribbean Region")
country_uids <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% datapack_names) %>%
  dplyr::pull(country_uids) %>%
  unlist()

scenario_f <- packCOP22TestDataset(country_uids = country_uids,
                                   cop_year = cop_year,
                                   im_allocated = TRUE,
                                   im_fully_allocated = TRUE,
                                   include_dedupe = TRUE,
                                   invalid_values = FALSE)

# Scenario J: DREAMS Eswatini ####
datapack_names <- c("Eswatini")
country_uids <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% datapack_names) %>%
  dplyr::pull(country_uids) %>%
  unlist()

scenario_j_esw <- packCOP22TestDataset(country_uids = country_uids,
                                   cop_year = cop_year,
                                   im_allocated = TRUE,
                                   im_fully_allocated = TRUE,
                                   include_dedupe = TRUE,
                                   invalid_values = FALSE)

# Scenario J*: DREAMS DSNU = PSNU ####
datapack_names <- c("Uganda")
country_uids <- datapackr::COP21_datapacks_countries %>%
  dplyr::filter(datapack_name %in% datapack_names) %>%
  dplyr::pull(country_uids) %>%
  unlist()

scenario_j_ug <- packCOP22TestDataset(country_uids = country_uids,
                                   cop_year = cop_year,
                                   im_allocated = TRUE,
                                   im_fully_allocated = TRUE,
                                   include_dedupe = TRUE,
                                   invalid_values = FALSE)

# Export ####
scenarios <- list(scenario_a, scenario_b, scenario_c, scenario_e,
                  scenario_f, scenario_j_esw, scenario_j_ug)
scenario_names <- c("scenario_a", "scenario_b", "scenario_c", "scenario_e",
                    "scenario_f", "scenario_j_esw", "scenario_j_ug")
 
for (i in 1:length(scenarios)) {
  output_file_name <- 
    paste0(output_folder, "Data Pack_", scenario_names[i], "_",
           format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
  
  readr::write_csv(scenarios[[i]], output_file_name)
}
