# This script begins with the Full Code List from DATIM, then combines categoryOption
# metadata, then combines this with the Data Pack schema to create a full map between
# Data Packs and DATIM for the purpose of generating import and analytics tables.

datapackr::loginToDATIM("~/.secrets/test-mer2.json")

cop_year = 2022

# Pull code lists ####
datasets_to_pull <- tibble::tribble(
  ~dataset_uid, ~dataset_name, ~FY, ~targets_results, ~datastream, ~org_unit,
  "iADcaCD5YXh", "FY23 MER Targets", 2023, "targets", "mer", "psnu",
  "cihuwjoY5xP", "FY23 MER DOD Targets", 2023, "targets", "mer", "_mil",
  "vzhO50taykm", "FY23 DREAMS Targets", 2023, "targets", "dreams", "dsnu",
  "CxMsvlKepvE", "FY23 IMPATT", 2023, "targets", "impatt", "psnu",
  "Zn27xns9Fmx", "FY22 IMPATT", 2022, "targets", "impatt", "psnu",
  "J4tdiDEi08O", "FY23 SUBNAT Targets", 2023, "targets", "subnat", "psnu",
  "Va7TYyHraRn", "FY22 SUBNAT Targets", 2022, "targets", "subnat", "psnu",
  "xiTCzZJ2GPP", "FY21 SUBNAT Results", 2021, "results", "subnat", "psnu")

ds <- data.frame()

fullCodeList <-
  lapply(
    datasets_to_pull$dataset_uid,
    function(x) {
      cl <- pullDATIMCodeList(x) %>%
        dplyr::mutate(dataset_uid = x)
      ds <- rbind(ds, cl)
    }) %>%
  do.call(rbind, .) %>%
  dplyr::left_join(
    dplyr::select(datasets_to_pull, -org_unit),
    by = c("dataset_uid" = "dataset_uid"))

dod_des <- fullCodeList %>%
  dplyr::filter(dataset_uid == "cihuwjoY5xP") %>%
  dplyr::pull(dataelementuid) %>%
  unique()

dsnu_des <- fullCodeList %>%
  dplyr::filter(dataset_uid == "vzhO50taykm") %>%
  dplyr::pull(dataelementuid) %>%
  unique()

fullCodeList %<>%
  dplyr::mutate(
    org_unit =
      dplyr::case_when(
        dataelementuid %in% dod_des ~ "_mil",
        dataelementuid %in% dsnu_des ~ "dsnu")) %>%
  dplyr::select(-dataset, -dataset_uid, -dataset_name) %>%
  dplyr::distinct()

## Combine Code Lists ####
fullCodeList %<>%
  dplyr::mutate(
    period_dataset =
      paste0(
        "FY", FY-2000, " ", toupper(datastream),
        dplyr::if_else(datastream == "impatt", "",
                       paste0(" ", stringr::str_to_title(targets_results)))),
    period = dplyr::case_when(
      targets_results == "targets" ~ paste0(FY-1, "Oct"),
      targets_results == "results" ~ paste0(FY, "Q3")))
    
## Add metadata for categoryOptions ####
categoryoptions <-
  datimutils::getMetadata(
    end_point = "categoryOptionCombos",
    "categoryCombo.id:!eq:wUpfppgjEza",
    fields = "id, categoryOptions")

fullCodeList %<>%
  dplyr::left_join(categoryoptions, by = c("categoryoptioncombouid" = "id")) %>%
  dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions,~.x[["id"]] %>%
                                                   sort() %>% paste(collapse = ".")))
  
## Standardize some column names ####
fullCodeList %<>%
  dplyr::rename(
    dataset = datastream,
    dataelementname = dataelement,
    categoryOptions.ids = categoryOptions,
    categoryoptioncomboname = categoryoptioncombo)

# Prep Data Pack schema for mapping ####
schema <- datapackr::cop22_data_pack_schema
  
dp_map <- schema %>%
  dplyr::filter((col_type == "target" & dataset %in% c("mer", "subnat", "impatt"))
                | dataset == "subnat" & col_type == "result",
                !is.na(FY)) %>%
  dplyr::select(indicator_code, dataset, col_type, value_type,
                dataelement_dsd, dataelement_ta,
                categoryoption_specified, valid_ages, valid_sexes, valid_kps,
                FY, period) %>%
  tidyr::unnest(cols = valid_ages, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_sexes, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_kps, names_sep  = ".") %>%
  dplyr::distinct()
  
## Correctly tag OVC_SERV 18+ Caregivers ####
dp_map %<>%
  dplyr::mutate(
    dataelement_dsd =
      dplyr::case_when(
        indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
            & valid_ages.name == "18+"
          ~ stringr::str_extract(dataelement_dsd, "(?<=\\.)([A-Za-z][A-Za-z0-9]{10})$"),
        indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
            & valid_ages.name != "18+"
          ~ stringr::str_extract(dataelement_dsd, "^([A-Za-z][A-Za-z0-9]{10})(?=\\.)"),
        TRUE ~ dataelement_dsd),
    dataelement_ta = dplyr::case_when(
        indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
            & valid_ages.name == "18+"
          ~ stringr::str_extract(dataelement_ta, "(?<=\\.)([A-Za-z][A-Za-z0-9]{10})$"),
        indicator_code %in% c("OVC_SERV.Active.T","OVC_SERV.Grad.T")
            & valid_ages.name != "18+"
          ~ stringr::str_extract(dataelement_ta, "^([A-Za-z][A-Za-z0-9]{10})(?=\\.)"),
        TRUE ~ dataelement_ta),
    categoryoption_specified = stringr::str_split(categoryoption_specified, "[.]"))

## Allow aggregation of OVC_HIVSTAT ####
dp_map %<>%
  dplyr::mutate_at(c("valid_ages.name", "valid_ages.id", "valid_sexes.name", "valid_sexes.id"),
                   ~dplyr::case_when(indicator_code == "OVC_HIVSTAT.T"
                                     ~ NA_character_,
                                     TRUE ~ .)) %>%
  dplyr::distinct()
    
## Remap 50+ age bands where necessary ####
fine_sr_age_des <- fullCodeList %>%
  dplyr::filter(stringr::str_detect(categoryOptions.ids, "SMXPADytkkF|RQbUeV6OAVk|C0GAyd5PaGn|HuWOqxjK4D5")) %>%
  dplyr::pull(dataelementuid) %>%
  unique()

dp_map %<>%
  dplyr::mutate(
    valid_ages.id_mapped =
      dplyr::if_else(
        !(dataelement_dsd %in% fine_sr_age_des | dataelement_ta %in% fine_sr_age_des),
        stringr::str_replace(
          string = valid_ages.id,
          pattern = "SMXPADytkkF|RQbUeV6OAVk|C0GAyd5PaGn|HuWOqxjK4D5",
          replacement = "TpXlQcoXGZF"),
        valid_ages.id))

## Combine all categoryOptions into a joinable list. ####
dp_map %<>%
  dplyr::mutate(
    categoryOptions.ids =
      purrr::pmap(list(valid_ages.id_mapped,
                       valid_sexes.id,
                       valid_kps.id,
                       categoryoption_specified),
                  c)) %>%
  dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, sort)) %>%
  dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, na.omit)) %>%
  dplyr::mutate(categoryOptions.ids = purrr::map_chr(categoryOptions.ids, paste, collapse = ".")) %>%

## Apply default categoryOption against dataElements with no assigned cOs. ####
  dplyr::mutate(
    categoryOptions.ids =
      dplyr::case_when(
        categoryOptions.ids == "" ~ "xYerKDKCefk",
        TRUE ~ categoryOptions.ids))
  
## Stack DSD and TA #### 
dp_map %<>%
  tidyr::pivot_longer(cols = dataelement_dsd:dataelement_ta,
                      names_to = "support_type",
                      values_to = "dataelementuid",
                      names_prefix = "dataelement_",
                      values_drop_na = TRUE) %>%
  dplyr::mutate(support_type = toupper(support_type)) %>%
  dplyr::mutate(
    support_type = dplyr::case_when(
      stringr::str_detect(indicator_code, "^AGYW_PREV") ~ "No Support Type",
      dataset %in% c("impatt","subnat") ~ "Sub-National",
      TRUE ~ support_type))
  
# Accommodate oddities with FY21 PMTCT_SUBNAT
# I believe this is now accommodated for in FY22-23
# dp_map %<>%
#   dplyr::mutate(
#     FY = dplyr::case_when(
#       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~ FY+1,
#       TRUE ~ FY),
#     period = dplyr::case_when(
#       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~ paste0(FY-1, "Oct"),
#       TRUE ~ period))
  
# Join Full Code List with Schema ####
dp_map %<>%
  dplyr::select(-dataset) %>%
  dplyr::full_join(fullCodeList,
                   by = c("dataelementuid" = "dataelementuid",
                          "categoryOptions.ids" = "categoryOptions.ids",
                          "period" = "period",
                          "FY" = "FY"))

# Readjust PMTCT SUBNAT
# dp_map %<>%
#   dplyr::mutate(
#     FY = dplyr::case_when(
#       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~ FY-1,
#       TRUE ~ FY),
#     period = dplyr::case_when(
#       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~ paste0(FY-1,"Oct"),
#       TRUE ~ period),
#     period_dataset = dplyr::case_when(
#       stringr::str_detect(indicator_code, "PMTCT_(.*)_SUBNAT(.*)\\.T_1$") ~ 
#         stringr::str_replace(period_dataset, "(?<=FY)\\d{2}", stringr::str_sub(FY,-2,-1)),
#       TRUE ~ period_dataset))
#   

# Add additional metadata for use in analytics ####

dp_map %<>%
  dplyr::left_join(getHTSModality(cop_year = cop_year),
                   by = c("dataelementuid" = "dataElement"))

getCOGSMap <- function(uid,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {

  r <- datimutils::getCatOptionGroupSets(values = uid,
                                         by = "id",
                                         fields = "categoryOptionGroups[id,name,categoryOptions[categoryOptionCombos[id,name]]]",
                                         d2_session = d2_session) %>%
    dplyr::rename(categoryOptionGroupSets.name = name,
                  categoryOptionGroupSets.id = id) %>%
    tidyr::unnest(categoryOptionGroups) %>%
    dplyr::rename(categoryOptionGroups.name = name,
                  categoryOptionGroups.id = id) %>%
    tidyr::unnest(categoryOptions) %>%
    tidyr::unnest(categoryOptionCombos) %>%
    dplyr::rename(categoryOptionCombos.name = name,
                  categoryOptionCombos.id = id)

  return(r)
}


getHIVSpecific <- function(d2_session = dynGet("d2_default_session",
                                              inherits = TRUE)) {
  
  hiv_specific <- getCOGSMap("bDWsPYyXgWP",
                             d2_session = d2_session) %>% #HIV Test Status (Specific)
    dplyr::select("categoryoptioncombouid" = categoryOptionCombos.id,
                  "resultstatus" = categoryOptionGroups.name) %>%
    dplyr::mutate(
      resultstatus = stringr::str_replace(resultstatus,"\\(Specific\\)",""),
      resultstatus = stringr::str_replace(resultstatus,"HIV",""),
      resultstatus = stringr::str_trim(resultstatus))
 
  return(hiv_specific)
   
}

getHIVInclusive <- function(d2_session = dynGet("d2_default_session",
                                               inherits = TRUE)) {

  hiv_inclusive <- getCOGSMap("ipBFu42t2sJ",
                              d2_session = d2_session) %>% # HIV Test Status (Inclusive)
    dplyr::select("categoryoptioncombouid" = categoryOptionCombos.id,
                  "resultstatus_inclusive" = categoryOptionGroups.name) %>%
    dplyr::mutate(
      resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"\\(Inclusive\\)",""),
      resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"HIV",""),
      resultstatus_inclusive = stringr::str_replace(resultstatus_inclusive,"Status",""),
      resultstatus_inclusive = stringr::str_trim(resultstatus_inclusive))

  return(hiv_inclusive)
}


getDEGSMap <- function(uid,
                       d2_session = dynGet("d2_default_session",
                                           inherits = TRUE)) {

  r <- datimutils::getDataElementGroupSets(values = uid,
                                           by = "id",
                                           fields = "id,name,dataElementGroups[name,id,dataElements[name,id]]",
                                           d2_session = d2_session) %>%
    dplyr::rename(dataElementGroupSets.name = name,
                  dataElementGroupSets.id = id) %>%
    tidyr::unnest(dataElementGroups) %>%
    dplyr::rename(dataElementGroups.name = name,
                  dataElementGroups.id = id) %>%
    tidyr::unnest(dataElements) %>%
    dplyr::rename(dataElements.name = name,
                  dataElements.id = id) %>%
    dplyr::distinct()
  
  return(r)

}

degs_map <- getDEGSMap(c("HWPJnUTMjEq",
                         "LxhLO68FcXm",
                         "dDkGyJpCY4c",
                         "TWXpUVE2MqL",
                         "lD2x0c8kywj")) %>%
  dplyr::select(dataElementGroupSets.name, dataElementGroupSets.id,
                dataElementGroups.name, dataelementuid = dataElements.id) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    dataElementGroupSets.name =
      dplyr::case_when(
        dataElementGroupSets.id == "HWPJnUTMjEq" ~ "disagg_type",
        dataElementGroupSets.id == "LxhLO68FcXm" ~ "technical_area",
        dataElementGroupSets.id == "dDkGyJpCY4c" ~ "top_level",
        dataElementGroupSets.id == "TWXpUVE2MqL" ~ "support_type",
        dataElementGroupSets.id == "lD2x0c8kywj" ~ "numerator_denominator",
        TRUE ~ dataElementGroupSets.name
      )
  ) %>%
  dplyr::select(-dataElementGroupSets.id) %>%
  tidyr::pivot_wider(names_from = dataElementGroupSets.name,
                     values_from = dataElementGroups.name,
                     values_fill = NA)

dp_map %<>%
  dplyr::left_join(getHIVSpecific(), by = "categoryoptioncombouid") %>%
  dplyr::left_join(getHIVInclusive(), by = "categoryoptioncombouid") %>%
  dplyr::rename(support_type_dp = support_type) %>%
  dplyr::left_join(degs_map, by = "dataelementuid") %>%
  dplyr::mutate(
    support_type = dplyr::if_else(is.na(support_type), support_type_dp, support_type)
  ) %>%
  dplyr::select(-support_type_dp)

# Get columns all in order ####
dp_map %<>%
  dplyr::select(
    indicator_code, col_type, value_type, categoryoption_specified,
    valid_ages.name, valid_ages.id, valid_sexes.name, valid_sexes.id,
    valid_kps.name, valid_kps.id, FY, period, categoryOptions.ids,
    dataelementuid, hts_modality, period_dataset, dataelementname,
    categoryoptioncomboname, categoryoptioncombouid, targets_results,
    dataset, resultstatus, resultstatus_inclusive, disagg_type, technical_area,
    top_level, support_type, numerator_denominator
  )

# Compare old and new maps for accuracy ####
new <- dp_map %>%
  dplyr::select(-categoryoption_specified)

compare_diffs <- datapackr::cop22_map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified) %>%
  dplyr::full_join(new, by = c("indicator_code",
                               "dataelementuid",
                               "categoryoptioncombouid",
                               "FY",
                               "valid_ages.name","valid_ages.id","valid_sexes.name",
                               "valid_sexes.id","valid_kps.name","valid_kps.id",
                               "categoryOptions.ids","support_type","resultstatus","resultstatus_inclusive")) %>%
  dplyr::filter(is.na(indicator_code) | is.na(dataelementname.x) | is.na(dataelementname.y))

# Expected changes from COP21:
# - finer age bands on DATIM side for TX_CURR only
# - finer age bands on DP side, mapped to 50+ on DATIM side
# - PrEP_CT instead of PrEP_CURR
# - New SNS modality in HTS_TST and HTS_RECENT
# - No need to remap PMTCT_STAT_SUBNAT in weird ways
# - AGYW_PREV listed as "dreams" dataset instead of "mer"
# - Updates to DEGS for HTS modalities and top level to reflect FY23 targets changes

cop22_map_DataPack_DATIM_DEs_COCs <- dp_map

save(cop22_map_DataPack_DATIM_DEs_COCs, file = "./data/cop22_map_DataPack_DATIM_DEs_COCs.rda", compress = "xz")
