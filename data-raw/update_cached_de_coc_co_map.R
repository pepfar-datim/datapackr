# This script begins with the Full Code List from DATIM, then combines categoryOption
# metadata, then combines this with the Data Pack schema to create a full map between
# Data Packs and DATIM for the purpose of generating import and analytics tables.

datapackr::loginToDATIM("~/.secrets/datim.json")

# Select only those datasets relevant for this COP ####
cop_year = datapackr::getCurrentCOPYear()

cop_ds_pd <- datapackr::DATIM_ds_pd_map %>%
  dplyr::filter(
    (FY == (cop_year+1) & datastream %in% c("MER","DREAMS", "SUBNAT", "IMPATT") & targets_results %in% c("Targets", "IMPATT") & DOD == "N")
    | (FY == cop_year & datastream %in% c("SUBNAT", "IMPATT") & targets_results %in% c("Targets", "IMPATT"))
    | (FY == (cop_year-1) & datastream %in% c("SUBNAT") & targets_results == "Results")
  )

# Pull Code Lists for all relevant datasets ####
ds <- data.frame()

fullCodeList <-
  purrr::map2(
    .x = cop_ds_pd$dataset.id, .y = cop_ds_pd$FY,
    function(.x, .y){
      cl <- pullDATIMCodeList(.x) %>%
        dplyr::mutate(FY = .y)
      ds <- rbind(ds, cl)
    }) %>%
  do.call(rbind, .) %>%
  dplyr::distinct() %>%
  dplyr::arrange(FY, dataelement, categoryoptioncombo) %>%
  dplyr::left_join(cop_ds_pd, by = c("FY" = "FY", "dataset.id" = "dataset.id")) %>%
  dplyr::mutate(
    period = dplyr::case_when(
      targets_results %in% c("Targets", "IMPATT") ~ paste0(FY-1, "Oct"),
      targets_results == "Results" ~ paste0(FY, "Q3")
  )) %>%
  dplyr::select(-dataset)

# Add categoryOption metadata ####
categoryoptions <- datimutils::getMetadata("categoryOptionCombos", fields = "id, categoryOptions")

fullCodeList %<>%
  dplyr::left_join(categoryoptions, by = c("categoryoptioncombouid" = "id")) %>%
  dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions,~.x[["id"]] %>%
                                                   sort() %>% paste(collapse = ".")))

  
# Combine Code Lists ####
fullCodeList %<>%
  dplyr::mutate(
    period_dataset = dplyr::case_when(
     FY == 2022 & datastream %in% c("MER","DREAMS") & targets_results == "Targets" & DOD == "N" ~ "FY22 MER Targets",
     FY == 2022 & datastream == "SUBNAT" & targets_results == "Targets" ~ "FY22 SUBNAT Targets",
     FY == 2022 & datastream == "IMPATT" ~ "FY22 IMPATT",
     FY == 2021 & datastream == "IMPATT" ~ "FY21 IMPATT",
     FY == 2021 & datastream == "SUBNAT" & targets_results == "Targets" ~ "FY21 SUBNAT Targets",
     FY == 2020 & datastream == "SUBNAT" & targets_results == "Results" ~ "FY20 SUBNAT Results"),
    dataset = dplyr::case_when(
      stringr::str_detect(period_dataset, "MER") ~ "mer",
      stringr::str_detect(period_dataset, "SUBNAT") ~ "subnat",
      stringr::str_detect(period_dataset, "IMPATT") ~ "impatt")
  )
  
# Standardize column names
  fullCodeList %<>%
    dplyr::rename(
      dataelementname = dataelement,
      categoryOptions.ids = categoryOptions,
      categoryoptioncomboname = categoryoptioncombo
    )

# Prep Data Pack schema for mapping ####
  map_DataPack_DATIM_DEs_COCs <- datapackr::cop21_data_pack_schema %>%
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

# Allow for aggregation of OVC_HIVSTAT due to SGAC-requested DATIM-Data Pack disalignment ####
    dplyr::mutate_at(c("valid_sexes.name","valid_ages.name","valid_ages.id","valid_sexes.id"),
                     ~dplyr::case_when(indicator_code == "OVC_HIVSTAT.T"
                                       ~ NA_character_,
                                       TRUE ~ .)) %>%
    dplyr::distinct()
  
# Correctly tag OVC_SERV 18+ Caregivers ####
  map_DataPack_DATIM_DEs_COCs %<>%
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
      categoryoption_specified = stringr::str_split(categoryoption_specified, "[.]")
    )

# Align KP_MAT metadata ####
    ### No longer needed. Alignment managed by Data Pack and DATIM
  # map_DataPack_DATIM_DEs_COCs %<>%
    # dplyr::mutate(
      # valid_kps.id =
      #   dplyr::case_when(
      #     (indicator_code %in% c("KP_MAT_SUBNAT.N.Sex.T", "KP_MAT.N.Sex.T")
      #       & valid_kps.id == "G6OYSzplF5a") ~ "Z1EnpTPaUfq",
      #     (indicator_code %in% c("KP_MAT_SUBNAT.N.Sex.T", "KP_MAT.N.Sex.T")
      #      & valid_kps.id == "wyeCT63FkXB") ~ "Qn0I5FbKQOA",
      #     TRUE ~ valid_kps.id))
    
# Combine all categoryOptions into a joinable list. ####
  map_DataPack_DATIM_DEs_COCs %<>%
    dplyr::mutate(
      categoryOptions.ids =
        purrr::pmap(list(valid_ages.id,
                         valid_sexes.id,
                         valid_kps.id,
                         categoryoption_specified),
                    c)) %>%
    dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, sort)) %>%
    dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, na.omit)) %>%
    dplyr::mutate(categoryOptions.ids = purrr::map_chr(categoryOptions.ids, paste, collapse = ".")) %>%
    dplyr::mutate(
      categoryOptions.ids =
        dplyr::case_when(
          categoryOptions.ids == "" ~ "xYerKDKCefk",
          TRUE ~ categoryOptions.ids))
  
# Stack DSD and TA #### 
  map_DataPack_DATIM_DEs_COCs %<>%
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
        TRUE ~ support_type
      )
    )
  
# Adorn dataset ####
  map_DataPack_DATIM_DEs_COCs %<>%
    dplyr::left_join(getHTSModality(cop_year = cop_year),
                     by = c("dataelementuid" = "dataElement"))
  
# Translate indicator_code to dataelement + categoryOptionCombo ####
  map_DataPack_DATIM_DEs_COCs %<>%
    dplyr::select(-dataset) %>%
    dplyr::full_join(fullCodeList,
                     by = c("dataelementuid" = "dataelementuid",
                            "categoryOptions.ids" = "categoryOptions.ids",
                            "period" = "period",
                            "FY" = "FY")) %>%
  dplyr::select(-dataset.id, -dataset.name, -code, -dataelementdesc, -shortname,
                -categoryoptioncombocode, -DOD) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(indicator_code))
  
# Add additional metadata for use in analytics ####
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
                         "NLZgRe4FuQJ",
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
        dataElementGroupSets.id == "NLZgRe4FuQJ" ~ "top_level",
        dataElementGroupSets.id == "TWXpUVE2MqL" ~ "support_type",
        dataElementGroupSets.id == "lD2x0c8kywj" ~ "numerator_denominator",
        TRUE ~ dataElementGroupSets.name
      )
  ) %>%
  dplyr::select(-dataElementGroupSets.id) %>%
  tidyr::pivot_wider(names_from = dataElementGroupSets.name,
                     values_from = dataElementGroups.name,
                     values_fill = NA)

map_DataPack_DATIM_DEs_COCs %<>%
  dplyr::left_join(getHIVSpecific(), by = "categoryoptioncombouid") %>%
  dplyr::left_join(getHIVInclusive(), by = "categoryoptioncombouid") %>%
  dplyr::rename(support_type_dp = support_type) %>%
  dplyr::left_join(degs_map, by = "dataelementuid") %>%
  dplyr::mutate(
    support_type = dplyr::if_else(is.na(support_type), support_type_dp, support_type)
  ) %>%
  dplyr::select(-support_type_dp)

# Compare old and new maps for accuracy ####
new <- map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified) %>%
  dplyr::mutate(new = "Y")

compare_diffs <- datapackr::map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified) %>%
  dplyr::mutate(in_pkg = "Y") %>%
  dplyr::full_join(new, by = c("indicator_code", "col_type", "value_type",
                               "dataelementuid", "dataelementname",
                               "categoryoptioncombouid", "categoryoptioncomboname",
                               "FY", "period", "targets_results",
                               "valid_ages.name","valid_ages.id","valid_sexes.name",
                               "valid_sexes.id","valid_kps.name","valid_kps.id",
                               "categoryOptions.ids","support_type", "disagg_type",
                               "technical_area", "top_level", "numerator_denominator",
                               "resultstatus","resultstatus_inclusive", "hts_modality")) %>%
  dplyr::filter(is.na(new) | is.na(in_pkg)) %>%
  dplyr::arrange(dplyr::across(c(-in_pkg, -new, dplyr::everything())))


save(map_DataPack_DATIM_DEs_COCs, file = "./data/map_DataPack_DATIM_DEs_COCs.rda", compress = "xz")
