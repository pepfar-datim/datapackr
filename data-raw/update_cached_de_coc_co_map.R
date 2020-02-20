secrets <- "/Users/scott/.secrets/test-mer2.json"
loginToDATIM(secrets)

cop_year = getCurrentCOPYear()

fullCodeList <- pullFullCodeList(FY = cop_year +1) %>%
  dplyr::left_join(
    api_call("categoryOptionCombos") %>% api_fields("id, categoryOptions") %>% api_get(),
    by = c("categoryoptioncombouid" = "id")) %>%
  dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions,~.x[["id"]] %>% 
                                                   sort() %>% paste(collapse = ".")))

map_DataPack_DATIM_DEs_COCs <- datapackr::cop20_data_pack_schema %>%
  dplyr::filter(col_type == "target") %>%
  dplyr::select(indicator_code, dataelement_dsd, dataelement_ta,
                categoryoption_specified, valid_ages, valid_sexes, valid_kps) %>%
  tidyr::unnest(cols = valid_ages, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_sexes, names_sep  = ".") %>%
  tidyr::unnest(cols = valid_kps, names_sep  = ".") %>%
  dplyr::mutate_at(c("valid_sexes.name","valid_ages.name","valid_ages.id","valid_sexes.id"),
                   ~dplyr::case_when(indicator_code == "OVC_HIVSTAT.N.total.T"
                                     ~ NA_character_,
                                     TRUE ~ .)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(categoryoption_specified = stringr::str_split(categoryoption_specified, "[.]")) %>%
  dplyr::mutate(
    valid_kps.id =
      dplyr::case_when(
        (indicator_code %in% c("KP_MAT_SUBNAT.N.Sex.T", "KP_MAT.N.Sex.T") 
          & valid_kps.id == "G6OYSzplF5a") ~ "Z1EnpTPaUfq",
        (indicator_code %in% c("KP_MAT_SUBNAT.N.Sex.T", "KP_MAT.N.Sex.T") 
         & valid_kps.id == "wyeCT63FkXB") ~ "Qn0I5FbKQOA",
        TRUE ~ valid_kps.id),
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
        TRUE ~ categoryOptions.ids)) %>%
  tidyr::pivot_longer(cols = dataelement_dsd:dataelement_ta,
                      names_to = "support_type",
                      values_to = "dataelement",
                      names_prefix = "dataelement_",
                      values_drop_na = TRUE) %>%
  dplyr::left_join(getHTSModality(cop_year = cop_year),
                   by = c("dataelement" = "dataElement")) %>%
  dplyr::left_join(getTechArea(),
                   by = c("dataelement" = "dataElement")) %>%
  dplyr::left_join(getNumeratorDenominator(),
                   by = c("dataelement" = "dataElement")) %>%
  dplyr::mutate(support_type = toupper(support_type)) %>%
  dplyr::left_join(fullCodeList,
                   by = c("dataelement" = "dataelementuid",
                          "categoryOptions.ids" = "categoryOptions"))

new <- map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified)

compare_diffs <- datapackr::map_DataPack_DATIM_DEs_COCs %>%
  dplyr::select(-categoryoption_specified) %>%
  dplyr::full_join(new, by = c("indicator_code",
                               "valid_ages.name","valid_ages.id","valid_sexes.name",
                               "valid_sexes.id","valid_kps.name","valid_kps.id",
                               "categoryOptions.ids","support_type")) %>%
  dplyr::filter(is.na(dataelement.x) | is.na(dataelement.y.y))

save(map_DataPack_DATIM_DEs_COCs, file = "./data/map_DataPack_DATIM_DEs_COCs.rda", compress = "xz")


