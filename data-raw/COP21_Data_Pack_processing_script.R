library(datapackr)
library(magrittr)

# Point to DATIM login secrets ####
secrets <- Sys.getenv("SECRETS_FOLDER") %>% paste0(., "datim.json")
datimutils::loginToDATIM(secrets)

output_folder <- Sys.getenv("OUTPUT_FOLDER") %>% paste0(., "COP21 OPUs/")
model_data_path <- Sys.getenv("MODEL_DATA_PATH")
snuxim_model_data_path <- Sys.getenv("SNUXIM_MODEL_DATA_PATH")

# Unpack Submitted Data Pack ####
d <- unPackTool(cop_year = 2021)

d <- checkAnalytics(d,
                   model_data_path)

d <- writePSNUxIM(d, snuxim_model_data_path, output_folder)






# Export DATIM import files ####
  exportPackr(data = d$datim$MER,
              output_folder = output_folder,
              type = "DATIM Export File",
              datapack_name = d$info$datapack_name)




# Produce Beta Pack data for PAW ####
  d$keychain$snuxim_model_data_path = snuxim_model_data_path
  d$keychain$output_folder = output_folder

  d$data$snuxim_model_data <- readRDS(d$keychain$snuxim_model_data_path)[d$info$country_uids] %>%
    dplyr::bind_rows()

  dsd_ta <- tibble::tribble(
    ~type,
    "DSD",
    "TA"
  )

  d$data$SNUxIM_combined <- d$data$MER %>%
    tidyr::crossing(dsd_ta)

  d$data$snuxim_model_data %<>%
    dplyr::left_join(
      (cop20_data_pack_schema %>%
         dplyr::filter(dataset %in% c("mer","subnat","impatt")
                       & col_type == "target") %>%
         dplyr::select(indicator_code, dataelement_dsd, dataelement_ta) %>%
         tidyr::pivot_longer(cols = tidyselect::all_of(c("dataelement_dsd", "dataelement_ta")),
                             names_to = "type",
                             values_to = "dataelement") %>%
         dplyr::mutate(
           type = stringr::str_remove(type, "^dataelement_"),
           type = toupper(type)) %>%
         tidyr::drop_na(dataelement)),
      by = c("indicator_code" = "indicator_code",
             "type" = "type")) %>%
    dplyr::select(-type, -indicator_code, -value)

  d$data$SNUxIM_combined %<>%
    dplyr::left_join(
      (cop21_data_pack_schema %>%
         dplyr::filter(dataset %in% c("mer","subnat","impatt")
                       & col_type == "target") %>%
         dplyr::select(indicator_code, dataelement_dsd, dataelement_ta) %>%
         tidyr::pivot_longer(cols = tidyselect::all_of(c("dataelement_dsd", "dataelement_ta")),
                             names_to = "type",
                             values_to = "dataelement") %>%
         dplyr::mutate(
           type = stringr::str_remove(type, "^dataelement_"),
           type = toupper(type)) %>%
         tidyr::drop_na(dataelement)),
      by = c("indicator_code" = "indicator_code",
             "type" = "type")
    )

  d$data$SNUxIM_combined %<>%
    dplyr::left_join(
      d$data$snuxim_model_data,
      by = c("psnuid" = "psnu_uid",
             "dataelement" = "dataelement",
             "Age" = "age_option_name",
             "Sex" = "sex_option_name",
             "KeyPop" = "kp_option_name")
    ) %>%
    dplyr::select(-age_option_uid, -sex_option_uid, -kp_option_uid) %>%
    tidyr::drop_na(value) %>%
    dplyr::mutate(distributed_value = value * percent,
                  distributed_value_rounded = datapackr::round_trunc(distributed_value)) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age,
                  Sex, KeyPop, mechanism_code, support_type = type,
                  value = distributed_value) %>%
    dplyr::mutate(value = round_trunc(value)) %>%
    dplyr::filter(value != 0) %>%
    tidyr::drop_na(value)

  map_DataPack_DATIM_DEs_COCs_local <- datapackr::cop21_data_pack_schema %>%
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
    dplyr::mutate(support_type = toupper(support_type))

  fullCodeList <- pullFullCodeList(FY = 2022,
                                   datastream = c("mer_targets", "subnat_targets", "impatt")) %>%
    dplyr::left_join(
      datimutils::getMetadata(categoryOptionCombos,
                              fields = "id,categoryOptions",
                              "categoryCombo.id:ne:wUpfppgjEza"),
      by = c("categoryoptioncombouid" = "id")) %>%
    dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions,~.x[["id"]] %>%
                                                     sort() %>% paste(collapse = ".")))

  map_DataPack_DATIM_DEs_COCs_local %<>%
    dplyr::left_join(fullCodeList,
                     by = c("dataelement" = "dataelementuid",
                            "categoryOptions.ids" = "categoryOptions"))

  d$datim$MER <- d$data$SNUxIM_combined %>%
    dplyr::left_join(., ( map_DataPack_DATIM_DEs_COCs_local %>%
                            dplyr::rename(Age = valid_ages.name,
                                          Sex = valid_sexes.name,
                                          KeyPop = valid_kps.name) )) %>%
    dplyr::mutate(
      period = paste0(d$info$cop_year,"Oct") ) %>%
    dplyr::select(
      dataElement = dataelement,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo = mechanism_code,
      value) %>%
    dplyr::group_by(dataElement, period, orgUnit,categoryOptionCombo,
                    attributeOptionCombo) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(complete.cases(.))

  exportPackr(
    data = d$datim$MER,
    output_folder = d$keychain$output_folder,
    type = "DATIM Export File",
    datapack_name = d$info$datapack_name
  )
