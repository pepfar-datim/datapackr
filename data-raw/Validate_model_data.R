# Validate model_data

model_data_path <- file.choose()

model_data <- readRDS(model_data_path) %>%
  dplyr::bind_rows()

# Validate categoryOptions
valid.cos <- datapackr::cop22_data_pack_schema %>%
  tidyr::unnest(valid_ages, .drop = FALSE, .sep = ".") %>%
  tidyr::unnest(valid_sexes, .drop = FALSE, .sep = ".") %>%
  tidyr::unnest(valid_kps, .drop = FALSE, .sep = ".") %>%
  unique()

model_data.cos <- model_data %>%
  dplyr::select(indicator_code, age_option_uid, sex_option_uid,
                kp_option_uid, period) %>%
  dplyr::distinct() %>%
  dplyr::mutate(in_model = 1) %>%
  dplyr::left_join(
    valid.cos,
    by = c("indicator_code" = "indicator_code",
           "age_option_uid" = "valid_ages.id",
           "sex_option_uid" = "valid_sexes.id",
           "kp_option_uid" = "valid_kps.id"))

invalid.cos <- model_data.cos %>%
  dplyr::filter(
    (is.na(valid_ages.name) & is.na(valid_sexes.name) & is.na(valid_kps.name))
    | is.na(sheet_name)
    | is.na(in_model))

# Validate PSNUs
invalid.PSNUs <- model_data %>%
  dplyr::filter(!psnu_uid %in% datapackr::valid_PSNUs$psnu_uid) %>%
  dplyr::select(psnu_uid)

# Validate Values
invalid.values <- model_data %>%
  dplyr::left_join(
    datapackr::cop22_data_pack_schema %>%
      dplyr::select(indicator_code, value_type) %>%
      dplyr::distinct(),
    by = c("indicator_code" = "indicator_code")) %>%
  dplyr::filter((round(value) != value) & (value_type == "integer"))


# Check for missing data


indicator_code_comparison <- model_data %>%
  dplyr::mutate(in_model = 1,
                na_value = is.na(value)) %>%
  dplyr::select(indicator_code, in_model, na_value) %>%
  dplyr::distinct() %>%
  dplyr::full_join(
    datapackr::cop22_data_pack_schema %>%
      dplyr::filter(col_type %in% c("past", "calculation")) %>%
      dplyr::select(indicator_code) %>%
      dplyr::mutate(in_schema = 1),
    by = c("indicator_code" = "indicator_code"))

missing.indicator_codes <- indicator_code_comparison %>%
  dplyr::filter(is.na(in_model))

na.indicator_codes <- indicator_code_comparison %>%
  dplyr::filter(indicator_code %in%
                  na.omit(indicator_code_comparison$indicator_code[indicator_code_comparison$na_value]))
