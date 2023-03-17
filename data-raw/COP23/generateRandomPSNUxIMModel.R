
dp_map <- getMapDataPack_DATIM_DEs_COCs(2023) %>%
  dplyr::select(indicator_code,
                type = support_type,
                age_option_name = valid_ages.name,
                age_option_uid = valid_ages.id,
                sex_option_name = valid_sexes.name,
                sex_option_uid = valid_sexes.id,
                kp_option_name = valid_kps.name,
                kp_option_uid = valid_kps.id
                )
#Choose a few random mechs and distribute

mechs_dist <- data.frame(
  mechanism_code = c("18025", "84562"),
  mechanism_uid = c("NNHq2qVK085","t7xQ6GDj57P" ),
  percent = c(0.75, 0.25)
)

d <- unPackTool("tests/testthat/sheets/COP23_sample_DataPack_Malawi.xlsx")

p <- d$data$analytics %>%
  dplyr::select(indicator_code,
               psnu_uid,
               type = support_type,
               age_option_name = age,
               sex_option_name = sex,
               kp_option_name = key_population,
               categoryoptioncombo_id,
               dataelement_id,
               target_value
               ) %>%
  dplyr::left_join(dp_map) %>%
  dplyr::full_join(mechs_dist, by = character()) %>%
  dplyr::mutate(value = ceiling(target_value * percent * 0.85))


model <- list()

model$`lZsCb6y0KDX` <-   p %>%
  dplyr::select( "indicator_code",
                 "psnu_uid",
                 "mechanism_uid","mechanism_code",
                 "type",
                 "age_option_name",
                 "age_option_uid",
                 "sex_option_name",
                 "sex_option_uid",
                 "kp_option_name",
                 "kp_option_uid",
                 "value",
                 "percent")

saveRDS(model, "tests/testthat/sheets/COP23_SNUxIM_Model_Random.rds")
