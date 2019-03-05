library(magrittr)

secrets <- "/Users/scott/.secrets/triage.json"

datapackr::loginToDATIM(secrets)



country_uids <- datapackr::dataPackMap %>%
  dplyr::filter(data_pack_name == d$info$datapack_name) %>%
  dplyr::pull(country_uid)





##########
sites <- datapackr::getSiteList(country_uids,
                                include_mil = TRUE) %>%
  dplyr::select(psnu,psnu_uid,site_tool_label)

distributedSite <- d$data$distributedMER %>%
  dplyr::filter(value != 0) %>%
  dplyr::select(PSNU,psnuid,sheet_name,indicatorCode,Age,Sex,KeyPop,mechanismCode) %>%
  dplyr::distinct() %>%
  tidyr::crossing(data.frame(type = c("DSD","TA"))) %>%
  dplyr::left_join(sites, by = c("psnuid" = "psnu_uid")) %>%
  dplyr::sample_frac(size = 0.5) %>%
  dplyr::select(psnuid,indicatorCode,mechanismCode,Age,Sex,KeyPop,type,site_tool_label) %>%
  dplyr::mutate(value = sample(x = 10000, size = NROW(.), replace = TRUE)) %>%
  dplyr::group_by(psnuid,indicatorCode,mechanismCode,Age,Sex,KeyPop,type) %>%
  dplyr::mutate(percent = value / sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(psnuid,indicatorCode,mechanismCode,Age,Sex,KeyPop,type,site_tool_label) %>%
  dplyr::select(-value)

d$data$site$distributed <- d$data$distributedMER %>%
  dplyr::select(-CoarseAge) %>%
  dplyr::filter(value != 0) %>%
  dplyr::left_join(distributedSite, by = c("psnuid" = "psnuid",
                                           "indicatorCode" = "indicatorCode",
                                           "Age" = "Age",
                                           "Sex" = "Sex",
                                           "KeyPop" = "KeyPop",
                                           "mechanismCode" = "mechanismCode")) %>%
  dplyr::mutate(siteValue = value * percent,
                siteValue = datapackr::round_trunc(siteValue)) %>%
  dplyr::select(sheet_name,indicatorCode,PSNU,psnuid,mechanismCode,
                Age,Sex,KeyPop,type,site_tool_label,
                value,percent,siteValue)
  