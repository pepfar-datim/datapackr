library(tidyverse)
library(magrittr)
library(devtools)
library(lubridate)
require(datapackcommons)
require(datapackr)

datimutils::loginToDATIM("/users/sam/.secrets/datim.json")
base_url = getOption("baseurl")
country_details <- datapackcommons::GetCountryLevels(base_url)
uids <- country_details$id
names <- country_details$country_name


beautify <- function(data) {
  if(NROW(data) == 0) {
    return(NULL)
  }
  
  included_indicators <- c("HTS_TST (N, DSD, KeyPop/Result) TARGET"	,
                           "HTS_TST (N, TA, KeyPop/Result) TARGET"	,
                           "IMPATT.PLHIV (N, SUBNAT, Age/Sex/HIVStatus) TARGET"	,
                           "KP_MAT (N, DSD, Sex) TARGET"	,
                           "KP_MAT (N, TA, Sex) TARGET"	,
                           "KP_PREV (N, DSD, KeyPop) TARGET v2"	,
                           "KP_PREV (N, TA, KeyPop) TARGET v2"	,
                           "PMTCT_ART (N, DSD, Age/NewExistArt/Sex/HIV) TARGET"	,
                           "PMTCT_ART (N, TA, Age/NewExistArt/Sex/HIV) TARGET"	,
                           "PrEP_NEW (N, DSD, Age/Sex) TARGET v2"	,
                           "PrEP_NEW (N, DSD, KeyPop) TARGET"	,
                           "PrEP_NEW (N, TA, Age/Sex) TARGET v2"	,
                           "PrEP_NEW (N, TA, KeyPop) TARGET"	,
                           "TB_ART (N, DSD, Age/Sex/NewExArt/HIV) TARGET"	,
                           "TB_ART (N, TA, Age/Sex/NewExArt/HIV) TARGET"	,
                           "TB_PREV (D, DSD, Age/Sex/NewExArt/HIV) TARGET"	,
                           "TB_PREV (D, TA, Age/Sex/NewExArt/HIV) TARGET"	,
                           "TB_PREV (N, DSD, Age/Sex/NewExArt/HIV) TARGET"	,
                           "TB_PREV (N, TA, Age/Sex/NewExArt/HIV) TARGET"	,
                           "TB_STAT (D, DSD, Age/Sex) TARGET"	,
                           "TB_STAT (D, TA, Age/Sex) TARGET"	,
                           "TB_STAT (N, DSD, Age/Sex/KnownNewPosNeg) TARGET"	,
                           "TB_STAT (N, TA, Age/Sex/KnownNewPosNeg) TARGET"	,
                           "TX_CURR (N, DSD, Age/Sex/HIVStatus) TARGET"	,
                           "TX_CURR (N, DSD, KeyPop/HIVStatus) TARGET"	,
                           "TX_CURR (N, TA, Age/Sex/HIVStatus) TARGET"	,
                           "TX_CURR (N, TA, KeyPop/HIVStatus) TARGET"	,
                           "TX_CURR_SUBNAT (N, SUBNAT, Age/Sex/HIV) TARGET"	,
                           "TX_TB (D, DSD, Age/Sex/TBScreen/ART/HIV) TARGET"	,
                           "TX_TB (D, TA, Age/Sex/TBScreen/ART/HIV) TARGET"	,
                           "VMMC_CIRC (N, DSD, Age/Sex/HIVStatus) TARGET"	,
                           "VMMC_CIRC (N, TA, Age/Sex/HIVStatus) TARGET"	)
  
  data$data_element <-
    datimvalidation::remapDEs(data$dataElement,
                              mode_in = "id",
                              mode_out = "shortName")
  
  data <- dplyr::filter(data, data_element %in% included_indicators)
  
  data$disagg <-
    datimvalidation::remapCategoryOptionCombos(data$categoryOptionCombo,
                                               mode_in = "id",
                                               mode_out = "name")
  psnus <-
    datapackr::valid_PSNUs %>% dplyr::select(psnu, psnu_uid)

  data %<>%
    dplyr::left_join(psnus, by = c("orgUnit" = "psnu_uid")) %>% 
    dplyr::filter(!stringr::str_detect(psnu, "_Military"))
}
# End Beautify function

sum_over_im <- function(data){
  if(NROW(data) == 0) {
    return(NULL)
  }  
  dplyr::group_by(data,
                  dataElement,
                  orgUnit,
                  categoryOptionCombo) %>%
    dplyr::summarise(datim_value = sum(value)) %>%
    dplyr::ungroup()
}

# get all 2021 target data in datim by country including dedup
data <- purrr::map(uids,
                   datapackr::getCOPDataFromDATIM,
                   cop_year = 2020) %>% 
  setNames(names)
# aggregate over IM including dedup
data <- purrr::map(names,
                   ~sum_over_im(data[[.x]])) %>% 
  setNames(names)
data <- purrr::map(names,
                   ~ beautify(data[[.x]]))%>% 
  setNames(names)

data <- purrr::discard(data, ~is.null(.x))


openxlsx::write.xlsx(data, paste0("gf_data_transfer_20200403_1.xlsx"))
