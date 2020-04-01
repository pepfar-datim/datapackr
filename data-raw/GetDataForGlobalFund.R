library(tidyverse)
library(magrittr)
library(devtools)
library(lubridate)
require(datapackcommons)
devtools::install_github("pepfar-datim/datapackr","pr/145-sam")

datapackr::loginToDATIM("/users/sam/.secrets/datim.json")
base_url = getOption("baseurl")
country_details <- datapackcommons::GetCountryLevels(base_url)
uids <- country_details$id
names <- country_details$country_name

beautify <- function(data) {
  if(NROW(data) == 0) {
    return(NULL)
    }
  data$data_element <-
    datimvalidation::remapDEs(data$data_element_uid,
                              mode_in = "id",
                              mode_out = "shortName")
  
  data$disagg <-
    datimvalidation::remapCategoryOptionCombos(data$category_option_combo_uid,
                                               mode_in = "id",
                                               mode_out = "name")
  psnus <-
    datapackr::valid_PSNUs %>% dplyr::select(psnu, psnu_uid)

  data %<>%
    dplyr::left_join(psnus, by = c("org_unit_uid" = "psnu_uid"))
}
# End Beautify function

sum_over_im <- function(data){
  if(NROW(data) == 0) {
    return(NULL)
  }  
  dplyr::group_by(data,
                  data_element_uid,
                  org_unit_uid,
                  category_option_combo_uid) %>%
    dplyr::summarise(datim_value = sum(datim_value)) %>%
    dplyr::ungroup()
}

# get all 2021 target data in datim by country including dedup
data <- purrr::map(uids,
                   datapackr::getCopDataFromDatim,
                   2021) %>% 
  setNames(names)
# aggregate over IM including dedup
data <- purrr::map(names,
                   ~sum_over_im(data[[.x]])) %>% 
  setNames(names)
data <- purrr::map(names,
                   ~ beautify(data[[.x]]))%>% 
  setNames(names)

data <- purrr::discard(data, ~is.null(.x))

openxlsx::write.xlsx(data, paste0("gf_data_transfer_20200401.xlsx"))
