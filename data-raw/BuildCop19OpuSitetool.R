devtools::install_github("https://github.com/pepfar-datim/datapackr",
                         "COP-19-Master",
                         upgrade = FALSE)

country_name <-  "Burundi" ### if it is a region, escalate to sam
secrets <- "~/.secrets/datim.json"
output_path<-"~/Documents/cop_19_data/opus"


require(jsonlite)
require(assertthat)
require(glue)
require(datapackr)

require(magrittr)
require(httr)
require(dplyr)
require(stringr)

# Main config options
d=NULL
country_details <-  datapackr::dataPackMap %>% 
  tibble::as_tibble() %>%
  dplyr::filter(country_name == !!country_name)

assertthat::assert_that(NROW(country_details) == 1)

d$info$datapack_name = country_details$data_pack_name
d$info$datapack_uid = country_details$country_uid

if (d$info$datapack_name != country_name){
  stop("seems like a regional data pack")
}


#' Pull data for the relevant data sets from DATIM for FY20, excluding dedups and deleted values.
#' 
#' @param country_uid UID of the country OU
#' @return dataset
getCOP19DataFromAPI<-function(country_uid) {
  parameters <- tibble::tribble(
    ~ key, ~ value,
    "dataSet", "nIHNMxuPUOR",
    "dataSet", "sBv1dj90IX6",
    "dataSet", "C2G7IyPPrvD",
    "dataSet", "HiJieecLXxN",
    "period",  "2019Oct",
    "children", "true",
    "categoryOptionComboIdScheme", "code",
    "includeDeleted", "false",
    "orgUnit", country_uid) 
  
  # remove any 0 value or dedup records, these do not go into site tool
  datapackr::getDataValueSets(parameters$key, parameters$value) %>%
    dplyr::filter(value != 0) %>%  # don't need 0s
    dplyr::filter(attribute_option_combo != '00000') %>% # dedup
    dplyr::filter(attribute_option_combo != '00001') %>% # dedup
    dplyr::select(-stored_by, - last_updated, -comment, -followup, -deleted) # extra columns
}


# Main Data Processing Routine --------------------------------
datapackr::loginToDATIM(secrets)

# get a table to map the DATIM columns data site tool columns 
# handling the special cases of HTS_SELF.N.HIVSelfTest.20T.Unassisted and 
# TB_ART.N
 site_to_datim <- dplyr::filter(datapackr::SiteToDATIM,
                       indicator_code != "HTS_SELF.N.HIVSelfTest.20T.Unassisted" |
                         (indicator_code == "HTS_SELF.N.HIVSelfTest.20T.Unassisted" &
                            valid_ages == "50+" & valid_sexes == "Male")
                       ) %>% 
   dplyr::filter(indicator_code != "TB_ART.N.Age/Sex/NewExistingART/HIVStatus.20T.Already")
   

sites  <-  datapackr::getSiteList(d$info$datapack_uid)

# Get the data
data <- getCOP19DataFromAPI(d$info$datapack_uid)

#join with site to datim table to back out columns for site tool 
data  <-  dplyr::left_join(data, site_to_datim, 
                 by = c("data_element" = "dataelementuid",
                        "category_option_combo" = "categoryoptioncombouid"))

# join with sites table to get PSNU details

data  <-  dplyr::left_join(data, sites, 
                                by = c("org_unit" = "id"))

# store these for investigation - Why is there FY20 target data in DATIM 
# that we cannot map to the site tool? Usually this is because the data collection form included
# entry points that were not on the data pack and someone manually eneterd the target

incomplete_records = dplyr::filter(data, is.na(indicator_code) | is.na(psnu))

assertthat::assert_that(NROW(incomplete_records) == 0)

data = dplyr::filter(data, !is.na(indicator_code) & !is.na(psnu)) %>% 
  dplyr::select(PSNU = psnu,
                psnuid = psnu_uid,
                sheet_name   = sheet_name,
                indicatorCode = indicator_code,
                Age          = valid_ages,
                Sex          = valid_sexes,
                KeyPop       = valid_kps,
                mechanismCode= attribute_option_combo,
                value, #(if we pulled directly from a data pack this would be different than siteValue, but for OPUs should equal siteValue) 
                org_unit     = org_unit,
                type         = type_options #{DSD, TA} -   (NA not valid in our case)
                ) %>% 
  dplyr::mutate(PSNU = glue::glue("{PSNU} ({psnuid})"), siteValue = value)


d$data$site$distributed <- data
d$data$distributedMER <- data
datapackr::packSiteTool(d,
                        output_path = output_path )