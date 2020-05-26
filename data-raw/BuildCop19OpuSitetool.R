country_name <-  "Laos" ### if it is a region, escalate to sam
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

d$info$datapack_name = country_details$data_pack_name
d$info$datapack_uid = country_details$country_uid


# Set up a site -> PSNU cache
# sitePsnuCache <- new.env()

#' Get the name of an OU from its UID
#' 
#'  @param uid A valid OU UID
#'  @return The OU's name 
# getOUNameFromUID<-function(uid) {
#   paste0(getOption("baseurl"),"api/29/organisationUnits/",uid,"?fields=name") %>% 
#     httr::GET() %>% 
#     httr::content(as = "text") %>% 
#     jsonlite::fromJSON(.) %>% 
#     purrr::pluck("name")
# }

#' Get the OU path string
#' 
#' @param uid A valid OU UID
#' @return '/' delimited list of parent OUs
# getOUPathFromUID<-function(uid) {
#     paste0(getOption("baseurl"),"api/29/organisationUnits/",uid,"?fields=path") %>% 
#         httr::GET() %>% 
#         httr::content(as = "text") %>% 
#         jsonlite::fromJSON(.) %>% 
#         purrr::pluck("path")
# }

#' Get the PSNU record set for a given site
#' 
#' @param siteUID OU UID
#' @param psnus Existing list of PSNUs for this country
#' @return row record or FALSE
# getPsnuForSite<-function(siteUID,psnus) {
#     # see if it is cached already
#     if (exists(siteUID, envir = sitePsnuCache)) {
#         return(sitePsnuCache[[siteUID]])
#     } else {
#         # pull it, shove it
#         sitePath <- getOUPathFromUID(siteUID)
#         if (length(sitePath) == 0) {
#             # @TODO:: return exception
#             return(FALSE)
#         }
#         sitePathList <- unlist(strsplit(sitePath,"/"))
#         # compare this site to the PSNU list for the country
#         for (s in sitePathList) {
#             # skip this site and any leading blanks
#             if (s == "" || s == siteUID) {
#                 next
#             }
#             candidatePSNU <- psnus %>% filter(psnu_uid == s)
#             if (length(candidatePSNU$psnu) == 0) {
#                 next
#             }
#             sitePsnuCache[[siteUID]] <- candidatePSNU
#             return(sitePsnuCache[[siteUID]])
#         }
#     }
#     return(FALSE)
# }

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
   

# Cache the PSNUs for this country
# psnus = datapackr::getPSNUs(parent_uid, TRUE)
sites  <-  datapackr::getSiteList(d$info$datapack_uid)
# Store the country OU name
# parent_name <- getOUNameFromUID(parent_uid)
# Get the main dataset for looping through
dataSets <- getCOP19DataFromAPI(d$info$datapack_uid)

#join with site to datim table to back out columns for site tool 
dataSets  <-  dplyr::left_join(dataSets, site_to_datim, 
                 by = c("data_element" = "dataelementuid",
                        "category_option_combo" = "categoryoptioncombouid"))

# join with sites table to get PSNU details

dataSets  <-  dplyr::left_join(dataSets, sites, 
                                by = c("org_unit" = "id"))

# store these for investigation - Why is there FY20 target data in DATIM 
# that we cannot map to the site tool? Usually this is because the data collection form included
# entry points that were not on the data pack and someone manually eneterd the target

incomplete_records = dplyr::filter(dataSets, is.na(indicator_code) | is.na(psnu))

dataSets = dplyr::filter(dataSets, !is.na(indicator_code) & !is.na(psnu)) %>% 
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


d$data$site$distributed <- dataSets
d$data$distributedMER <- dataSets
datapackr::packSiteTool(d,
                        output_path = output_path )
