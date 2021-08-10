## TO DO only assing military data to military PSNU
library(magrittr)
library(datimutils)
datapackr::loginToDATIM("/users/sam/.secrets/cop.json")
base_url <- getOption("baseurl")

country_uid <- datimutils::getOrgUnits("Rwanda", name)



global_data <- datapackr::getCOPDataFromDATIM("ybg3MO3hcf4",
                                               2020,
                                               streams = "mer_targets") 
country_data <-  datapackr::getCOPDataFromDATIM(country_uid, 
                                                2020,
                                                streams = "mer_targets") 

# global_orgUnit_aoc_st_sets <- dplyr::select(global_data,
#                                          support_type,
#                                          orgUnit_uid,
#                                          attribute_option) %>%
#   dplyr::distinct()

global_orgUnits <- dplyr::select(global_data,
                                       orgUnit) %>% 
  dplyr::distinct()

global_aocs <- dplyr::select(global_data,
                             attributeOptionCombo) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(attributeOptionCombo != "00000",
                attributeOptionCombo != "00001")

country_aocs <- dplyr::select(country_data,
                              attributeOptionCombo) %>%
  dplyr::distinct() %>% 
  dplyr::filter(attributeOptionCombo != "00000",
                attributeOptionCombo != "00001")

country_orgUnits <- dplyr::select(country_data,
                                  orgUnit) %>%
  dplyr::distinct()

# 
# country_orgUnit_aoc_pairs <- dplyr::full_join(country_aocs,
#                                            country_orgUnits,
#                                            by = character())

max = NROW(country_orgUnits)

n = NROW(global_orgUnits)

mix <- runif(n, max = max) %>% ceiling()


country_orgUnits <- dplyr::mutate(country_orgUnits,
                                        index = 1:max)
global_orgUnits <- dplyr::mutate(global_orgUnits,
                                       mix = mix) %>% 
  dplyr::inner_join(country_orgUnits, by = c("mix" = "index"))


max = NROW(country_aocs)

n = NROW(global_aocs)

mix <- runif(n, max = max) %>% ceiling()


country_aocs <- dplyr::mutate(country_aocs,
                               index = 1:max)
global_aocs <- dplyr::mutate(global_aocs,
                              mix = mix) %>% 
  dplyr::inner_join(country_aocs, by = c("mix" = "index")) %>% 
  rbind(c("00000")) %>% 
  rbind(c("00001"))

dreams_agyw_data_elements <- datimutils::getMetadata(dataElements, 
                                                     name %.like% "DREAMS",
                                                     name %.like% "AGYW",
                                                     fields = "id")
dreams_ovc_data_elements <- datimutils::getMetadata(dataElements, 
                                                    name %.like% "DREAMS",
                                                    name %.like% "OVC",
                                                    fields = "id")

dreams_snus <- datimutils::getOrgUnitGroups("DREAMS SNUs", 
                                            by = name,
                                            fields = "organisationUnits[id, path]")  

org_units_in_dreams_path <- dreams_snus$path %>% 
  stringr::str_split("/") %>% 
  unlist() %>% 
  unique()

dreams_psnus <- datimutils::getOrgUnitGroups("COP Prioritization SNU", 
                                            by = name,
                                            fields = "organisationUnits[id]") %>%
  .$id %>% 
  intersect(org_units_in_dreams_path)

random_data <- dplyr::inner_join(global_data,
                                 global_orgUnits,
                                 by = c("orgUnit" = "orgUnit.x")) %>%
  dplyr::inner_join(global_aocs,
                    by = c("attributeOptionCombo" = "attributeOptionCombo.x")) %>%
  dplyr::mutate(orgUnit = orgUnit.y,
                attributeOptionCombo = attributeOptionCombo.y) %>% 
  dplyr::select(dataElement,
                period,
                orgUnit,
                categoryOptionCombo,
                attributeOptionCombo,
                value) %>% 
  dplyr::group_by_at(dplyr::vars(-value)) %>% 
  dplyr::summarise(value = sum(value)) %>% 
  dplyr::ungroup()  %>% 
  dplyr::filter(!(dataElement %in% dreams_agyw_data_elements) |
                  (dataElement %in% dreams_agyw_data_elements &
                  orgUnit %in% dreams_snus$id)) %>% 
  dplyr::filter(!(dataElement %in% dreams_ovc_data_elements) |
                  (dataElement %in% dreams_ovc_data_elements &
                     orgUnit %in% dreams_psnus))


no_dedupes <- dplyr::group_by_at(random_data,
                   dplyr::vars(-value,-attributeOptionCombo)) %>% 
  dplyr::summarise(attributeOptionCombo=paste0(attributeOptionCombo,collapse = ","),
                   value=sum(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!(stringr::str_detect(attributeOptionCombo, "00000") |
                    stringr::str_detect(attributeOptionCombo, "00001")
                  )
                )

extra_dedupes <- dplyr::bind_rows(
  dplyr::mutate(no_dedupes, attributeOptionCombo = "00000", value = 0),
  dplyr::mutate(no_dedupes, attributeOptionCombo = "00001", value = 0)
)

random_data <- dplyr::bind_rows(random_data,
                                extra_dedupes) %>% 
  dplyr::mutate(value = as.character(value))

dedupes_00000 <- dplyr::filter(random_data,
                               attributeOptionCombo == "00000")

dedupes_00001 <- dplyr::filter(random_data,
                               attributeOptionCombo == "00001")

deletes <- country_data

random_data <- dplyr::filter(random_data,
                             !(attributeOptionCombo %in% c("00000", "00001")))

prep_json <- function(data){
  
  pl <- jsonlite::toJSON(list(dataValues = data), auto_unbox = TRUE)
  
  # import file handling to get ready for api call
  output_file <- tempfile()
  writeLines(pl, output_file)
  zip_file <- tempfile()
  zip(zipfile = zip_file, files = output_file)
  read_file <- file(paste0(zip_file, ".zip"), "rb")
  raw_file <-
    readBin(read_file, "raw", n = file.size(paste0(zip_file, ".zip")))
  close(read_file)
  return(list(pl = pl, raw_file = raw_file))
}

# create data structure to import data one PSNU at a time
# cop-test doesn't seem to work well with large imports
random_data_nested <- dplyr::mutate(random_data, group = orgUnit) %>%  
  dplyr::group_by(group) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = purrr::map(data, prep_json)) %>%
  dplyr::ungroup()
  

dedupes_00000_json <- prep_json(dedupes_00000)
dedupes_00001_json <- prep_json(dedupes_00001)
deletes_json = prep_json(deletes)

url <- paste0("https://cop-test.datim.org/", 
              "api/dataValueSets?importStrategy=DELETE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")
r <- httr::POST(url, body = deletes_json[["raw_file"]],
                httr::content_type_json(),
                handle = d2_default_session$handle,
                httr::timeout(600))

# prin import summary
httr::content(r)

url <- paste0("https://cop-test.datim.org/", 
              "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")

r <- purrr::map(random_data_nested[["data"]], ~ {httr::POST(url, body = .x[["raw_file"]],
                                                      httr::content_type_json(),
                                                      handle = d2_default_session$handle)})
# r <- httr::POST(url, body = random_data_json[["raw_file"]],
#                 httr::content_type_json(),
#                 handle = d2_default_session$handle)
# prin import summary
purrr::map(r, httr::content)


url <- paste0("https://cop-test.datim.org/", 
              "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")
r <- httr::POST(url, body = dedupes_00000_json[["raw_file"]],
                httr::content_type_json(),
                handle = d2_default_session$handle,
                httr::timeout(600))

# prin import summary
httr::content(r)

url <- paste0("https://cop-test.datim.org/", 
              "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")
r <- httr::POST(url, body = dedupes_00001_json[["raw_file"]],
                httr::content_type_json(),
                handle = d2_default_session$handle,
                httr::timeout(600))
