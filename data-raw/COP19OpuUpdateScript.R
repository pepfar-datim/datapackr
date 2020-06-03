devtools::install_github("https://github.com/pepfar-datim/datapackr",
                         "COP-19-Master")

country_name = "South Africa"

require(datapackr)
require(datimvalidation)
require(httr)
require(jsonlite)
require(magrittr)

datapackr::runApp_CompareSiteVsDatim()

compare_flat <- file.choose()
options("scipen"=999)

# converts a data frame into a json for import
# tibble [72,304 × 6] (S3: tbl_df/tbl/data.frame)
# $ data_element_uid           : chr [1:72304] "uid11111111" "uid11111111" ...
# $ period                     : chr [1:72304] "2019Oct" "2019Oct" ...
# $ org_unit_uid               : chr [1:72304] "uid22222222" "uid22222222" ...
# $ category_option_combo_uid  : chr [1:72304] "uid33333333" "uid33333333" ...
# $ attribute_option_combo_code: chr [1:72304] "99999" "99999" ...
# $ datim_value                : chr [1:72304] "1" "2" "3" "4" ...

prep_json <- function(data){
  
  data <- dplyr::rename(data,
                dataElement = "data_element_uid",
                period = "period",
                orgUnit = "org_unit_uid",
                categoryOptionCombo = "category_option_combo_uid",
                attributeOptionCombo = "attribute_option_combo_code",
                value = suppressWarnings(dplyr::one_of("datim_value",
                                                       "tool_value")))

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

deletes <- readxl::read_xlsx(compare_flat, "deletes", col_types = "text")
deletes_json <- prep_json(deletes)
updates <- readxl::read_xlsx(compare_flat, "updates", col_types = "text")
updates_json <- prep_json(updates)

datapackcommons::DHISLogin("~/.secrets/triage.json")
triage_base_url <- getOption("baseurl")


# send imports to DATIM
url <- paste0(triage_base_url, "api/dataValueSets?importStrategy=DELETE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")
r <- httr::POST(url, body = deletes_json[["raw_file"]], 
                content_type_json())

# prin import summary
httr::content(r)

url <- paste0(triage_base_url, "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")
r <- httr::POST(url, body = updates_json[["raw_file"]], 
                content_type_json())

# prin import summary
httr::content(r)

datapackcommons::DHISLogin("~/.secrets/datim.json")
prod_base_url <- getOption("baseurl")

# send imports to DATIM
url <- paste0(prod_base_url, "api/dataValueSets?importStrategy=DELETE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")
r <- httr::POST(url, body = deletes_json[["raw_file"]], 
                content_type_json())

# prin import summary
httr::content(r)

url <- paste0(prod_base_url, "api/dataValueSets?importStrategy=CREATE_AND_UPDATE&force=true&preheatCache=true&categoryOptionComboIdScheme=code")
r <- httr::POST(url, body = updates_json[["raw_file"]], 
                content_type_json())

# prin import summary
httr::content(r)


datapackr::runApp_CompareSiteVsDatim()


# store a copy of the import file, I attach it to the zendesk ticket for herecord
write(deletes_json[["pl"]], paste0("deletes_", 
                 country_name,
                 ".json"))

write(updates_json[["pl"]], paste0("updates_", 
                                   country_name,
                                   ".json"))
