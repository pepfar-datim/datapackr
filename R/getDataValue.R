#' @export
#' @title GetDataValueSet
#' 
#' @description Used to read DHIS 2 data using the data value set endpoint
#' @param keys character vector - data value set parameter keys (e.g. "dataSet", "period")
#' @param values character vector - values marching the key from keys (e.g. "Abcde123456", "2019Q1"
#' @return  tibble with the data requested
#'
GetDataValueSet <- function(keys, values, 
                            base_url = getOption("baseurl"), 
                            api_version = "30"){
  
  #  api_call <- "https://triage.datim.org/api/30/dataValueSets.csv?dataSet=nIHNMxuPUORX&dataSet=sBv1dj90IX6&dataSet=C2G7IyPPrvD&dataSet=HiJieecLXxNX&period=2019Oct&orgUnit=XtxUYCsDWrR&children=true&categoryOptionComboIdScheme=code&includeDeleted=false"
  
  # concatenate and format the keys and values provided for the api call 
  parameters <- stringr::str_c(keys, 
                               values, 
                               sep = "=", 
                               collapse = "&")
  api_call <- glue::glue("{base_url}api/{api_version}/dataValueSets.csv?{parameters}")
  # note the deleted column comes back empty (missing a comma in fact) which results in a warning
  # we can disregard that warning
  #  row col   expected     actual         file
  #  1  -- 11 columns 10 columns literal data
  
  datapackcommons::RetryAPI(api_call, "application/csv") %>%   
    httr::content(., "text") %>% 
    {suppressWarnings(readr::read_csv(., 
                                      col_names = TRUE, 
                                      col_types = readr::cols(.default = "c", 
                                                              value = "d", 
                                                              lastupdated = "T")))} %>%
    dplyr::rename(data_element = dataelement, 
                  org_unit = orgunit,
                  category_option_combo = categoryoptioncombo,
                  attribute_option_combo = attributeoptioncombo,
                  stored_by = storedby,
                  last_updated = lastupdated)
}