#' @export
#' @title getDataValueSets
#' 
#' @description Used to read DHIS 2 data using the data value set endpoint
#' @param keys character vector - data value set parameter keys (e.g. "dataSet", "period")
#' @param values character vector - values marching the key from keys (e.g. "Abcde123456", "2019Q1"
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param api_version - api version to use when calling DHIS2
#' @return  tibble with the data requested
#'
getDataValueSets <- function(keys, values,  
                            api_version = datapackr::api_version(),
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)){
  
  # concatenate and format the keys and values provided for the api call 
  parameters <- stringr::str_c(keys, 
                               values, 
                               sep = "=", 
                               collapse = "&")
  api_call <- glue::glue("{d2_session$base_url}api/{api_version}/dataValueSets.csv?{parameters}")
  # note the deleted column comes back empty (missing a comma in fact) which results in a warning
  # we can disregard that warning
  #  row col   expected     actual         file
  #  1  -- 11 columns 10 columns literal data
  
  data <- httr::GET(api_call,handle = d2_session$handle) %>%
    httr::content(.,"text") %>% 
    {suppressWarnings(readr::read_csv(.,col_names = TRUE, 
                    col_types = readr::cols(.default = "c", 
                                            value = "d", 
                                            lastupdated = "T")))} %>%
    dplyr::rename(data_element = dataelement, 
                  org_unit = orgunit,
                  category_option_combo = categoryoptioncombo,
                  attribute_option_combo = attributeoptioncombo,
                  stored_by = storedby,
                  last_updated = lastupdated)

  return(data)
}
