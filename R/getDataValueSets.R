#' @export
#' @title getDataValueSets
#'
#' @description Used to read DHIS 2 data using the data value set endpoint
#' @param keys character vector - data value set parameter keys (e.g. "dataSet", "period")
#' @param values character vector - values marching the key from keys (e.g. "Abcde123456", "2019Q1"
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return  tibble with the data requested
#'
getDataValueSets <- function(keys, values,
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {

  #TODO: Consider implementing a check of all paramaters
  #https://docs.dhis2.org/en/develop/using-the-api/dhis-core-version-master/data.html

  # concatenate and format the keys and values provided for the api call
  parameters <- stringr::str_c(keys,
                               values,
                               sep = "=",
                               collapse = "&")

    paste0(d2_session$base_url,
           "api/dataValueSets.json?",
           parameters,
           "&paging=false") %>%
    httr::GET(httr::timeout(600),
              handle = d2_session$handle) %>%
    httr::content(., "text") %>%
    jsonlite::fromJSON(.) %>%
    purrr::pluck("dataValues") %>%
    dplyr::rename(data_element = dataElement,
                  org_unit = orgUnit,
                  category_option_combo = categoryOptionCombo,
                  attribute_option_combo = attributeOptionCombo,
                  stored_by = storedBy,
                  last_updated = lastUpdated)

}
