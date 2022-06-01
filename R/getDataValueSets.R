#' @export
#' @title getDataValueSets
#'
#' @description Used to read DHIS 2 data using the data value set endpoint
#' @param keys character vector - data value set parameter keys (e.g. "dataSet", "period")
#' @param values character vector - values marching the key from keys (e.g. "Abcde123456", "2019Q1")
#' @inheritParams datapackr_params
#' @return  tibble with the data requested
#'
getDataValueSets <- function(keys, values,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {

  # concatenate and format the keys and values provided for the api call
  parameters <- stringr::str_c(keys,
                               values,
                               sep = "=",
                               collapse = "&")

  data <- datimutils::getMetadata("dataValueSets",
                                  parameters,
                                  d2_session = d2_session) %>%
    dplyr::rename(data_element = dataelement,
                  org_unit = orgunit,
                  category_option_combo = categoryoptioncombo,
                  attribute_option_combo = attributeoptioncombo,
                  stored_by = storedby,
                  last_updated = lastupdated)

  data
}
