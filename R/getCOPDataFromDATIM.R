#' @export
#' @title Grab all raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @description
#' Grab all raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @param country_uid country_uid
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param d2_session DHIS2 Session id
#'
#' @return Raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @examples getCOPDataFromDATIM(country_uid = d$info$country_uids, cop_year = d$info$cop_year)
#'
getCOPDataFromDATIM <- function(country_uids,
                                cop_year,
                                streams = c("mer_targets", "subnat_targets", "impatt"),
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  if(!cop_year %in% c(2020, 2021)){
    stop("The COP year provided is not supported by the internal function getCOPDataFromDATIM")
  }
  
  dataset_uids <- datapackr::getDatasetUids(cop_year + 1, streams)
  
  # hack to allow forward compatibility between FY21 subnat dataset in DATIM and 
  # COP21/FY22 datapack
  # need to be able to grab dataelements from FY22 subnat targets dataset for FY21 period
  if (cop_year == 2020 && "subnat_targets" %in% streams){
    dataset_uids <-  c(dataset_uids, datapackr::getDatasetUids(2022, "subnat_targets") )
  }
    
  # package parameters for getDataValueSets function call
  parameters <-
    dplyr::bind_rows(
      tibble::tibble(key = "dataSet", value = dataset_uids),
      tibble::tibble(key = "orgUnit", value = country_uids),
      tibble::tribble(~ key, ~ value,
                      "children", "true",
                      "categoryOptionComboIdScheme", "code",
                      "includeDeleted", "false",
                      "period", paste0(cop_year, "Oct")
      )
    )
  
  # get data from datim using dataValueSets
  # rename to standard names
  datim_data <-
    getDataValueSets(parameters$key,
                     parameters$value,
                     d2_session = d2_session) %>%
    dplyr::rename(
      dataElement = data_element,
      orgUnit = org_unit,
      categoryOptionCombo = category_option_combo,
      attributeOptionCombo = attribute_option_combo
    )
  
  return(datim_data)
}
