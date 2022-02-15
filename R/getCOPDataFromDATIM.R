#' @export
#' @title Grab all raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @description
#' Grab all raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @param country_uids country_uids
#' @param cop_year Specifies COP year for dating as well as selection of
#' templates.
#' @param streams data stream or streams. One or more of "mer_targets", "subnat_targets", "impatt".
#' If not specified, then all data streams are returned.
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#'
#' @return Raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @examples
#' \dontrun{getCOPDataFromDATIM(country_uid = d$info$country_uids, cop_year = d$info$cop_year)}
#'
getCOPDataFromDATIM <- function(country_uids,
                                cop_year,
                                streams = c("mer_targets", "subnat_targets", "impatt"),
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  if (!cop_year %in% c(2020,
                       2021,
                       2022)) {
    stop("The COP year provided is not supported by the internal function getCOPDataFromDATIM")
    ### NOTE for COP23 some special handling of SUBNAT data for FY23 like the code below may be
    ### required if the 50+ finer age categories needs to be imported during COP23
  }

  dataset_uids <- datapackr::getDatasetUids(cop_year + 1, streams)

  # hack to allow forward compatibility between FY21 subnat dataset in DATIM and
  # COP21/FY22 datapack
  # need to be able to grab dataelements from FY22 subnat targets dataset for FY21 period
  if (cop_year == 2020 && "subnat_targets" %in% streams) {
    dataset_uids <-  c(dataset_uids, datapackr::getDatasetUids(2022, "subnat_targets"))
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
