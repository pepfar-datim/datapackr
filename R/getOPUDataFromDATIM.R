#' @export
#' @title Pull data for packing OPU Data Pack
#'
#' @description
#' Pulls data from DATIM API needed to pack OPU Data Pack.
#'
#' @inheritParams datapackr_params
#'
#' @return Returns dataset needed for use in \code{\link{packOPUDataPack}}.
#'
getOPUDataFromDATIM <- function(cop_year,
                                country_uids = NULL,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {

  stopifnot("ERROR! Must provide country_uids." = !is.null(country_uids))

  map_des_cocs_local <- datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year)

  options("scipen" = 999)
  options(warning.length = 8170)

  # Pull data from DATIM ####
  data_datim <- datapackr::getCOPDataFromDATIM(country_uids,
                                               cop_year,
                                               datastreams = "mer_targets",
                                               d2_session = d2_session)

  # Filter data by required indicator_codes ####
  indicator_codes <- datapackr::pick_schema(cop_year = cop_year,
                                            tool = "Data Pack") %>%
    dplyr::filter(dataset == "mer",
                  col_type == "target") %>%
    .[["indicator_code"]]

  # TODO: Remove the mutate function when the COC issue is fixed in DATIM
  data_datim %<>%
    dplyr::mutate(categoryOptionCombo =
                    ifelse(categoryOptionCombo == "default",
                           "HllvX50cXC0", categoryOptionCombo)) %>%
    dplyr::left_join(map_des_cocs_local,
                      by = c("dataElement" = "dataelementuid",
                            "categoryOptionCombo" = "categoryoptioncombouid",
                            "period" = "period"))

  if (any(is.na(data_datim$indicator_code))) {
    stop("Problem mapping target data pulled from DATIM to datapack schema")
  }

  data_datim %<>%
    dplyr::filter(indicator_code %in% indicator_codes)

  # COP21+: Output as DHIS2 import file ####
  data_datim %<>%
    dplyr::select(dataElement,
                  period,
                  orgUnit,
                  categoryOptionCombo,
                  attributeOptionCombo,
                  value)

  return(data_datim)

}
