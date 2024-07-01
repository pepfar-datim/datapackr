#' @export
#' @title Grab all raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @description
#' Grab all raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @inheritParams datapackr_params
#'
#' @return Raw data in DATIM for a country for the COP data sets for a given COP Year.
#'
#' @examples
#' \dontrun{getCOPDataFromDATIM(country_uid = d$info$country_uids, cop_year = d$info$cop_year)}
#'
getCOPDataFromDATIM <- function(country_uids,
                                cop_year,
                                datastreams = c("mer_targets", "subnat_targets", "impatt"),
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {


  if (!cop_year %in% c(2023:2024)) {

    stop("The COP year provided is not supported by the internal function getCOPDataFromDATIM")
    ### NOTE for COP23 some special handling of SUBNAT data for FY23 like the code below may be
    ### required if the 50+ finer age categories needs to be imported during COP23
  }

  dataset_uids <- getCOPDatasetUids(cop_year, datastreams)

  # hack to allow forward compatibility between FY21 subnat dataset in DATIM and
  # COP21/FY22 datapack
  # need to be able to grab dataelements from FY22 subnat targets dataset for FY21 period

  # Mon May 13 19:44:45 2024 ----- Will delete after this proves not to break anything
  # if (cop_year == 2020 && "subnat_targets" %in% datastreams) {
  #   dataset_uids <-  c(dataset_uids, getCOPDatasetUids(2021, "subnat_targets"))
  # }


  # package parameters for getDataValueSets function call
  parameters <-
    dplyr::bind_rows(
      tibble::tibble(key = "dataSet", value = dataset_uids),
      tibble::tibble(key = "orgUnit", value = country_uids),
      tibble::tribble(~ key, ~ value,
                      "children", "true",
                      "categoryOptionComboIdScheme", "code",
                      "attributeOptionComboIdScheme", "code",
                      "includeDeleted", "false",
                      "period", paste0(cop_year, "Oct")
      )
    )

  datim_data <-
    tryCatch({
      datimutils::getDataValueSets(parameters$key,
                     parameters$value,
                     d2_session = d2_session)},
             error = function(cond) {
               message(cond)
               warning("Could not retreive COP data from DATIM")
               return(NULL)
             })

  if (is.null(datim_data) || NROW(datim_data) == 0) {
    return(NULL)
  } else {
    datim_data %>%
      #Maintain legacy behavior which
      #Cast values to doubles (even though this does not always have to be the case)
      dplyr::mutate(value = as.numeric(value),
                    created = strptime(created, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
                    lastUpdated = strptime(created, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }



}
