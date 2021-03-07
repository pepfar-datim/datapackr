# this code relates to https://github.com/pepfar-datim/COP-Target-Setting/issues/700
# it is not being exported pending https://github.com/pepfar-datim/COP-Target-Setting/issues/734
# as it works for COP20 leaving it in place for now

# #' @export
# #' @title Pull & combine all  UIDS for specified data_stream types and FY.
# #'
# #' @description
# #' Pulls all uids for types argument for a specified FY.
# #'
# #' @param cop_year Specifies COP year. Remember, COP20 = FY21 targets.
# #' @param types Specify MER, SUBNAT, or IMPATT, or omit to specify all.
# #'
# #' @return a character vector with the requested uids respecting the selection in the type parameter and FY parameter.
# #'
# #' @example getCOPDatasetUIDs(cop_year = 2020, types = c("MER", "SUBNAT", "IMPATT"))
# #' 
# getCOPDatasetUIDs <- function(cop_year = getCurrentCOPYear(),
#                               types = c("MER", "SUBNAT", "IMPATT"),
#                               d2_session = dynGet("d2_default_session",
#                                                   inherits = TRUE)) {
#   if(cop_year != 2020){
#     stop("The COP year provided is not supported by the internal function getCOPDatasetUids")
#   }
#   
#   data <- api_get(api_call("dataSets",
#                            d2_session = d2_session),
#                   d2_session = d2_session)
#   data <- data[grepl("^MER Targets: (Community|Facility)|MER Target Setting: PSNU|^(Host Country Targets|Planning Attributes): COP Prioritization SNU",
#                      data$displayName),]
#   
#   FY = cop_year + 1
#   
#   # if(FY != currentFY()+1) #This only works if we assume that DATIM datasets all update and deploy by Oct 1
#   if(FY != getCurrentCOPYear()+1) {
#     data <- data[grepl(paste0("FY",FY), data$displayName),]
#   } else {
#     data <- data[!(grepl("FY[0-9]{4}", data$displayName)),]
#   }
#   
#   data$fiscal_year <- ifelse(!stringr::str_detect(data$displayName, "FY"), currentFY()+1,
#                              as.numeric(stringr::str_extract(data$displayName,"(?<=FY)\\d{4}$")))
#   data$data_stream <- ifelse(stringr::str_detect(data$displayName, "^MER "), "MER",
#                              ifelse(stringr::str_detect(data$displayName, "^Host Country Targets"),
#                                     "SUBNAT","IMPATT"))
#   if(!(all((types %in% data$data_stream))))
#   {
#     stop(paste0("UID Not Found for ", setdiff(types, data$data_stream), " for FY ", FY))
#   }
#   
#   #  print(paste0("returning uids for " , FY))
#   return(data$id[data$data_stream %in% types])
#   
# }


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
#' @example getCOPDataFromDATIM(country_uid = d$info$country_uids,
#'                              cop_year = d$info$cop_year)
#'
getCOPDataFromDATIM <- function(country_uids,
                                cop_year,
                                streams = c("mer_targets", "subnat_targets", "impatt"),
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  if(cop_year != 2020){
    stop("The COP year provided is not supported by the internal function getCOPDataFromDATIM")
  }
  
  dataset_uids <- datapackr::getDatasetUids(cop_year + 1, streams)
    
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
      datim_value = value,
      data_element_uid = data_element,
      org_unit_uid = org_unit,
      category_option_combo_uid = category_option_combo,
      attribute_option_combo_code = attribute_option_combo
    )
  
  return(datim_data)
}
