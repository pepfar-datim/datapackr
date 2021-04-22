#' @export
#' @title Pull & combine all MER, SUBNAT, IMPATT code lists for specified FY.
#'
#' @description
#' Pulls all code lists for MER, SUBNAT, and IMPATT for a specified FY and
#' combines these into a unique list.
#'
#' @param FY Reporting FY for which to filter active code lists.
#' @param datastream At least one
#' of "mer_targets", "mer_results", "subnat_targets", "subnat_results" or "impatt".
#' If left will return a code list for the specified COP Year of all streams
#' @param datasets Character vector of dataSet IDs to pull code lists for.
#' @param expanded If TRUE, will add dataset, period, period_dataset, and
#' targets_results, related categoryOption metadata,  as additional columns
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @return Combined code list as dataframe.
#'
pullFullCodeList <- function(FY = getCurrentCOPYear() + 1,
                             datastream = c("mer_targets", "mer_results",
                                            "subnat_targets", "subnat_results",
                                            "impatt"),
                             datasets,
                             expanded = FALSE,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {

  datasets <- datasets %missing% NULL
  datasets_provided <- !is.null(datasets)

  if (!datasets_provided) {
    datasets <- character(0)

    if ("mer_targets" %in% datastream) {
      datasets <- c(datasets,
                    datapackr::getDatasetUids(FY, type = "mer_targets"))
    }
    if ("mer_results" %in% datastream) {
      datasets <- c(datasets,
                    datapackr::getDatasetUids(FY, type = "mer_results"))
    }
    if ("subnat_targets" %in% datastream) {
      datasets <- c(datasets,
                    datapackr::getDatasetUids(FY, type = "subnat_targets"))
    }
    if ("subnat_results" %in% datastream) {
      datasets <- c(datasets,
                    datapackr::getDatasetUids(FY, type = "subnat_results"))
    }
    if ("impatt" %in% datastream) {
      datasets <- c(datasets,
                    datapackr::getDatasetUids(FY, type = "impatt"))
    }

  }

  datasets <- unique(datasets)

  ds <- data.frame()

  fullCodeList <-
    lapply(
      datasets,
      function(x) {
        cl <- pullDATIMCodeList(x, d2_session = d2_session)
        ds <- rbind(ds, cl)
        }) %>%
    do.call(rbind, .) %>%
    dplyr::mutate(FY = FY) %>%
    dplyr::select(FY, dplyr::everything()) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dataelement, categoryoptioncombo)

  return(fullCodeList)

}
