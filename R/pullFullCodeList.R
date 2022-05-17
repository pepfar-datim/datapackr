#' @export
#' @title Pull & combine all MER, SUBNAT, IMPATT code lists for specified FY.
#'
#' @description
#' Pulls all code lists for MER, SUBNAT, and IMPATT for a specified FY and
#' combines these into a unique list.
#'
#' @param FY Reporting FY for which to filter active code lists.
#' @param datasets Character vector of dataSet IDs to pull code lists for.
#' @param expanded If TRUE, will add dataset, period, period_dataset, and
#' targets_results, related categoryOption metadata,  as additional columns
#' @inheritParams datapackr_params
#' @return Combined code list as dataframe.
#'
pullFullCodeList <- function(FY = getCurrentCOPYear() + 1,
                             datastreams = c("mer_targets", "mer_results",
                                             "subnat_targets", "subnat_results",
                                             "impatt"),
                             datasets,
                             expanded = FALSE,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {

  datasets <- datasets %missing% NULL
  datasets_provided <- !is.null(datasets)

  if (!datasets_provided) {
    
    datasets <- sapply(datastreams, function(x) getDatasetUids(FY,x)) %>% 
      unlist() 
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
    dplyr::select(dataelement, dataelementuid, categoryoptioncombo, categoryoptioncombouid) %>%
    dplyr::mutate(FY = FY) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dataelement, categoryoptioncombo)

  return(fullCodeList)

}
