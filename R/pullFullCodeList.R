#' @export
#' @title Pull & combine all MER, SUBNAT, IMPATT code lists for specified FY.
#' 
#' @description
#' Pulls all code lists for MER, SUBNAT, and IMPATT for a specified FY and
#' combines these into a unique list.
#' 
#' @param FY Reporting FY for which to filter active code lists.
#' @param datastream Specify MER, SUBNAT, or IMPATT, or omit to specify all.
#' 
#' @return Combined code list as dataframe.
#'
pullFullCodeList <- function(FY = getCurrentCOPYear() + 1, 
                             datastream = c("mer_targets", "mer_results",
                                            "subnat_targets", "subnat_results",
                                            "impatt"),
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE)
                             ) {
  
  datasets_list <- character(0)
  
  if ("mer_targets" %in% datastream) {
    datasets_list <- c(datasets_list,
                       datapackr::getDatasetUids(FY, type = "mer_targets"))
  }
  if ("mer_results" %in% datastream) {
    datasets_list <- c(datasets_list,
                       datapackr::getDatasetUids(FY, type = "mer_results"))
  }
  if ("subnat_targets" %in% datastream) {
    datasets_list <- c(datasets_list,
                       datapackr::getDatasetUids(FY, type = "subnat_targets"))
  }
  if ("subnat_results" %in% datastream) {
    datasets_list <- c(datasets_list,
                       datapackr::getDatasetUids(FY, type = "subnat_results"))
  }
  if ("impatt" %in% datastream) {
    datasets_list <- c(datasets_list,
                       datapackr::getDatasetUids(FY, type = "impatt"))
  }
  
  datasets_list <- unique(datasets_list)
  
  ds <- data.frame()
  
  fullCodeList <-
    lapply(
      datasets_list,
      function(x){
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
