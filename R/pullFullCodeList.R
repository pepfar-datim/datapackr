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
pullFullCodeList <- function(FY = currentFY(), 
                             datastream = c("MER", "SUBNAT", "IMPATT")) {
  
  if ("MER" %in% datastream) {
    MER <- datapackr::getDatasetUids(FY, type = "targets")
    }
  if ("SUBNAT" %in% datastream) {
    SUBNAT <- datapackr::getDatasetUids(FY, type = "subnat")
  }
  if ("IMPATT" %in% datastream) {
    IMPATT <- datapackr::getDatasetUids(FY, type = "impatt")
  }
  
  datasets_list <- c(MER, SUBNAT, IMPATT)
  
  ds <- data.frame()
  
  fullCodeList <-
    lapply(
      datasets_list,
      function(x){
        cl <- pullDATIMCodeList(x)
        ds <- rbind(ds, cl)
        }) %>%
    do.call(rbind, .) %>%
    dplyr::select(dataelement, dataelementuid, categoryoptioncombo, categoryoptioncombouid) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dataelement, categoryoptioncombo)
  
  return(fullCodeList)
  
}
