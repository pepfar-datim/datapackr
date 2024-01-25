getPopData <- function(d) {

  d$data$SUBNAT_IMPATT

  datim_map <- getMapDataPack_DATIM_DEs_COCs(cop_year = d$info$cop_year,
                                             datasource = "Data Pack")
}








packForPAW <- function(d) {
  datim_data <- packForDATIM(d)
  pop_data <- getPopData(d)
  return(dplyr::rbind(datim_data, pop_data))
}
