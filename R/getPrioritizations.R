#' @export
#' @title Pull Prioritizations from DATIM
#'
#' @description Pull Prioritizations from DATIM for a given set of OUs and periods
#'
#' @param country_uids List of 11 digit alphanumeric DATIM codes representing
#' countries. If not provided, will check file for these codes. If not in file,
#' will flag error.
#' @param d2_session DHIS2 Session id
#' 
#' @return prioritizations
#'
getPrioritizations <- function(country_uids = NULL,
                               cop_year = NULL,
                               d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {
  
  if (is.null(country_uids)) {country_uids <- c("ybg3MO3hcf4")}
  country_list <- paste(country_uids, collapse = ";")
  
  period <- paste0(cop_year, "Oct")
  
  pzn_de.coc <- datapackr::DATIM_ds_pd_map %>%
    dplyr::filter(datastream == "IMPATT",
                  FY == (cop_year + 1)) %>%
    dplyr::pull(dataset.id) %>%
    datapackr::pullDATIMCodeList() %>%
    dplyr::mutate(FY = cop_year +1) %>%
    dplyr::filter(stringr::str_detect(dataelement, "PRIORITY_SNU")) %>%
    dplyr::select(dataelementuid, categoryoptioncombouid) %>%
    tidyr::unite(de.coc, dataelementuid, categoryoptioncombouid, sep = ".")
  
  prioritizations <- 
    datimutils::getAnalytics(dx = pzn_de.coc,
                             ou = paste0("OU_GROUP-AVy8gJXym2D;", country_list),
                             pe_f = period,
                             return_names = F)
  
  if (is.null(prioritizations)) {
    prioritizations <- tibble::tribble(~orgUnit, ~value, NA_character_, NA_real_) %>%
      tidyr::drop_na()
  } else {
    prioritizations %<>%
      dplyr::select(orgUnit = `Organisation unit`, value = Value)
  }

  return(prioritizations)  
}
