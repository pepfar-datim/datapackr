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
pullFullCodeList <- function(FY = NULL, datastream = NULL) {
  
  if (is.null(FY)) {FY = currentFY()}
  
  if (is.null(datastream)) {datastream = c("MER", "SUBNAT", "IMPATT")}
  
  datasets <- api_call("dataSets") %>%
    api_get() %>%
    dplyr::filter(
      stringr::str_detect(
        displayName,
        "^MER Targets: (Community|Facility)|MER Target Setting: PSNU|^(Host Country Targets|Planning Attributes): COP Prioritization SNU"
        )
      ) %>%
    dplyr::mutate(
      fiscal_year = dplyr::case_when(
        !stringr::str_detect(displayName, "FY") ~ currentFY()+1,
        TRUE ~ as.numeric(stringr::str_extract(displayName,"(?<=FY)\\d{4}$"))
      ),
      data_stream = dplyr::case_when(
        stringr::str_detect(displayName, "^MER ") ~ "MER",
        stringr::str_detect(displayName, "^Host Country Targets") ~ "SUBNAT",
        stringr::str_detect(displayName, "^Planning Attributes") ~ "IMPATT",
      )
    )
  
  if ("MER" %in% datastream) {
    MER <- datasets %>%
      dplyr::filter(fiscal_year == FY & data_stream == "MER") %>%
      dplyr::pull(id)
  }
  if ("SUBNAT" %in% datastream) {
    SUBNAT <- datasets %>%
      dplyr::filter(fiscal_year == FY & data_stream == "SUBNAT") %>%
      dplyr::pull(id)
    
    if (length(SUBNAT) == 0) {
      SUBNAT <- datasets %>%
        dplyr::filter(fiscal_year < FY & data_stream == "SUBNAT") %>%
        dplyr::slice(which.max(fiscal_year)) %>%
        dplyr::pull(id)
    }
  }
  if ("IMPATT" %in% datastream) {
    IMPATT <- datasets %>%
      dplyr::filter(fiscal_year == FY & data_stream == "IMPATT") %>%
      dplyr::pull(id)
    
    if (length(IMPATT) == 0) {
      IMPATT <- datasets %>%
        dplyr::filter(fiscal_year < FY & data_stream == "IMPATT") %>%
        dplyr::slice(which.max(fiscal_year)) %>%
        dplyr::pull(id)
    }
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
