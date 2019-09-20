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
pullFullCodeList <- function(FY = NA, datastream = NA) {
  
  if (is.na(FY)) {FY = currentFY()}
  
  if (is.na(datastream)) {datastream = c("MER", "SUBNAT", "IMPATT")}
  
  datasets_list <- api_call("dataSets") %>%
    api_get() %>%
    dplyr::filter(
      stringr::str_detect(
        displayName,
        "^MER Targets: (Community|Facility)|^(Host Country Targets|Planning Attributes): COP Prioritization SNU"
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
    ) %>%
    dplyr::filter(fiscal_year == FY,
                  data_stream %in% datastream) %>%
    dplyr::pull(id)
  
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
