#' @export
#' @title Pull & combine all MER, SUBNAT, IMPATT code lists for specified FY.
#' 
#' @description
#' Pulls all code lists for MER, SUBNAT, and IMPATT for a specified FY and
#' combines these into a unique list.
#' 
#' @param FY Reporting FY for which to filter active code lists.
#' @param data_stream Specify MER, SUBNAT, or IMPATT, or omit to specify all.
#' 
#' @return Combined code list as dataframe.
#'
pullFullCodeList <- function(FY = NA, datastream = NA) {
  current_year <- Sys.Date() %>%
    format("%Y") %>%
    as.numeric()
  
  current_month <- Sys.Date() %>%
    format("%m") %>%
    as.numeric()
  
  current_FY <- ifelse(current_month > 9, current_year + 1, current_year)
  
  if (is.na(FY)) {FY = current_FY}
  
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
        stringr::str_detect(displayName, "FY", negate = TRUE) ~ current_FY+1,
        TRUE ~ as.numeric(stringr::str_extract(displayName,"(?<=FY)\\d{4}$"))
      ),
      data_stream =dplyr::case_when(
        stringr::str_detect(displayName, "^MER ") ~ "MER",
        stringr::str_detect(displayName, "^Host Country Targets") ~ "SUBNAT",
        stringr::str_detect(displayName, "^Planning Attributes") ~ "IMPATT",
      )
    ) %>%
    dplyr::filter(fiscal_year == FY,
                  data_stream %in% datastream
    )
    
  datasets_list %<>% dplyr::pull(id)
  
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
