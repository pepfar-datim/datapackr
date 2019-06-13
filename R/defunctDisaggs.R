#' @importFrom magrittr %>% %<>%
#' @title defunctDisaggs(d)
#'
#' @description Checks data extracted from a sheet in a submitted Data Pack or
#'    Site Tool to identify cases where invalid Disaggregate combinations have
#'    been used.
#'
#' @param d datapackr list object containing at least d$data$extract of data
#'     from a single sheet in submission file.
#' @param type Either "Data Pack" or "Site Tool".
#'     
#' @return A datapackr list object, \code{d}, storing a dataframe of defunct
#'    disaggs used in a specific sheet.
defunctDisaggs <- function(d,
                           type = "Data Pack") {
  defunct <- d$data$extract %>%
    replace(is.na(.), "") %>%
    dplyr::filter(
      (
        stringr::str_detect(indicatorCode, "Malnutrition|Pediatric") &
          Age != "01-04"
      )
      |
        (
          stringr::str_detect(indicatorCode, "HTS_RECENT|PrEP") &
            Age %in% c("01-04", "05-09", "10-14")
        )
      |
        (
          stringr::str_detect(
            indicatorCode,
            "HTS_TST_PMTCTPostANC1|PMTCT_STAT|PMTCT_ART"
          ) & (Age %in% c("<01", "01-04", "05-09") | Sex == "Male")
        )
      |
        (
          stringr::str_detect(indicatorCode, "HTS_SELF|PP_PREV") &
            Age %in% c("01-04", "05-09")
        )
      |
        (
          stringr::str_detect(
            indicatorCode,
            "(HTS_TST|KP_ESTIMATES|PrEP|TX_NEW)(.)+KeyPop"
          )
          &
            !KeyPop %in% c(
              "FSW",
              "MSM",
              "People in prisons and other enclosed settings",
              "PWID",
              "TG"
            )
        )
      |
        (
          stringr::str_detect(indicatorCode, "KP_MAT") &
            !KeyPop %in% c("Female PWID", "Male PWID")
        )
      | (
        stringr::str_detect(indicatorCode, "KP_PREV")
        &
          !KeyPop %in% c(
            "Female PWID",
            "Male PWID",
            "FSW",
            "MSM not SW",
            "MSM SW",
            "People in prisons and other enclosed settings",
            "TG not SW",
            "TG SW"
          )
      )
      |
        (
          stringr::str_detect(indicatorCode, "OVC_SERV") &
            !Age %in% c("<01", "01-04", "05-09", "10-14", "15-17", "18+")
        )
      |
        (
          stringr::str_detect(indicatorCode, "PRIORITY_SNU") &
            !value %in% (datapackr::prioritizations %>%
                           dplyr::pull(value))
        )
      |
        (
          stringr::str_detect(indicatorCode, "VMMC") &
            (Age %in% c("<01", "01-04", "05-09") | Sex == "Female")
        )
    )
  
  if(type == "Data Pack") {
    defunct <- d$data$extract %>%
      replace(is.na(.), "") %>%
      dplyr::filter(
        stringr::str_detect(indicatorCode, "OVC_HIVSTAT") &
          !Age %in% c("<01", "01-04", "05-09", "10-14", "15-17", "18+")) %>%
      dplyr::bind_rows(defunct)
  }
  
  if(type == "Site Tool") {
    defunct <- d$data$extract %>%
      replace(is.na(.), "") %>%
      dplyr::filter(
        stringr::str_detect(indicatorCode, "OVC_HIVSTAT") &
          !Age %in% c("")) %>%
      dplyr::bind_rows(defunct)
  }
  
  defunct %<>%
    dplyr::select(indicatorCode, Age, Sex, KeyPop) %>%
    dplyr::distinct()
  
  return(defunct)
}
