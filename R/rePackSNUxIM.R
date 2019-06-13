#' @importFrom magrittr %>% %<>%
#' @title rePackSNUxIM(d)
#'
#' @description Takes the output of the \code{\link{unPackSNUxIM}} and
#'     \code{\link{unPackSheets}} functions and delicatety combines these to create
#'     a single dataframe at the PSNU x IM level.
#'
#' @param d datapackr list object containing at least \code{d$keychain$MER} and
#'     \code{d$data$SNUxIM}
#' @return A datapackr list object, \code{d}, storing at least 1 dataframe of
#'    data at the SNU x IM level, \code{d$data$distributedMER}.
rePackPSNUxIM <- function(d) {
  d$data$distributedMER <- d$data$MER %>%
    dplyr::mutate(
      CoarseAge = dplyr::case_when(
        stringr::str_detect(indicatorCode,"OVC_SERV")
        & Age %in% c("<01","01-04","05-09","10-14","15-17") ~ "<18",
        stringr::str_detect(indicatorCode,"OVC_HIVSTAT") ~ NA_character_,
        stringr::str_detect(indicatorCode,"Malnutrition|Pediatric") ~ "01-04",
        stringr::str_detect(indicatorCode,"HTS_SELF(.)+Unassisted") ~ NA_character_,
        Age %in% c("<01","01-04","05-09","10-14") ~ "<15",
        Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+") ~ "15+",
        TRUE ~ Age),
      Sex = dplyr::case_when(
        stringr::str_detect(indicatorCode,"OVC_HIVSTAT") ~ NA_character_,
        stringr::str_detect(indicatorCode,"KP_MAT") ~ stringr::str_replace(KeyPop," PWID",""),
        TRUE ~ Sex),
      KeyPop = dplyr::case_when(
        stringr::str_detect(indicatorCode,"KP_MAT") ~ NA_character_,
        TRUE ~ KeyPop)
    ) %>%
    dplyr::left_join(dplyr::select(d$data$SNUxIM,-PSNU)) %>%
    dplyr::mutate(newValue = value * distribution) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicatorCode, Age, CoarseAge,
                  Sex, KeyPop, mechanismCode, value = newValue) %>%
    dplyr::filter(value != 0) %>%
    tidyr::drop_na(value)
  
  return(d)
  
}
