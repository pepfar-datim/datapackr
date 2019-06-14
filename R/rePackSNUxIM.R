#' @export
#' @title rePackSNUxIM(d)
#'
#' @description Takes the output of the \code{\link{unPackSNUxIM}} and
#'     \code{\link{unPackSheets}} functions and delicatety combines these to create
#'     a single dataframe at the PSNU x IM level.
#'
#' @param d Datapackr object
#' 
#' @return d
#' 
rePackPSNUxIM <- function(d) {
  d$data$distributedMER <- d$data$MER %>%
    dplyr::mutate(
      CoarseAge = dplyr::case_when(
        stringr::str_detect(indicator_code,"OVC_SERV")
          & Age %in% c("<01","01-04","05-09","10-14","15-17") ~ "<18",
        stringr::str_detect(indicator_code,"OVC_HIVSTAT") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        stringr::str_detect(indicator_code,"Malnutrition|Pediatric") ~ "01-04", #TODO: Fix in Data Pack, not here.
        stringr::str_detect(indicator_code,"HTS_SELF(.)+Unassisted") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        Age %in% c("<01","01-04","05-09","10-14") ~ "<15",
        Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+") ~ "15+",
        TRUE ~ Age),
      Sex = dplyr::case_when(
        stringr::str_detect(indicator_code,"OVC_HIVSTAT") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        stringr::str_detect(indicator_code,"KP_MAT") ~ stringr::str_replace(KeyPop," PWID",""), #TODO: Fix in Data Pack, not here.
        TRUE ~ Sex),
      KeyPop = dplyr::case_when(
        stringr::str_detect(indicator_code,"KP_MAT") ~ NA_character_, #TODO: Fix in Data Pack, not here.
        TRUE ~ KeyPop)
    ) %>%
    dplyr::left_join(dplyr::select(d$data$SNUxIM,-PSNU)) %>%
    dplyr::mutate(newValue = value * distribution) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, CoarseAge,
                  Sex, KeyPop, mechanismCode, value = newValue) %>%
    dplyr::filter(value != 0) %>%
    tidyr::drop_na(value)
  
  return(d)
  
}
