#' @export
#' @importFrom magrittr %>% %<>%
#' @title Aggregates sheets based on sheet type in the Datapack.
#'
#' @description Aggregates sheets based on the sheet code. Currently handles
#' OVC, PMTCT_EID and KP.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'

aggregateSheet <- function(d, sheet) {
  # Aggregate OVC_HIVSTAT
  if (sheet == "OVC") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Age),
        Sex = dplyr::case_when(
          stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Sex)) %>%
      dplyr::group_by(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }
  
  # Add ages to PMTCT_EID
  if (sheet == "PMTCT_EID") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
          TRUE ~ Age
        )
      )
  }
  
  if (sheet == "KP") {
    d$data$extract %<>%
      dplyr::mutate(
        Sex = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T"
                               ~ stringr::str_replace(KeyPop, " PWID", ""),
                               TRUE ~ Sex),
        KeyPop = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T" ~ NA_character_,
                                  TRUE ~ KeyPop)
      )
  }
  return(d)
}

