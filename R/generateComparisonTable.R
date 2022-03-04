#' Title
#'
#' @param d 
#' @description Generates a PSNUxIM level comparison between 
#' the values in the (OPU) DataPack and what is currently 
#' present in DATIM. 
#'
#' @return A comparison table
#' @export
#'

generateComparisonTable <- function(d) {
 
  
  if (is.null(d$memo$datapack$by_psnu)) {
    d_datapack <- data.frame("psnu_uid" = character(),
                             "datapack_value" = numeric())
  } else {
    d_datapack <- d$memo$datapack$by_psnu %>% 
      dplyr::rename("datapack_value" = value)
  }
  
  if (is.null(d$memo$datim$by_psnu)) {
    d_datim <-  data.frame("psnu_uid" = character(),
                           "datim_value" = numeric())
  } else {
    d_datim <-  d$memo$datim$by_psnu %>% 
      dplyr::rename("datim_value" = value)
  }
  
  if(NROW(d_datapack) > 0 | NROW(d_datim) > 0)
  {
    d$memo$comparison <- dplyr::full_join(d_datapack, d_datim) %>%
      dplyr::mutate(
        change_type = dplyr::case_when(
          datapack_value == datim_value ~ "No change",
          is.na(datapack_value) & !is.na(datim_value) ~ "Deletion",
          !is.na(datapack_value) & is.na(datim_value) ~ "New value",
          datapack_value != datim_value ~ "Update"
        )
      ) %>%
      dplyr::mutate(datim_value = ifelse(is.na(datim_value), 0, datim_value),
                    datapack_value = ifelse(is.na(datapack_value), 0, datapack_value)) %>%
      dplyr::mutate(identical = datapack_value == datim_value,
                    "Diff" = datapack_value - datim_value) %>%
      dplyr::filter(!identical) %>%
      tidyr::pivot_longer(cols = c(datim_value, datapack_value, Diff), names_to = "value_type") %>%
      dplyr::mutate(value_type = dplyr::recode(value_type,
                                               datim_value = "Current",
                                               datapack_value = "Proposed",
                                               Diff = "Difference"))
    
  }
  
 d
}
