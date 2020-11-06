#' @export
#' @importFrom magrittr %>% %<>%
#' @title adornPSNUs(d)
#'
#' @description Creates an analtyical dimension for planning prioritization from SUBNAT data
#' contained in the datapack
#'
#' @param d Datapackr object
#' 
#' @return Modified d$data$distributedMER 
#' object with a DATIM compatible data frame for import id d$datim$MER
#' 
#' 
adornPSNUs <- function(d) {
  
  PSNUList <- datapackr::valid_PSNUs %>%
      dplyr::select(country_name, country_uid, psnu, psnu_uid)
  
  d$data$analytics %<>% dplyr::left_join(PSNUList, by = c("psnuid" = "psnu_uid"))
  
  
  # Skip all of this if the tool is an OPU Data Pack
  if (d$info$tool == "Data Pack") {  
    prio_defined <- prioritization_dict() %>%
      dplyr::select(value, prioritization = name)

    # We need to add the prioritization as a dimension here
    prio <- d$data$SUBNAT_IMPATT %>% 
      dplyr::filter(indicator_code == "IMPATT.PRIORITY_SNU.T") %>% 
      dplyr::select(psnuid, value) %>% 
      dplyr::left_join(prio_defined, by = "value") %>% 
      dplyr::select(-value)

    d$data$analytics %<>%
      dplyr::left_join(prio, by = "psnuid") %>% 
      dplyr::mutate(
        prioritization =
          dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                           TRUE ~ prioritization))
  }

  d
  
}
