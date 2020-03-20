#' @export
#' @importFrom magrittr %>% %<>%
#' @title adornPSNUs(d)
#'
#' @description Creates an analtyical dimension for planning prioritization from SUBNAT data
#' contained in the datapack
#'
#' @param d
#' 
#' @return Modified d$data$distributedMER 
#' object with a DATIM compatible data frame for import id d$datim$MER
#' 
#' 
adornPSNUs<-function(d) {
  
  PSNUList <- datapackr::valid_PSNUs %>%
      dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id, psnu, psnu_uid)
  
  d$data$analytics %<>% dplyr::left_join(PSNUList, by = c("psnuid" = "psnu_uid"))
  
  prio_defined <- prioritization_dict() %>%
    dplyr::select(value, prioritization = name)
  
  # We need to add the prioritization as a dimension here
  prio <- d$data$SUBNAT_IMPATT %>% 
    dplyr::filter(indicator_code == "IMPATT.PRIORITY_SNU.T") %>% 
    dplyr::select(psnuid, value) %>% 
    dplyr::left_join(prio_defined, by= "value") %>% 
    dplyr::select(-value)
  
  d$data$analytics %<>%
    dplyr::left_join(prio, by = "psnuid") %>% 
    dplyr::mutate(
      prioritization =
        dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                         TRUE ~ prioritization))
  
  d
  
}
