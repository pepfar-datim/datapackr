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
  
  getPSNUList<-function() {
    datapackr::valid_PSNUs %>%
      dplyr::mutate(
        ou_id = purrr::map_chr(ancestors, list("id", 3), .default = NA),
        ou = purrr::map_chr(ancestors, list("name", 3), .default = NA),
        snu1_id = dplyr::if_else(
          condition = is.na(purrr::map_chr(ancestors, list("id",4), .default = NA)),
          true = psnu_uid,
          false = purrr::map_chr(ancestors, list("id",4), .default = NA)),
        snu1 = dplyr::if_else(
          condition = is.na(purrr::map_chr(ancestors, list("name",4), .default = NA)),
          true = psnu,
          false = purrr::map_chr(ancestors, list("name",4), .default = NA))
      ) %>%
      dplyr::select(ou, ou_id, country_name, country_uid, snu1, snu1_id, psnu, psnu_uid)
  }
  
  d$data$analytics %<>% dplyr::left_join(getPSNUList(), by = c("psnuid" = "psnu_uid"))
  
  prio_defined<-tibble::tribble(
    ~value,~prioritization,
    1,"Scale-Up: Saturation",
    4 ,"Sustained",
    0 ,"No Prioritization",
    6, "Sustained: Commodities" ,
    2, "Scale Up: Aggressive"  ,   
    5, "Centrally Supported",
    7,  "Attained" ,
    8, "Not PEPFAR Supported" )
  
  
  #We need to add the prioritization as a dimension here
  prio <- d$data$SUBNAT_IMPATT %>% 
    dplyr::filter(indicator_code == "IMPATT.PRIORITY_SNU.T") %>% 
    dplyr::select(psnuid,value) %>% 
    dplyr::left_join(prio_defined,by="value") %>% 
    dplyr::select(-value)
  
  d$data$analytics %<>% dplyr::left_join(prio,by="psnuid") %>% 
    dplyr::mutate(prioritization = case_when(is.na(prioritization) ~ "No Prioritization",
                                             TRUE ~ prioritization ))
  
  d
  
}
