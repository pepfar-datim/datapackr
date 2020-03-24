#' @export
#' @importFrom magrittr %>% %<>%
#' @title createAnalytics(d)
#'
#' @description Wrapper function for creation of d$data$analtyics object 
#' which is suitable for export to external analytics sytems. 
#'
#' @param d
#' 
#' @return Modified d object with d$data$analtyics
#' 
#' 
createAnalytics <- function(d) {
  #Append the distributed MER data and subnat data together
  d$data$analytics <- dplyr::bind_rows(
    d$data$distributedMER,
    dplyr::mutate(
      d$data$SUBNAT_IMPATT,
      mechanism_code = "HllvX50cXC0",
      support_type = "DSD"
    )
  )
  #Adorn organisation units
  d <- adornPSNUs(d)
  #Adorn mechanisms
  d$data$analytics <- adornMechanisms(d$data$analytics)
  #TODO: Centralize this fix with exportDistributeMERtoDATIM
  map_DataPack_DATIM_DEs_COCs_local <- datapackr::map_DataPack_DATIM_DEs_COCs
  map_DataPack_DATIM_DEs_COCs_local$valid_sexes.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                       map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Male PWID"] <- "Male"
  map_DataPack_DATIM_DEs_COCs_local$valid_sexes.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                       map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Female PWID"] <- "Female"
  map_DataPack_DATIM_DEs_COCs_local$valid_kps.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                     map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Male PWID"] <- NA_character_
  map_DataPack_DATIM_DEs_COCs_local$valid_kps.name[map_DataPack_DATIM_DEs_COCs_local$indicator_code == "KP_MAT.N.Sex.T" &
                                                     map_DataPack_DATIM_DEs_COCs_local$valid_kps.name == "Female PWID"] <- NA_character_
  
  
  #Adorn data element and category option group sets
  d$data$analytics %<>%  dplyr::left_join(
    .,
    (
      map_DataPack_DATIM_DEs_COCs_local %>%
        dplyr::rename(
          Age = valid_ages.name,
          Sex = valid_sexes.name,
          KeyPop = valid_kps.name
        )
    ),
    by = c("Age", "Sex", "KeyPop", "indicator_code", "support_type")
  ) %>% 
  dplyr::mutate(upload_timestamp = format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                fiscal_year = "FY21") %>% 
    dplyr::select( ou,
                   ou_id,
                   country_name,
                   country_uid,
                   snu1,
                   snu1_id,
                   psnu,
                   psnuid,
                   prioritization,
                   mechanism_code,
                   mechanism_desc,
                   partner_id,
                   partner_desc,
                   funding_agency  = agency,
                   fiscal_year,
                   dataelement_id  = dataelement,
                   dataelement_name = dataelement.y,
                   indicator = technical_area,
                   numerator_denominator ,
                   support_type ,
                   hts_modality ,
                   categoryoptioncombo_id = categoryoptioncombouid,
                   categoryoptioncombo_name = categoryoptioncombo,
                   age = Age,
                   sex = Sex, 
                   key_population = KeyPop,
                   resultstatus_specific = resultstatus,
                   upload_timestamp,
                   disagg_type,
                   resultstatus_inclusive,
                   top_level,
                   target_value = value,
                   indicator_code)
  
  return(d)
}
