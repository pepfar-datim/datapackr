
#' @title getFY22Prioritizations(d)
#'
#' @param  d Datapackr object
#'
#' @return Dataframe of psnu_uid and value
#' @export
#'
getFY22Prioritizations <- function(d) {
  
  psnu_prioritizations <- d$datim$fy22_prioritizations %>%
    dplyr::select(orgUnit, value)
  
  psnus<-dplyr::filter(datapackr::valid_PSNUs,psnu_type =="SNU") %>% 
    dplyr::filter(country_uid %in% d$info$country_uids) %>% 
    dplyr::select(ancestor_uid = psnu_uid,ancestor_name = psnu)
  
  #Classify any DREAMS districts the same as their PSNU parents
  dreams_prioritizations<-dplyr::filter(valid_PSNUs, DREAMS == "Y") %>% 
    dplyr::select(psnu_uid,psnu,ancestors) %>% 
    tidyr::unnest("ancestors") %>% 
    dplyr::select(-organisationUnitGroups) %>% 
    dplyr::group_by(psnu_uid,psnu) %>% 
    dplyr::summarise(path = paste(id,sep="",collapse="/")) %>% 
    dplyr::ungroup() %>% 
    fuzzyjoin::regex_inner_join(psnus,by=c("path" = "ancestor_uid")) %>% 
    dplyr::inner_join(psnu_prioritizations,by=c("ancestor_uid" = "orgUnit")) %>% 
    dplyr::select(orgUnit=psnu_uid,value)
  
  dplyr::bind_rows(psnu_prioritizations,dreams_prioritizations)
  
}


#' @export
#' @importFrom magrittr %>% %<>%
#' @title createAnalytics(d)
#'
#' @description Wrapper function for creation of d$data$analytics object
#' which is suitable for export to external analytics sytems.
#'
#' @param d Datapackr object
#'
#' @return Modified d object with d$data$analytics
#'
#'
createAnalytics <- function(d,
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {
  # Append the distributed MER data and subnat data together
  if (d$info$tool == "OPU Data Pack") {
    d$data$analytics <- d$datim$OPU %>% 
      adorn_import_file(cop_year = d$info$cop_year,
                        psnu_prioritizations = NULL,
                        d2_session = d2_session)
  }

  if (d$info$tool == "Data Pack") {
    if (d$info$cop_year == 2020) {
      d$data$analytics <- dplyr::bind_rows(
        d$data$distributedMER,
        d$data$SUBNAT_IMPATT %>%
          dplyr::mutate(
            mechanism_code = "HllvX50cXC0",
            support_type = "DSD")
        )
    }
    if (d$info$cop_year == 2021) {
      # For COP21+, get data from import files for better consistency ####
      fy22_prioritizations <- getFY22Prioritizations(d)
      
      d$data$analytics <-
        dplyr::bind_rows(
          d$datim$MER,
          d$datim$subnat_impatt) %>%
        adorn_import_file(cop_year = d$info$cop_year,
                          psnu_prioritizations = fy22_prioritizations,
                          d2_session)
    }
  }

  
  #This has been moved to adorn_import_file :point_up
  # # Add timestamp and FY ####
  # d$data$analytics %<>%
  #   dplyr::mutate(upload_timestamp = format(Sys.time(),"%Y-%m-%d %H:%M:%S", tz = "UTC"),
  #                 fiscal_year = paste0("FY", stringr::str_sub(as.integer(d$info$cop_year)+1,-2)))

  #TODO: This seems to no longer be required since it has been 
  # moved to adorn_import_file
  # Column names coming out of adorn_import_file
  # 
  # if (d$info$tool == "Data Pack") {
  #   d$data$analytics %<>%
  #     dplyr::select( ou,
  #                    ou_id,
  #                    country_name,
  #                    country_uid,
  #                    snu1,
  #                    snu1_id,
  #                    psnu,
  #                    psnu_uid,
  #                    prioritization,
  #                    mechanism_code,
  #                    mechanism_desc,
  #                    partner_id,
  #                    partner_desc,
  #                    funding_agency  = agency,
  #                    fiscal_year,
  #                    dataelement_id  = dataElement,
  #                    dataelement_name = dataelementname,
  #                    indicator = technical_area,
  #                    numerator_denominator,
  #                    support_type,
  #                    hts_modality,
  #                    categoryoptioncombo_id = categoryOptionCombo,
  #                    categoryoptioncombo_name = categoryoptioncomboname,
  #                    age = Age,
  #                    sex = Sex,
  #                    key_population = KeyPop,
  #                    resultstatus_specific = resultstatus,
  #                    upload_timestamp,
  #                    disagg_type,
  #                    resultstatus_inclusive,
  #                    top_level,
  #                    target_value = value,
  #                    indicator_code)
  # } else {
  #   d$data$analytics %<>%
  #     dplyr::select( ou,
  #                    ou_id,
  #                    country_name,
  #                    country_uid,
  #                    snu1,
  #                    snu1_id,
  #                    psnu,
  #                    psnu_uid,
  #                    mechanism_code,
  #                    mechanism_desc,
  #                    partner_id,
  #                    partner_desc,
  #                    funding_agency  = agency,
  #                    fiscal_year,
  #                    dataelement_id  = dataelement,
  #                    dataelement_name = dataelement.y,
  #                    indicator = technical_area,
  #                    numerator_denominator ,
  #                    support_type ,
  #                    hts_modality ,
  #                    categoryoptioncombo_id = categoryoptioncombouid,
  #                    categoryoptioncombo_name = categoryoptioncombo,
  #                    age = Age,
  #                    sex = Sex,
  #                    key_population = KeyPop,
  #                    resultstatus_specific = resultstatus,
  #                    upload_timestamp,
  #                    disagg_type,
  #                    resultstatus_inclusive,
  #                    top_level,
  #                    target_value = value,
  #                    indicator_code)
  # }

  return(d)
}
