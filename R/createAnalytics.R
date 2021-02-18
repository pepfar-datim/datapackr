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
    d$data$analytics <- d$data$extract %>%
      dplyr::select(
        PSNU, psnuid, indicator_code, Age, Sex, KeyPop,
        mechanism_code = mech_code, support_type, value
      )
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
      d$data$analytics <-
  # Get data from import files for better consistency
        dplyr::bind_rows(
          d$datim$MER,
          d$datim$subnat_impatt) %>%
        adorn_import_file(cop_year = 2021,
                          d2_session)

      prio_defined <- prioritization_dict() %>%
        dplyr::select(value, prioritization = name)

  # We need to add the prioritization as a dimension here
      prio <- d$datim$fy22_prioritizations %>%
        dplyr::select(psnu_uid = orgUnit, value) %>%
        dplyr::left_join(prio_defined, by = "value") %>%
        dplyr::select(-value)

      d$data$analytics %<>%
        dplyr::left_join(prio, by = "psnu_uid") %>%
        dplyr::mutate(
          prioritization =
            dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                             TRUE ~ prioritization))
    }
  }

  # Add timestamp and FY ####
  d$data$analytics %<>%
    dplyr::mutate(upload_timestamp = format(Sys.time(),"%Y-%m-%d %H:%M:%S"),
                  fiscal_year = paste0("FY", stringr::str_sub("FY",-2)))

  # Selects appropriate columns based on COP or OPU tool
  if (d$info$tool == "Data Pack") {
    d$data$analytics %<>%
      dplyr::select( ou,
                     ou_id,
                     country_name,
                     country_uid,
                     snu1,
                     snu1_id,
                     psnu,
                     psnu_uid,
                     prioritization,
                     mechanism_code,
                     mechanism_desc,
                     partner_id,
                     partner_desc,
                     funding_agency  = agency,
                     fiscal_year,
                     dataelement_id  = dataElement,
                     dataelement_name = dataelementname,
                     indicator = technical_area,
                     numerator_denominator,
                     support_type,
                     hts_modality,
                     categoryoptioncombo_id = categoryOptionCombo,
                     categoryoptioncombo_name = categoryoptioncomboname,
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
  } else {
    d$data$analytics <- d$data$analytics %>%
      dplyr::select_if(tolower(names(.)) %in% c("ou",
                     "ou_id",
                     "country_name",
                     "country_uid",
                     "snu1",
                     "snu1_id",
                     "psnu",
                     "psnu_uid",
                     "mechanism_code",
                     "mechanism_desc",
                     "partner_id",
                     "partner_desc",
                     "agency",
                     "fiscal_year",
                     "dataelement",
                     "dataelement.y",
                     "technical_area",
                     "numerator_denominator" ,
                     "support_type" ,
                     "hts_modality" ,
                     "categoryoptioncombouid",
                     "categoryoptioncombo",
                     "age",
                     "sex",
                     "keypop",
                     "resultstatus",
                     "upload_timestamp",
                     "disagg_type",
                     "resultstatus_inclusive",
                     "top_level",
                     "value",
                     "indicator_code"))
    
    
    namekey <- c(resultstatus = "resultstatus_specific", categoryoptioncombouid = "categoryoptioncombo_id", 
    categoryoptioncombo = "categoryoptioncombo_name", value = "target_value", KeyPop = "key_population",
    Sex = "sex", Age = "age", agency = "funding_agency", dataelement = "dataelement_id", dataelement.y = "dataelement_name",
    technical_area = "indicator")
    
    sel <- is.element(colnames(d$data$analytics), names(namekey))
    names(d$data$analytics)[sel] <- namekey[names(d$data$analytics)][sel]
    
    
  }
  
  
  


  return(d)
}
