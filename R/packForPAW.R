#' @export
#' @importFrom magrittr %>% %<>%
#' @title packForPAW
#'  
#' @description 
#' Prepare dataset for use in PAW.
#' 
#' @param d Datapackr object.
#' @param type Type of dataset to prep for PAW. Choose from \code{PSNUxIM} or
#' \code{PSNU}.
#' 
#' @return Data frame ready for use in PAW
#' 
packForPAW <- function(d, type) {
  PSNUs <- datapackr::valid_PSNUs %>%
    dplyr::mutate(
      ou_id = purrr::map_chr(ancestors, list("id", 3), .default = NA),
      snu1_id = dplyr::if_else(
        condition = is.na(purrr::map_chr(ancestors, list("id",4), .default = NA)),
        true = psnu_uid,
        false = purrr::map_chr(ancestors, list("id",4), .default = NA))
    ) %>%
    dplyr::select(psnu_uid, ou_id, snu1_id)
  
  schema <- d$info$schema %>%
    dplyr::filter(col_type == "target") %>%
    dplyr::select(indicator_code, dataelement_dsd, dataelement_ta,
                  categoryoption_specified)
  
  #TODO: Map to dataelements and categoryoptioncombos
  
  if (type == "PSNU") {
    sj <- d$data$MER %>%
      dplyr::bind_rows(d$data$SUBNAT_IMPATT) %>%
      dplyr::left_join(PSNUs, by = c("psnuid" = "psnu_uid"))
      dplyr::mutate(mechanism_code = "12345",
                    mechanism_desc = "default",
                    partner_id = NA_character_,
                    partner_desc = NA_character_,
                    fiscal_year = 2021,
                    funding_agency = NA_character_,
                    indicator = ,
                    hts_modality = ,
                    categoryoptioncombo_id = ,
                    categoryoptioncombo_name = ,
                    result_value = NA_character_) %>%
      dplyr::select(
        ou_id, snu1_id, psnuid,
        mechanism_code, mechanism_desc, partner_id, partner_desc, funding_agency,
        fiscal_year,
        sex = Sex, indicator, hts_modality,
        categoryoptioncombo_id, categoryoptioncombo_name,
        result_value, target_value = value)
                    
      
  } else if (type == "PSNU x IM") {
    d$data$paw <- d$data$SNUxIM %>%
      dplyr::bind_rows(d$data$SUBNAT_IMPATT)
    
  } else {stop("Can't send that to PAW.")}
  
  return (d)
}
  