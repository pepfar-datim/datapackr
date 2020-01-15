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
  
  fullCodeList <- pullFullCodeList(FY = cop_year +1) %>%
    dplyr::left_join(datapackr::valid_COCs_COs, by = c("categoryoptioncombouid" = "id")) %>%
    dplyr::mutate(categoryOptions = purrr::map_chr(categoryOptions,~.x[["id"]] %>% 
                                                     sort() %>% paste(collapse = ".")))
  
  schema <- d$info$schema %>%
    dplyr::filter(col_type == "target") %>%
    dplyr::select(indicator_code, dataelement_dsd, dataelement_ta,
                  categoryoption_specified, valid_ages, valid_sexes, valid_kps) %>%
    tidyr::unnest(cols = valid_ages, names_sep  = ".") %>%
    tidyr::unnest(cols = valid_sexes, names_sep  = ".") %>%
    tidyr::unnest(cols = valid_kps, names_sep  = ".") %>%
    dplyr::mutate(categoryOptions.ids = purrr::pmap(list(valid_ages.id, valid_sexes.id, valid_kps.id, categoryoption_specified ), cbind)) %>% 
    dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, sort)) %>%
    dplyr::mutate(categoryOptions.ids = purrr::map(categoryOptions.ids, na.omit)) %>% 
    dplyr::mutate(categoryOptions.ids = purrr::map_chr(categoryOptions.ids, paste, collapse = ".")) %>%
    tidyr::pivot_longer(cols = dataelement_dsd:dataelement_ta,
                        names_to = "support_type",
                        values_to = "dataelement",
                        names_prefix = "dataelement_",
                        values_drop_na = TRUE) %>%
    dplyr::left_join(getHTSModality(cop_year = d$info$cop_year),
                     by = c("dataelement" = "dataElement")) %>%
    dplyr::left_join(getTechArea(),
                     by = c("dataelement" = "dataElement")) %>%
    dplyr::mutate(support_type = toupper(support_type)) %>%
    dplyr::left_join(fullCodeList,
                     by = c("dataelement" = "dataelementuid",
                            "categoryOptions.ids" = "categoryOptions"))
  
  if (type == "PSNU") {
    sj <- d$data$MER %>%
      dplyr::bind_rows(d$data$SUBNAT_IMPATT) %>%
      dplyr::left_join(PSNUs, by = c("psnuid" = "psnu_uid")) %>%
      dplyr::left_join(schema,
                       by = c("indicator_code" = "indicator_code",
                              "Age" = "valid_ages.name",
                              "Sex" = "valid_sexes.name",
                              "KeyPop" = "valid_kps.name")) %>%
      dplyr::mutate(mechanism_code = "12345",
                    mechanism_desc = "default",
                    partner_id = NA_character_,
                    partner_desc = NA_character_,
                    fiscal_year = 2021,
                    funding_agency = NA_character_,
                    categoryoptioncombo_id = categoryoptioncombouid,
                    categoryoptioncombo_name = categoryoptioncombo,
                    result_value = NA_character_) %>%
      dplyr::select(
        ou_id, snu1_id, psnuid,
        mechanism_code, mechanism_desc, partner_id, partner_desc, funding_agency,
        fiscal_year,
        sex = Sex, indicator = tech_area, support_type, hts_modality,
        categoryoptioncombo_id, categoryoptioncombo_name,
        result_value, target_value = value)
    
    readr::write_csv(sj, path = "/Users/scott/Google Drive/PEPFAR/COP Targets/COP 20/3) Testing & Deployment/DataPackTEST.csv")
                    
      
  } else if (type == "PSNU x IM") {
    d$data$paw <- d$data$SNUxIM %>%
      dplyr::bind_rows(d$data$SUBNAT_IMPATT)
    
  } else {stop("Can't send that to PAW.")}
  
  return (d)
}
  