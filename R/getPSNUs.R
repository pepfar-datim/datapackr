#' @export
#' @title Pull list of PSNUs from DATIM based on provided country UIDs.
#' 
#' @description
#' Queries DATIM to extract list of PSNUs and adds additional PSNUs not
#' currently in DATIM as needed.
#' 
#' @param country_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit UIDs will not be accepted
#' @param include_mil Logical. If \code{TRUE}, will also include _Military nodes
#' related to \code{country_uids}. Default is \code{TRUE}.
#' 
#' @return Data frame of PSNUs
#' 
getPSNUs <- function(country_uids = NA,
                     include_mil = TRUE) {
  
  # Pull PSNUs from DATIM ####
  PSNUs <- api_call("organisationUnits") %>%
    api_filter("organisationUnitGroups.id","in",
               paste0("AVy8gJXym2D",
                      dplyr::if_else(include_mil, ",nwQbMeALRjL", ""))) %>%
    {if (all(!is.na(country_uids)))
      api_filter(., "ancestors.id", "in", match = paste(country_uids, collapse = ","))
      else . } %>%
    datapackr::api_fields("id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]") %>%
    datapackr::api_get()
  
  # Extract metadata ####
  PSNUs %<>%
    dplyr::mutate(
      psnu_type =
        dplyr::case_when(
          stringr::str_detect(as.character(organisationUnitGroups), "nwQbMeALRjL") ~ "Military",
          stringr::str_detect(as.character(organisationUnitGroups), "AVy8gJXym2D") ~ "PSNU")
    ) %>%
    tidyr::unnest(ancestors, .drop = FALSE, .sep = ".") %>%
    tidyr::unnest(ancestors.organisationUnitGroups, .drop = FALSE, .sep = ".") %>%
    dplyr::filter(ancestors.organisationUnitGroups.name == "Country") %>%
    dplyr::select(psnu = name, psnu_uid = id, psnu_type,
                  country_name = ancestors.name, country_uid = ancestors.id)
  
  # Add countries that double as PSNUs ####
  country_as_psnu <- getIMPATTLevels() %>%
    dplyr::filter(country_uid %in% country_uids
                  & country == prioritization) %>%
    dplyr::pull(country_uid)
  
  if (length(country_as_psnu) > 0) {
    countries_as_PSNUs <-
      api_call("organisationUnits") %>%
      api_filter("id","in",paste(country_as_psnu,collapse=",")) %>%
      datapackr::api_fields("id,name,ancestors[id,name,organisationUnitGroups[id,name]],organisationUnitGroups[id,name]") %>%
      datapackr::api_get() %>%
      dplyr::mutate(country_name = name, country_uid = id,
                    psnu_type = "PSNU") %>%
      dplyr::select(psnu = name, psnu_uid = id, psnu_type,
                    country_name, country_uid)
    
    PSNUs %<>% dplyr::bind_rows(countries_as_PSNUs)
  }
  
  return(PSNUs)
}


#' @export
#' @title Modify PSNU list to add datapackr IDs.
#' 
#' @description
#' Queries DATIM to extract list of PSNUs and adds additional PSNUs not
#' currently in DATIM as needed.
#'
#' @param PSNUs Data frame of PSNUs produced by \code{\link{getPSNUs}}.
#' @param country_uids Character vector of DATIM country IDs. This can only
#' include countries. Regional Operating Unit UIDs will not be accepted
#' 
#' @return Data frame of PSNUs
#' 
getPSNUs <- function(PSNUs, country_uids) {

# Create Data Pack PSNU ID & tag with country name breadcrumb where country != PSNU
PSNUs %<>%
  dplyr::mutate(
    dp_psnu = paste0(
      dplyr::if_else(
        length(country_uids) > 1 & country_uid != psnu_uid,
        paste0(country_name, " > "),
        ""),
      psnu, " [", psnu_uid,"]")
  )
  
  return(PSNUs)
}
