#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pull data for packing OPU Data Pack
#'
#' @description
#' Pulls data from DATIM API needed to pack OPU Data Pack.
#'
#' @param cop_year COP Year. Remember, COP20 = FY21 targets.
#' @param country_uids DATIM UIDs of Countries to return data for. Can supply either
#' this or Country Names.
#' @param country_names Names of Countries to return data for. Can supply either this
#' or DATIM Country UIDs.
#' @param d2_session DHIS2 Session id
#'
#' @return Returns dataset needed for use in \code{\link{packOPUDataPack}}.
#'
getOPUDataFromDATIM <- function(cop_year,
                                country_uids = NULL,
                                country_names = NULL,
                                d2_session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  
  if (cop_year == 2020){
    map_DataPack_DATIM_DEs_COCs_local <- 
      datapackr::cop20_map_DataPack_DATIM_DEs_COCs
  } else {
    stop("The COP year provided is not supported by getOPUDataFromDATIM")
  }
  
  options("scipen" = 999)
  options(warning.length = 8170)
  
  # Select Countries to pull data for ####
  if (is.null(country_names) & is.null(country_uids)) {
    stop("ERROR! Must provide either country_uids or country_names.")
  }
  
  if (is.null(country_uids)) {
    all_country_uids <- 
      datimutils::getOrgUnitGroups("Country",
                                   by = name,
                                   fields = "organisationUnits[name,id]",
                                   d2_session = d2_session) %>%
      dplyr::arrange(name)
    
    mapped_country_uids <- all_country_uids %>%
      dplyr::right_join(
        tibble::tibble(country_names), by = c("name" = "country_names"))
    
      country_uids <- mapped_country_uids %>%
        dplyr::filter(!is.na(id)) %>%
        dplyr::pull(id)
    
    if (any(is.na(mapped_country_uids))) {
      stop("The following Country Names either are not supported by PEPFAR or do not match DATIM syntax:\r\n\r\n  - ",
            paste0(mapped_country_uids$name[is.na(mapped_country_uids$id)], collapse = "\r\n  - "),
            "\r\n\r\nAcceptable Country Names include: \r\n\r\n  - ",
           paste0(all_country_uids$name, collapse = "\r\n  - "))
    }
  }
  
  # Pull data from DATIM ####
  data_datim <- datapackr::getCOPDataFromDATIM(country_uids,
                                               cop_year,
                                               streams = "mer_targets",
                                               d2_session = d2_session)
  
  # Filter data by required indicator_codes ####
  indicator_codes <- datapackr::cop20_data_pack_schema %>% 
    dplyr::filter(dataset == "mer",
                  col_type =="target") %>% 
    .[["indicator_code"]]
  
  data_datim %<>%
    dplyr::left_join(map_DataPack_DATIM_DEs_COCs_local,
                      by = c("dataElement" = "dataelement",
                            "categoryOptionCombo" = "categoryoptioncombouid"))
  
  if (any(is.na(data_datim$indicator_code))) {
    stop("Problem mapping target data pulled from DATIM to datapack schema")
  }
  
  data_datim %<>%
    dplyr::filter(indicator_code %in% indicator_codes) %>% 
    dplyr::select(indicator_code,
                  support_type,
                  period,
                  psnu_uid = orgUnit,
                  age_option_uid = valid_ages.id,
                  Age = valid_ages.name,
                  sex_option_uid = valid_sexes.id,
                  Sex = valid_sexes.name,
                  kp_option_uid = valid_kps.id,
                  KeyPop = valid_kps.name,
                  attribute_option = attributeOptionCombo,
                  value)
  
  return(data_datim)

}
