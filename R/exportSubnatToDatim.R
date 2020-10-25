#' @export
#' @importFrom magrittr %>% %<>%
#' @title exportSubnatToDATIM(d)
#'
#' @description Takes the outputs of the \code{\link{unPackSheets}} function and
#'     adds  a dataframe containing SUBNAT and IMPATT data,
#'     \code{d$data$SUBNAT_IMPATT} into a standard DATIM import file.
#'
#' @param data SUBNAT/IMPATT dataframe to pack for DATIM.
#' 
#' @return Datapackr d object
#' 
exportSubnatToDATIM <- function(d) {
  
  dataelement_value_types<-datapackr::cop20_data_pack_schema %>% 
    dplyr::filter(col_type == "target") %>% 
    dplyr::select(dataelement = dataelement_dsd,value_type) %>% 
    tidyr::drop_na() %>% 
    dplyr::distinct()
  
  d$datim$subnat_impatt <- d$data$SUBNAT_IMPATT %>%
    dplyr::left_join(., ( datapackr::map_DataPack_DATIM_DEs_COCs %>% 
                        dplyr::rename(Age = valid_ages.name,
                                      Sex = valid_sexes.name,
                                       KeyPop = valid_kps.name) ),
                     by = c("indicator_code", "Age", "Sex", "KeyPop")) %>% 
    dplyr::left_join(dataelement_value_types,by="dataelement") %>% 
    dplyr::mutate(
      period = paste0( d$info$cop_year ,"Oct" ),
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
    dplyr::mutate( value = dplyr::case_when( value_type == "integer" ~ datapackr::round_trunc(value),
                                      TRUE ~ value)) %>% 
    dplyr::select(
      dataElement = dataelement,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo,
      value
    ) 
  
  duplicated_rows<-d$datim$subnat_impatt %>% 
    dplyr::group_by(dataElement,orgUnit,categoryOptionCombo,attributeOptionCombo) %>% 
    dplyr::tally() %>% 
    dplyr::filter(n > 1)
    
  d$tests$duplicated_subnat_impatt<-duplicated_rows
  attr(d$tests$duplicated_subnat_impatt,"Duplicated SUBNAT/IMPATT data")
  
  
  # TEST: Whether any NAs in any columns
  if ( NROW(duplicated_rows) > 0 ) {
    warning_msg <-
      paste0(
        "ERROR! In tab SUBNATT/IMPATT. Duplicate rows. Contact support.")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  blank_rows<-d$datim$subnat_impatt %>% 
    dplyr::filter_all(dplyr::any_vars(is.na(.)))
  
  # TEST: Whether any NAs in any columns
  if ( NROW(blank_rows) > 0 ) {
    d$tests$blank_rows_datim_subnat_impatt<-blank_rows
    attr(d$tests$blank_rows_datim_subnat_impatt, "test_name")<-"SUBNAT/IMPATT data with blanks"
    warning_msg <-
      paste0(
        "ERROR! In tab SUBNATT/IMPATT. DATIM Export has blank rows. Contact support.")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # TEST: Any Negative values? (not allowed for SUBNAT/IMPATT dataset)
  if (any(d$datim$subnat_impatt$value < 0)) {
    
    warning_msg <-("ERROR occurred. Negative values present in SUBNAT/IMPATT data.")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  return(d)
}
