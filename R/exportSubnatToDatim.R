#' @export
#' @importFrom magrittr %>% %<>%
#' @title exportSubnatToDATIM(d)
#'
#' @description Takes the outputs of the \code{\link{unPackSheets}} function and
#'     adds  a dataframe containing SUBNAT and IMPATT data,
#'     \code{d$data$SUBNAT_IMPATT} into a standard DATIM import file.
#'
#' @param d Datapackr object
#' 
#' @return Datapackr d object
#' 
exportSubnatToDATIM <- function(d) {
  
  SUBNAT_IMPATT <- d$data$SUBNAT_IMPATT %>%
    dplyr::left_join(datapackr::map_DataPack_DATIM_DEs_COCs,
                     by = c("indicator_code" = "indicator_code",
                            "Age" = "valid_ages.name",
                            "Sex" = "valid_sexes.name",
                            "KeyPop" = "valid_kps.name")) %>%
    dplyr::mutate(
      attributeOptionCombo = datapackr::default_catOptCombo()
    ) %>%
    dplyr::mutate(
      value =
        dplyr::case_when(
          value_type == "integer" ~ datapackr::round_trunc(value),
          TRUE ~ value))
    
  # Form into DATIM import file ####
  SUBNAT_IMPATT %<>%
    dplyr::select(
      dataElement = dataelementuid,
      period,
      orgUnit = psnuid,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo,
      value
    )
  
 # TEST: Duplicate Rows; Error; Continue ####
  duplicated_rows <- SUBNAT_IMPATT %>%
    dplyr::group_by(dataElement,orgUnit,categoryOptionCombo,attributeOptionCombo,period) %>% 
    dplyr::tally() %>% 
    dplyr::filter(n > 1)
    
  d$tests$duplicated_subnat_impatt <- duplicated_rows
  attr(d$tests$duplicated_subnat_impatt, "test_name") <- "Duplicated SUBNAT/IMPATT data"
  
  # TEST: Whether any NAs in any columns
  if ( NROW(duplicated_rows) > 0 ) {
    warning_msg <-
      paste0(
        "ERROR! In tab SUBNATT/IMPATT. Duplicate rows. Contact support.")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  
  # TEST: Blank Rows; Error;
  blank_rows <- SUBNAT_IMPATT %>% 
    dplyr::filter_all(dplyr::any_vars(is.na(.)))
  
  # TEST: Whether any NAs in any columns
  if ( NROW(blank_rows) > 0 ) {
    d$tests$blank_rows_datim_subnat_impatt <- blank_rows
    attr(d$tests$blank_rows_datim_subnat_impatt, "test_name") <- "SUBNAT/IMPATT data with blanks"
    warning_msg <-
      paste0(
        "ERROR! In tab SUBNATT/IMPATT. DATIM Export has blank rows. Contact support.")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # TEST: Negative values; Error; 
  if (any(SUBNAT_IMPATT$value < 0)) {
    warning_msg <- "ERROR occurred. Negative values present in SUBNAT/IMPATT data."
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  # Drop any rows with any NA to prevent breakage in iHub ####
  SUBNAT_IMPATT %<>%
    tidyr::drop_na()
  
  d$datim$subnat_impatt <- SUBNAT_IMPATT
  
  d$datim$subnat_fy20 <-  SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2020Q3",
      dataElement %in%
        (datapackr::map_DataPack_DATIM_DEs_COCs %>%
           dplyr::filter(period_dataset == "FY20 SUBNAT Results" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
        )
    )
    
  d$datim$subnat_fy21 <-  SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2020Oct",
      dataElement %in%
        (datapackr::map_DataPack_DATIM_DEs_COCs %>%
           dplyr::filter(period_dataset == "FY21 SUBNAT Targets" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
        )
    )
    
  d$datim$subnat_fy22 <- SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2021Oct",
      dataElement %in%
        (datapackr::map_DataPack_DATIM_DEs_COCs %>%
           dplyr::filter(period_dataset == "FY22 SUBNAT Targets" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
        )
    )
    
  d$datim$impatt_fy21 <- SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2020Oct",
      dataElement %in%
        (datapackr::map_DataPack_DATIM_DEs_COCs %>%
           dplyr::filter(period_dataset == "FY21 IMPATT" & !is.na(indicator_code)) %>%
           dplyr::pull(dataelementuid)
      )
    )
      
  d$datim$fy22_prioritizations <- SUBNAT_IMPATT %>%
    dplyr::filter(
      period == "2021Oct",
      dataElement %in%
        (datapackr::map_DataPack_DATIM_DEs_COCs %>%
           dplyr::filter(period_dataset == "FY22 IMPATT" & indicator_code == "IMPATT.PRIORITY_SNU.T") %>%
           dplyr::pull(dataelementuid)
        )
    )
  
  return(d)
}
