#' @export
#' @importFrom magrittr %>% %<>%
#' @title Loop through and populate normal Data Pack sheets
#' 
#' @description 
#' Loops through all normally structured sheets in a submitted Data Pack
#' and writes data.
#'
#' @param sheet Specified sheet within wb
#' @param row_headers Structure to use in row_headers
#' @param data Data from DATIM
#' 
#' @return dataframe of data prepared for Data Pack
#'
prepareSheetData <- function(sheet, row_headers, data) {

left <- data_structure %>%
  dplyr::filter(sheet_name == sheet)
  
dataCols <- d$info$schema %>%
  dplyr::filter(sheet_name == sheet
                & col_type %in% c("past","assumption")
                & is.na(formula)) %>%
  dplyr::pull(indicator_code)

right <- data %>%
  dplyr::filter(indicator_code %in% dataCols) %>%
  tidyr::spread(key = indicator_code,
                value = value)
                     
combined <- left %>%
  dplyr::left_join(
    right,
    by = c("psnu_uid" = "psnu_uid",
           "valid_ages.id" = "age_option_uid",
           "valid_sexes.id" = "sex_option_uid",
           "valid_kps.id" = "kp_option_uid"))


dataStructure <- d$info$schema %>%
  dplyr::filter(sheet_name == sheet) %>%
  dplyr::arrange(col) %>%
  `row.names<-`(.[, "indicator_code"]) %>%
  dplyr::select(formula) %>%
  t() %>%
  tibble::as_tibble() %>%
  # Setup formulas
  dplyr::slice(rep(1:dplyr::n(), times = NROW(left))) %>%
  dplyr::mutate_if(
    is.character,
    stringr::str_replace_all,
    pattern = paste0("(?<=[:upper:])",headerRow(tool = "Data Pack Template")+1),
    replacement = as.character(1:NROW(left) + headerRow(tool = "Data Pack Template")))

dataStructure <- dataStructure %>%
  swapColumns(., combined) %>%
  as.data.frame(.)
