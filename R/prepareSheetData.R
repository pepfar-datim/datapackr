#' @export
#' @importFrom magrittr %>% %<>%
#' @title Prepare sheet-specific dataset for writing into a Data Pack sheet.
#' 
#' @description 
#' Prepares provided dataset for writing into specified Data Pack sheet.
#'
#' @param sheet Specified sheet within wb.
#' @param org_units 
#' @param schema Defaults to standard Data Pack schema, but allows for provision
#' of custom schema if needed.
#' @param sheet_data 
#' 
#' @return dataframe of data prepared for Data Pack
#'
prepareSheetData <- function(sheet,
                             org_units,
                             schema = datapackr::data_pack_schema,
                             sheet_data) {

  # Get valid disaggs ####
  valid_disaggs <- schema %>%
    dplyr::filter(data_structure == "normal"
                  & sheet_name == sheet
                  & col_type == "target") %>%
    dplyr::select(valid_ages, valid_sexes, valid_kps) %>%
    unique() %>%
    tidyr::unnest(valid_ages, .drop = FALSE, .sep = ".") %>%
    tidyr::unnest(valid_sexes, .drop = FALSE, .sep = ".") %>%
    tidyr::unnest(valid_kps, .drop = FALSE, .sep = ".") %>%
    unique() %>%
    dplyr::rename(Age = valid_ages.name,
                  Sex = valid_sexes.name,
                  KeyPop = valid_kps.name) %>%
    dplyr::arrange(Age, Sex, KeyPop)
  
  # Cross PSNUs and disaggs ####
  row_headers <- org_units %>%
    tidyr::crossing(valid_disaggs) %>%
    dplyr::mutate(
      AgeCoarse = dplyr::case_when(
        sheet == "OVC" ~ dplyr::case_when(
          Age %in% c("<01","01-04","05-09","10-14","15-17","<18") ~ "<18",
          Age %in% c("18-24","25+","18+") ~ "18+"),
        TRUE ~ dplyr::case_when(
          Age %in% c("<01","01-04","05-09","10-14","<15") ~ "<15",
          Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+","15+") ~ "15+")
      )
    ) %>%
    dplyr::select(
      PSNU, Age, Sex, KeyPop, AgeCoarse,
      psnu_uid, valid_ages.id, valid_sexes.id, valid_kps.id) %>%
    dplyr::arrange_at(dplyr::vars(dplyr::everything()))
  
  # Setup data structure ####
  dataStructure <- schema %>%
    dplyr::filter(sheet_name == sheet) %>%
    dplyr::arrange(col) %>%
    `row.names<-`(.[, "indicator_code"]) %>%
    dplyr::select(formula) %>%
    t() %>%
    tibble::as_tibble() %>%
    ## Setup formulas
    dplyr::slice(rep(1:dplyr::n(), times = NROW(row_headers))) %>%
    dplyr::mutate_if(
      is.character,
      stringr::str_replace_all,
      pattern = paste0("(?<=[:upper:])",headerRow(tool = "Data Pack Template")+1),
      replacement = as.character(1:NROW(row_headers) + headerRow(tool = "Data Pack Template")))
  
  # Swap in model data ####
  sheet_data %<>%
    tidyr::spread(key = indicator_code,
                  value = value)
  
  combined <- row_headers %>%
    dplyr::left_join(
      sheet_data,
      by = c("psnu_uid" = "psnu_uid",
             "valid_ages.id" = "age_option_uid",
             "valid_sexes.id" = "sex_option_uid",
             "valid_kps.id" = "kp_option_uid"))
  
  dataStructure <- dataStructure %>%
    swapColumns(., combined) %>%
    as.data.frame(.)
  
  return(dataStructure)

}
