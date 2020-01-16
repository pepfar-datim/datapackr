#' @export
#' @importFrom magrittr %>% %<>%
#' @title Loop through and populate normal Data Pack sheets
#' 
#' @description 
#' Loops through all normally structured sheets in a submitted Data Pack
#' and writes data.
#'
#' @param wb datapackr list object.
#' @param country_uids datapackr list object.
#' @param model_data
#' 
#' @return wb with all sheets written except SNU x IM
#'
packDataPackSheets <- function(wb, country_uids, model_data) {
  
  # Get valid disaggs ####
  valid_disaggs <- datapackr::data_pack_schema %>%
    dplyr::filter(data_structure == "normal"
                  & sheet_name != "SNU x IM"
                  & col_type == "target") %>%
    dplyr::select(sheet_num, sheet_name, valid_ages, valid_sexes, valid_kps) %>%
    unique() %>%
    tidyr::unnest(valid_ages, .drop = FALSE, .sep = ".") %>%
    tidyr::unnest(valid_sexes, .drop = FALSE, .sep = ".") %>%
    tidyr::unnest(valid_kps, .drop = FALSE, .sep = ".") %>%
    unique() %>%
    dplyr::rename(Age = valid_ages.name,
                  Sex = valid_sexes.name,
                  KeyPop = valid_kps.name) %>%
    dplyr::arrange(sheet_num, Age, Sex, KeyPop)
  
  # Get PSNUs ####
  #TODO: Add code here to pull from config based on country_uids
  
  # Cross PSNUs and disaggs ####
  row_headers <- PSNUs %>%
    tidyr::crossing(valid_disaggs) %>%
    dplyr::mutate(
      AgeCoarse = dplyr::if_else(
        sheet_name == "OVC",
        true = dplyr::case_when(
          Age %in% c("<01","01-04","05-09","10-14","15-17","<18") ~ "<18",
          Age %in% c("18-24","25+","18+") ~ "18+"),
        false = dplyr::case_when(
          Age %in% c("<01","01-04","05-09","10-14","<15") ~ "<15",
          Age %in% c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+","15+") ~ "15+")
      )
    ) %>%
    dplyr::select(
      sheet_num, sheet_name, PSNU = dp_psnu, Age, Sex, KeyPop, AgeCoarse,
      psnu_uid, valid_ages.id, valid_sexes.id, valid_kps.id) %>%
    dplyr::arrange_at(dplyr::vars(dplyr::everything()))
  
  # Prepare data ####
  if (!all(country_uids %in% names(model_data))) {
    missing <- country_uids[!country_uids %in% names(model_data)]
    stop(
      paste0(
        "Model data file does not have data for the following country_uids: \r\n\t- ",
        paste(missing, collapse = "\r\n\t- ")
      )
    )
  }
  
  data <- model_data[country_uids] %>%
    dplyr::bind_rows() %>%
    tidyr::drop_na(value) %>%
    dplyr::select(-period)
  
  # Loop through sheets ####
  data_sheets <- datapackr::data_pack_schema %>%
    dplyr::filter(data_structure == "normal"
                  & sheet_name != "SNU x IM"
                  & sheet_name %in% names(wb)) %>%
    dplyr::pull(sheet_name) %>%
    unique()
  
  for (sheet in data_sheets) {
    sheet_data <- prepareSheetData(sheet, row_headers, data)
    
    wb <- packDataPackSheet(wb = wb,
                            sheet = sheet,
                            row_headers = row_headers,
                            data = data)
  }
  
  
  return()
}
  