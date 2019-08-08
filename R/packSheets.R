#' @export
#' @importFrom magrittr %>% %<>%
#' @title Pack data into a Data Pack sheet
#' 
#' @description 
#' Loops through all normally structured sheets in a submitted Data Pack
#' and writes data.
#'
#' @param wb Openxlsx workbook object built from template.
#' @param d datapackr list object.
#' 
#' @return wb with all sheets written except SNU x IM
#'
packDataPackSheets <- function(d) {
  
  sheets <- readxl::excel_sheets(d$keychain$template_path)
  
  if (!sheets %in% unique(d$info$template$sheet_name))
  sheets_to_loop <- sheets[which(!stringr::str_detect(sheets, "Home|Quotes|Summary|Spectrum|SNU x IM"))]
  
  skip_tabs(tool = "Data Pack Template")
  
  return()
}
  