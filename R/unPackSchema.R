#' @export
#' @title Extract and save schema from Data Pack template.
#' 
#' @description
#' Supplied a filepath to a Data Pack template (XLSX), will extract and save a
#' schema based on the template.
#' 
#' @param path Local filepath for a Data Pack template (XLSX).
#' @param skip Character vector of Sheet Names to label for skipping in schema.
#' 
#' @return Data Pack schema.
#'
unPackSchema_datapack <- function(filepath, skip = NA) {
  
  filepath <- handshakeFile(path = filepath,
                            tool = "Data Pack Template")
  
  sheets <- tidyxl::xlsx_sheet_names(filepath)
  verbose_sheets <- sheets[!sheets %in% skip]
  
  schema <- tidyxl::xlsx_cells(path = filepath, include_blank_cells = FALSE) %>%
    dplyr::select(sheet_name = sheet, col, row, character, formula, numeric,
                  width, style_format)
  
  data.table::setDT(schema)[,sheet_num:=.GRP, by = c("sheet_name")]
  
  schema %<>%
    dplyr::filter(sheet_name %in% verbose_sheets,
                  row %in% c(3,5,6)) %>%
    tidyr::gather(key,value,-sheet_num,-sheet_name,-col,-row) %>%
    tidyr::unite(new.col, c(key,row)) %>%
    tidyr::spread(new.col,value) %>%
    dplyr::select(sheet_num,sheet_name,col,
                  label = character_3,
                  indicator_code = character_5,
                  formula = formula_6,
                  value = numeric_6) %>%
    dplyr::mutate(
      formula = dplyr::case_when(is.na(formula) ~ value,
                                 TRUE ~ formula),
      col_type = dplyr::case_when(
        stringr::str_detect(indicator_code, "20T")
        & !(sheet_name == "Prioritization" & indicator_code != "IMPATT.PRIORITY_SNU.20T")
        & !(sheet_name == "HTS" & (!stringr::str_detect(indicator_code,"^HTS_") | stringr::str_detect(indicator_code,"HTS_TST_PMTCT")))
        & !(sheet_name == "VMMC" & stringr::str_detect(indicator_code, "POP_EST|coverage"))
        ~ "Target",
        indicator_code %in% c("PSNU","Age","Sex","ID","AgeCoarse","IDAgeCoarse",
                              "PSNUCheck","KeyPop","sheet_name","indicatorCode",
                              "CoarseAge","sheet_num")
        ~ "Row Header"),
      dataset = dplyr::case_when(
        col_type == "Target" & stringr::str_detect(indicator_code,"SUBNAT|VL_SUPPRESSED") ~ "SUBNAT",
        col_type == "Target" & stringr::str_detect(indicator_code,"PLHIV|POP_EST|HIV_PREV|PRIORITY|KP_ESTIMATES") ~ "IMPATT",
        col_type == "Target" ~ "MER")) %>%
    dplyr::select(-value) %>%
    dplyr::arrange(sheet_num, col)
  
  return(schema)
}