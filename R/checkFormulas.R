#' @export
#' @title Check formulas used in specified sheet in submitted Data Pack.
#'
#' @description Checks formulas to make sure they are up to date and have not
#' been tampered with.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to unpack.
#' 
#' @return d
#' 
checkFormulas <- function(d, sheet) {

  header_row <- headerRow(tool = "Data Pack", cop_year = d$info$cop_year)
  
  # Pull in formulas from Data Pack sheet ####
  formulas_datapack <-
    tidyxl::xlsx_cells(path = d$keychain$submission_path,
                      sheets = sheet,
                      include_blank_cells = T) %>%
    dplyr::filter(row >= header_row) %>%
    dplyr::mutate(
      formula = 
        dplyr::case_when(
          is.na(formula) ~ as.character(numeric),
          TRUE ~ formula
        )
    ) %>%
    dplyr::select(row, col, character, formula) %>%
    dplyr::left_join(
      ((.) %>%
         dplyr::filter(row == header_row) %>%
         dplyr::select(col, indicator_code = character)),
      by = c("col" = "col")) %>%
    dplyr::select(row, col, indicator_code, formula) %>%
    dplyr::filter(row != header_row) %>%
    
    # Handle duplicate column headers ####
    dplyr::group_by(row) %>%
    dplyr::mutate(occurrence = duplicated(indicator_code)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(occurrence == FALSE) %>%
    dplyr::select(-occurrence)
  
  # Pull in formulas from schema ###
    
  formulas_schema <- datapackr::cop20_data_pack_schema %>%
    dplyr::filter(sheet_name == sheet) %>%
    dplyr::select(col, indicator_code, formula) %>%
    tidyr::crossing(row = ((header_row+1):max(formulas_datapack$row))) %>%
    dplyr::select(row, col, indicator_code, formula) %>%
    dplyr::mutate(
      formula =
        stringr::str_replace_all(
          formula,
          pattern = paste0("(?<=[:upper:])", header_row+1),
          replacement = as.character((header_row+1):max(formulas_datapack$row))
          )
      )
  
  # Compare formulas from schema against Data Pack to see diffs ####
  
  formulas_comparison <- formulas_schema %>%
    dplyr::left_join(
      formulas_datapack,
      by = c("row" = "row", "indicator_code" = "indicator_code")) %>%
    dplyr::mutate(
      formula_diff = formula.x == formula.y) %>%
    dplyr::filter(formula_diff == FALSE)
  
  # Compile warning message ####
  if (NROW(formulas_comparison) > 0) {
  
    cols_affected <- formulas_comparison %>%
      dplyr::select(indicator_code) %>%
      dplyr::distinct() %>%
      dplyr::pull(indicator_code)
    
    correct_formulas <- formulas_schema %>%
      dplyr::filter(row == header_row + 1,
                    indicator_code %in% cols_affected) %>%
      dplyr::select(indicator_code, formula) %>%
      tidyr::unite(correct_fx, c(indicator_code, formula), sep = ":   ") %>%
      dplyr::pull(correct_fx)
    
    warning_msg <- 
      paste0(
        "WARNING! In tab ",
        sheet,
        ", ALTERED FORMULAS: Note that this may be due to a formula being deleted",
        " or overwritten, or a manual fix not being applied. See",
        " https://github.com/pepfar-datim/Data-Pack-Feedback/wiki/Manual-Data-Pack-Fixes-for-COP20",
        " for all manual fixes that must be applied to your Data Pack prior to",
        " submission. Affected columns and their correct formulas include ->  \n\n* ",
        paste(correct_formulas, collapse = "\n\n* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
      
  }
  
  return(d)  
}
