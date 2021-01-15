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
  
  # Pull in formulas from schema ###
  formulas_schema <- datapackr::cop20_data_pack_schema %>%
    dplyr::filter(
      sheet_name == sheet,
      !is.na(formula),
      !sheet_name %in% c("Epi Cascade I","Epi PMTCT","Prioritization","PrEP","PSNUxIM"),
      !indicator_code %in% c("TX_CURR.N.growth",
                             "TX_CURR.N.natlContribution",
                             "TX_NEW.N.VMMCRateNew",
                             "VMMC_CIRC_SUBNAT.Age_Sex.T_1",
                             "VMMC_TOTALCIRC_SUBNAT.N.Age_Sex.T_1",
                             "VMMC_CIRC.N.natlContribution",
                             "KP_ESTIMATES.N.KeyPop_TotalSizeEstimate.T",
                             "KP_ESTIMATES.N.KeyPop_PositiveEstimate.T",
                             "KP_ESTIMATES.N.KeyPop_Prevalence.T"),
      (indicator_code == "TX_NEW.N.otherRate"
       | col_type %in% c("reference","target")),
      !(sheet_name == "KP" 
          & !(indicator_code %in% c("HTS_RECENT.N.KeyPop_HIVStatus.T",
                                    "TX_PVLS.D.KeyPop_HIVStatus.T",
                                    "TX_PVLS.N.KeyPop_HIVStatus.T")))
      ) %>%
    dplyr::select(col, indicator_code, formula) %>%
    # tidyr::crossing(row = ((header_row+1):max(formulas_datapack$row))) %>%
    # dplyr::select(row, col, indicator_code, formula) %>%
    # dplyr::mutate(
    #   formula =
    #     stringr::str_replace_all(
    #       formula,
    #       pattern = paste0("(?<=[:upper:])", header_row+1),
    #       replacement = as.character((header_row+1):max(formulas_datapack$row))
    #     )
    # )
    dplyr::mutate(
      formula = stringr::str_replace_all(
        formula, 
        "(?<=[:upper:])\\d+",
        "\\\\d+"
      )
    )
  
  # Pull in formulas from Data Pack sheet ####
  formulas_datapack <-
    tidyxl::xlsx_cells(path = d$keychain$submission_path,
                      sheets = sheet,
                      include_blank_cells = T) %>%
    dplyr::filter(row >= header_row,
                  row <= (NROW(d$data$extract) + header_row)) %>%
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
    dplyr::select(-occurrence) %>%
  # Limit to only columns that DUIT cares about
    dplyr::filter(indicator_code %in% formulas_schema$indicator_code) %>%
    dplyr::mutate(
      formula = stringr::str_replace_all(
        formula,
        "(?<=[:upper:])\\d+",
        "\\\\d+"
      ))
  
  # Compare formulas from schema against Data Pack to see diffs ####
  altered_formulas <- formulas_schema %>%
    dplyr::left_join(
      formulas_datapack,
      by = c("indicator_code" = "indicator_code")) %>%
    dplyr::mutate(
      formula_diff = formula.x == formula.y) %>%
    dplyr::filter(formula_diff == FALSE) %>%
    dplyr::select(
      indicator_code, correct_fx = formula.x, submitted_fx = formula.y, row) %>%
    dplyr::group_by(indicator_code, correct_fx, submitted_fx) %>%
    dplyr::mutate(count = dplyr::n()) %>%
    dplyr::group_by(indicator_code, correct_fx, submitted_fx, count) %>%
    dplyr::summarise(affected_rows = list(unique(row))) %>%
    dplyr::ungroup()
  
  d$tests$altered_formulas <- 
    dplyr::bind_rows(d$tests$altered_formulas, altered_formulas)
  attr(d$tests$altered_formulas,"test_name") <- "Altered Formulas"
  
  # Compile warning message ####
  if (NROW(altered_formulas) > 0) {
  
    cols_affected <- altered_formulas %>%
      dplyr::select(indicator_code, correct_fx, count) %>%
      dplyr::group_by(indicator_code, correct_fx) %>%
      dplyr::summarize(count = sum(count)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(fx_violations = paste0(indicator_code,":  ", count))
    
    warning_msg <- 
      paste0(
        "WARNING! In tab, ensure all Data Pack formulas are as originally provided,  ",
        sheet,
        ", " ,NROW(cols_affected)," ALTERED FORMULAS: Note that this may be due to a formula being deleted",
        " or overwritten, or a manual fix not being applied. See",
        " https://github.com/pepfar-datim/Data-Pack-Feedback/wiki/Manual-Data-Pack-Fixes-for-COP20",
        " for all manual fixes that must be applied to your Data Pack prior to",
        " submission. Affected columns and the number of violations include ->  \n\t* ",
        paste(cols_affected$fx_violations, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
      
  }
  
  return(d)  
}
