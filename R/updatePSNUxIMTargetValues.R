
#' Title updatePSNUxIMTargetValues
#'
#' @param d
#'
#' @return Modified d object with a PSNUxIM workbook object.
#' Target values in the "Data Pack Target" column will be updated with values
#' from the main DataPack tabs.
#' @export
#'
updatePSNUxIMTargetValues <- function(d) {

  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

  updated_targets <- d$sheets$PSNUxIM %>%
    dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, original_target = DataPackTarget) %>%
    dplyr::left_join(d$tests$non_equal_targets, by = c("PSNU", "indicator_code", "Age", "Sex", "KeyPop")) %>%
    dplyr::mutate(are_equal = ifelse(is.na(are_equal), TRUE, FALSE)) %>%
    dplyr::mutate(final_target = ifelse(!are_equal, MainTabsTarget, original_target)) %>%
    dplyr::select(PSNU, indicator_code, Age, Sex, KeyPop, DataPackTarget = final_target)


  if (NROW(updated_targets) == 0) {
    warning("Not updating anything since there was nothing to update.")
    return(d)
  }

  interactive_print("Loading existing PSNUxIM file.")
  wb <- openxlsx::loadWorkbook(d$keychain$psnuxim_file_path)
  openxlsx::activeSheet(wb) <- "PSNUxIM"


    openxlsx::writeData(
      wb,
      "PSNUxIM",
      startCol = cellranger::letter_to_num("G"),
      startRow = header_row + 1,
      x = updated_targets$DataPackTarget
    )

  d$tool$wb <- wb

  d
}

