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
  if (sheet %in% c("SNU x IM","PSNUxIM") & d$info$tool == "Data Pack") {
    data = d$data$SNUxIM
  } else {
    data = d$data$extract
  }
  
  header_row <- headerRow(tool = "Data Pack", cop_year = d$info$cop_year)

  # Pull in formulas from schema ###
  formulas_schema <- d$info$schema %>%
    dplyr::filter(
      sheet_name == sheet,
      !is.na(formula)) %>%
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
                  row <= (NROW(data) + header_row)) %>%
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
    {if (sheet == "PSNUxIM" & d$info$tool == "Data Pack") {
      (.)
    } else {
      dplyr::group_by(., row) %>%
        dplyr::mutate(occurrence = duplicated(indicator_code)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(occurrence == FALSE) %>%
        dplyr::select(-occurrence) %>%
        # Limit to only columns that DUIT cares about
        dplyr::filter(indicator_code %in% formulas_schema$indicator_code)
        }
      } %>%
    dplyr::mutate(
      formula = stringr::str_replace_all(
        formula,
        "(?<=[:upper:])\\d+",
        "\\\\d+"
      ))

  # Compare formulas from schema against Data Pack to see diffs ####
  #TODO: Add sheet to the output for audit purposes
  altered_formulas <- formulas_schema %>%
    dplyr::left_join(
      formulas_datapack,
      by = 
        {if (sheet == "PSNUxIM" & d$info$tool == "Data Pack") {
          c("col" = "col")
          } else {c("indicator_code" = "indicator_code")}
        }
    ) %>%
    dplyr::filter(formula.x != formula.y) %>%
    {if (sheet == "PSNUxIM" & d$info$tool == "Data Pack") {
      dplyr::rename(., indicator_code = indicator_code.y)
      } else {.}} %>%
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
        "WARNING! In tab ",
        sheet,
        ", " ,NROW(cols_affected)," ALTERED FORMULAS:",
        " Altering formulas without DUIT and PPM approval may lead to programmatic",
        " and technical issues in your Data Pack.",
        " Note that this may be due to a formula being deleted",
        " or overwritten, or a manual fix not being applied.",
        " Affected columns and the number of violations are listed below. ->  \n\t* ",
        paste(cols_affected$fx_violations, collapse = "\n\t* "),
        "\n")

    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)

  }

  return(d)
}
