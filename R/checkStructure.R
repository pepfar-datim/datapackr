#' @export
#' @title Check tab structure of submitted tool.=
#' 
#' @description Checks structural integrity of sheets for submitted tool.
#'
#' @param d Datapackr object
#' 
#' @return d
#' 
checkStructure <- function(d) {
  # Check structural integrity of Workbook sheets
  msg <- NULL
  
  submission_sheets <-
    readxl::excel_sheets(d$keychain$submission_path) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::select(sheet_name = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  # Check all sheets present and accounted for
  if (d$info$tool == "Data Pack") {
    schema <- datapackr::data_pack_schema
  } else if (d$info$tool == "Site Tool") {
    schema <- datapackr::site_tool_schema
  }
  #TODO Add once https://github.com/pepfar-datim/datapackr/issues/43 resolved
  #else if (d$info$tool == "Mechanism Map") {
  #   schema <- datapackr::mech_map_schema
  # }
  
  sheets_check <- schema %>%
    dplyr::select(sheet_name, template_order = sheet_num) %>%
    dplyr::distinct() %>%
    dplyr::left_join(submission_sheets, by = c("sheet_name")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  d$info$sheets_check <- sheets_check
  
  # Alert to missing Sheets
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)
  
  if (any(is.na(sheets_check$submission_order))) {
    missing_sheets <- sheets_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(sheet_name)
    
    msg <- paste0(
      "MISSING SHEETS: Did you delete or rename these tabs?): ",
      paste0(missing_sheets, collapse = ", "), "")
    d$info$warning_msg <- append(msg, d$info$warning_msg)
  }
  
  return(d)
  
}
