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
  
  d$tests$sheets_check <- schema %>%
    dplyr::select(sheet_name, template_order = sheet_num) %>%
    dplyr::distinct() %>%
    dplyr::left_join(submission_sheets, by = c("sheet_name")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  #TODO: Decide whether to add all sheets into schema to check against
  
  # Alert to missing Sheets
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)
  
  if (any(is.na(d$tests$sheets_check$submission_order))) {
    d$tests$missing_sheets <- d$tests$sheets_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(sheet_name)
    
    warning_msg <-
      paste0(
        "MISSING SHEETS: Did you delete or rename these tabs?: ",
        paste0(d$tests$missing_sheets, collapse = ", "),
        "")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  return(d)
  
}
