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
  
  # Check structural integrity of Workbook sheets ####
  submission_sheets <-
    readxl::excel_sheets(d$keychain$submission_path) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::select(sheet_name = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  # Check all sheets present and accounted for ####
  d$tests$sheets_check <- d$info$schema %>%
    dplyr::select(sheet_name, template_order = sheet_num) %>%
    dplyr::distinct() %>%
    dplyr::left_join(submission_sheets, by = c("sheet_name")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  # TEST for missing Sheets ####
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)
  
  if (any(is.na(d$tests$sheets_check$submission_order))) {
   
    missing_sheets <- d$tests$sheets_check %>%
      dplyr::filter(is.na(submission_order))
    
    d$tests$missing_sheets<-dplyr::bind_rows(
      d$tests$missing_sheets,missing_sheets
      ) %>% dplyr::filter(!is.na(missing_sheets))
    attr(d$tests$missing_sheets,"test_name")<-"Missing sheets"
    
      paste0(
        "MISSING SHEETS: Did you delete or rename these tabs? -> \n  * ",
        paste0(d$tests$missing_sheets, collapse = "\n  * "),
        "\n")
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  return(d)
  
}
