#' @importFrom magrittr %>% %<>%
#' @title checkWorkbookStructure(d)
#'
#' @description Checks structural integrity of tabs for submitted Data Pack or
#'    Site Tool.
#'
#' @param d datapackr list object containing at least d$keychain$submission_path
#'     & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool tab names or order.
checkWorkbookStructure <- function(d) {
  # Check structural integrity of Workbook tabs
  msg <- NULL
  
  submission_sheets <-
    readxl::excel_sheets(d$keychain$submission_path) %>%
    tibble::as_tibble() %>%
    dplyr::select(sheet_name = value) %>%
    dplyr::filter(!sheet_name %in% c("Home", "Quotes", "Summary", "Spectrum", "Validations")) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n()) + 4))
  
  # Check all tabs present and accounted for
  sheets_check <- datapackr::data_pack_schema %>%
    dplyr::select(sheet_name, template_order = sheet_num) %>%
    dplyr::distinct() %>%
    dplyr::full_join(submission_sheets, by = c("sheet_name")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  ## Alert to missing Sheets
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)
  
  if (any(is.na(sheets_check$submission_order))) {
    missing_sheets <- sheets_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(sheet_name)
    
    msg <- paste0(
      "MISSING SHEETS: Be advised that while deleting tabs will
      not prohibit data processing,
      it may cause issues in formulas in the SNU x IM tab.
      : ",
      paste0(missing_sheets, collapse = ", "), "")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to added/renamed sheets
  interactive_print("Checking for any added or renamed tabs...")
  if (any(is.na(sheets_check$template_order))) {
    added_sheets <- sheets_check %>%
      dplyr::filter(is.na(template_order)) %>%
      dplyr::pull(sheet_name)
    msg <- paste0(
      "ADDED/RENAMED SHEETS: Be advised that while adding tabs for custom 
       purposes will not prohibit data processing, renaming existing tabs will.
      : ",paste(added_sheets, collapse = ", "),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to surprises in sheet order
  interactive_print("Checking for any surprises in tab order...")
  if (!all(sheets_check$order_check, na.rm = TRUE)) {
    out_of_order <- sheets_check %>%
      dplyr::filter(order_check == FALSE) %>%
      dplyr::pull(sheet_name)
    msg <- paste0(
      "SHEETS OUT OF ORDER: Be advised that reordering tabs may not prohibit
       data processing, and this issue may be related to missing, 
       added, or renamed sheets. : ", paste(out_of_order, collapse = ","),"")
    
    d$info$warningMsg <- append(msg,d$info$warningMsg)
  }
  
  return(d)
  
}


#' @title checkSiteToolStructure(d)
#'
#' @description Checks structural integrity of tabs for SiteTool
#'
#' @param d datapackr list object containing at least d$keychain$submission_path
#'     & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool tab names or order.
checkSiteToolStructure <- function(d) {
  # Check structural integrity of Workbook tabs
  msg <- NULL
  ##Remove this after https://github.com/pepfar-datim/datapackr/issues/25
  site_tool_additional_sheets <-
    tibble::tribble(
      ~sheet_name, ~template_order, ~should_unpack,
      "Home",   1, FALSE,
      "Site List",   2, FALSE,
      "Mechs",   3, FALSE,
      "Validations", 4, FALSE
    )
  
  submission_sheets <-
    readxl::excel_sheets(d$keychain$submission_path) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::select(sheet_name = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  # Check all tabs present and accounted for
  sheets_check <- datapackr::site_tool_schema %>%
    dplyr::select(sheet_name, template_order = sheet_num) %>%
    dplyr::distinct() %>%
    ##Remove this after https://github.com/pepfar-datim/datapackr/issues/25
    dplyr::mutate(template_order = template_order  + 1,
                  should_unpack = TRUE) %>% 
    dplyr::bind_rows(site_tool_additional_sheets,.) %>% 
    dplyr::left_join(submission_sheets, by = c("sheet_name")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  d$info$sheets_info <- sheets_check
  
  ## Alert to missing Sheets
  info_msg <- "Checking for any missing tabs..."
  interactive_print(info_msg)
  
  if (any(is.na(sheets_check$submission_order))) {
    missing_sheets <- sheets_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(sheet_name)
    
    msg <- paste0(
      "MISSING SHEETS: Be advised that while deleting tabs will
      not prohibit data processing.: ",
      paste0(missing_sheets, collapse = ", "), "")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to added/renamed sheets
  interactive_print("Checking for any added or renamed tabs...")
  if (any(is.na(sheets_check$template_order))) {
    added_sheets <- sheets_check %>%
      dplyr::filter(is.na(template_order)) %>%
      dplyr::pull(sheet_name)
    msg <- paste0(
      "ADDED/RENAMED SHEETS: Be advised that while adding tabs for custom 
       purposes will not prohibit data processing, renaming existing tabs will.
      : ",paste(added_sheets, collapse = ", "),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to surprises in sheet order
  interactive_print("Checking for any surprises in tab order...")
  if (!all(sheets_check$order_check, na.rm = TRUE)) {
    out_of_order <- sheets_check %>%
      dplyr::filter(order_check == FALSE) %>%
      dplyr::pull(sheet_name)
    msg <- paste0(
      "SHEETS OUT OF ORDER: Be advised that reordering tabs may not prohibit
       data processing, and this issue may be related to missing, 
       added, or renamed sheets. : ", paste(out_of_order, collapse = ","),"")
    
    d$info$warningMsg <- append(msg,d$info$warningMsg)
  }
  
  return(d)
  
}
