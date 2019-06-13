#' @importFrom magrittr %>% %<>%
#' @title checkColStructure(d)
#'
#' @description Checks structural integrity of columns on critical sheets for
#'    submitted Data Pack or Site Tool.
#'
#' @param d datapackr list object containing at least d$data$extract,
#'     d$data$sheet & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool columns names or order.
checkColStructure <- function(d) {
  # Check column structure
  msg <- NULL
  
  submission_cols <- names(d$data$extract) %>%
    tibble::as_tibble() %>%
    dplyr::select(indicatorCode = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  col_check <- datapackr::data_pack_schema %>%
    dplyr::filter(sheet_name == d$data$sheet) %>%
    dplyr::select(indicator_code, template_order = col) %>%
    dplyr::full_join(submission_cols, by = c("indicator_code" = "indicatorCode")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  ## Alert to missing cols
  if (any(is.na(col_check$submission_order))) {
    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(indicator_code)
    msg <- paste0("In tab",d$data$sheet, 
                  " MISSING COLUMNS: Note that this may be due to missing/renamed sheets,
       or added or renamed columns.:  ",
                  paste(missing_cols, collapse = ", "),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to added Columns
  if (any(is.na(col_check$template_order))) {
    added_cols <- col_check %>%
      dplyr::filter(is.na(template_order)) %>%
      dplyr::pull(indicator_code)
    msg <- paste0( "In tab ",d$data$sheet, 
                   " ADDED/RENAMED COLUMNS: DO NOT rename columns.
                   Adding columns is ok : ", 
                   paste(added_cols, collapse = ","),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to surprises in column order
  if (!all(col_check$order_check, na.rm = TRUE)) {
    out_of_order <- col_check %>%
      dplyr::filter(order_check == FALSE) %>%
      dplyr::pull(indicator_code)
    msg <- paste0("In tab ",d$data$sheet,
                  " COLUMNS OUT OF ORDER: Note that this may be due to missing, 
        added, or renamed columns: ", 
                  paste(out_of_order, collapse = ","),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  return(d)
}


#' @title checkSiteToolColStructure(d)
#'
#' @description Checks structural integrity of columns on critical sheets for
#'    submitted  Site Tool.
#'
#' @param d datapackr list object containing at least d$data$extract,
#'     d$data$sheet & d$info$warningMsg.
#' @return A datapackr list object, \code{d}, storing a warning message of all
#'    issues related to Data Pack or Site Tool columns names or order.
checkSiteToolColStructure <- function(d) {
  # Check column structure
  msg <- NULL
  
  submission_cols <- names(d$data$extract) %>%
    tibble::as_tibble() %>%
    dplyr::select(indicatorCode = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  col_check <- datapackr::site_tool_schema %>%
    dplyr::filter(sheet_name == d$data$sheet) %>%
    dplyr::select(indicator_code, template_order = col) %>%
    dplyr::full_join(submission_cols, by = c("indicator_code" = "indicatorCode")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  ## Alert to missing cols
  if (any(is.na(col_check$submission_order))) {
    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(indicator_code)
    msg <- paste0("In tab",d$data$sheet, 
                  " MISSING COLUMNS: Note that this may be due to missing/renamed sheets,
                  or added or renamed columns.:  ",
                  paste(missing_cols, collapse = ", "),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to added Columns
  if (any(is.na(col_check$template_order))) {
    added_cols <- col_check %>%
      dplyr::filter(is.na(template_order)) %>%
      dplyr::pull(indicator_code)
    msg <- paste0( "In tab ",d$data$sheet, 
                   " ADDED/RENAMED COLUMNS: DO NOT rename columns.
                   Adding columns is ok : ", 
                   paste(added_cols, collapse = ","),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  ## Alert to surprises in column order
  if (!all(col_check$order_check, na.rm = TRUE)) {
    out_of_order <- col_check %>%
      dplyr::filter(order_check == FALSE) %>%
      dplyr::pull(indicator_code)
    msg <- paste0("In tab ",d$data$sheet,
                  " COLUMNS OUT OF ORDER: Note that this may be due to missing, 
                  added, or renamed columns: ", 
                  paste(out_of_order, collapse = ","),"")
    d$info$warningMsg<-append(msg,d$info$warningMsg)
  }
  
  return(d)
}
