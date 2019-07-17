#' @export
#' @importFrom magrittr %>% %<>%
#' @title checkColStructure(d)
#'
#' @description Checks structural integrity of columns on critical sheets for
#'    submitted Data Pack or Site Tool.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#' 
#' @return d
#' 
checkColStructure <- function(d, sheet) {
  if (sheet == "SNU x IM") {
    data = d$data$SNUxIM
  } else {
    data = d$data$extract
  }
  
  submission_cols <- names(data) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::select(indicator_code = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  col_check <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & !(sheet == "SNU x IM" & indicator_code == "Mechanism1")) %>%
    dplyr::select(indicator_code, template_order = col) %>%
    dplyr::left_join(submission_cols, by = c("indicator_code" = "indicator_code")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  d[["tests"]][["col_check"]][[as.character(sheet)]] <- col_check
  
  # Alert to missing cols
  if (any(is.na(col_check$submission_order))) {
    
    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(indicator_code)
    
    d[["tests"]][["missing_cols"]][[as.character(sheet)]] <- missing_cols
    
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", MISSING COLUMNS: Note that this may be due to missing/renamed sheets,
        or added or renamed columns. ->  \n  * ",
        paste(missing_cols, collapse = "\n  * "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  return(d)
}
