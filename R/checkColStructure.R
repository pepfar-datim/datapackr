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
  if (sheet %in% c("SNU x IM","PSNUxIM")) {
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
                  & !(sheet %in% c("SNU x IM","PSNUxIM")
                        & indicator_code %in% c("12345_DSD","12345_TA"))) %>%
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
        or added or renamed columns. ->  \n\t* ",
        paste(missing_cols, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Alert to columns which may be out of order
  columns_out_of_order <- col_check[which(col_check$template_order != col_check$submission_order),"indicator_code"]
  d[["tests"]][["columns_out_of_order"]][[as.character(sheet)]] <- columns_out_of_order
  
  if ( length(columns_out_of_order) > 0 ) {
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", OUT OF ORDER COLUMNS: DO NOT add columns on the left or remove any columns. ->  \n\t* ",
        paste(columns_out_of_order, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
 
  return(d)
}
