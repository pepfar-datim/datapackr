#' @export
#' @importFrom magrittr %>% %<>%
#' @title checkColStructure(d)
#'
#' @description Checks structural integrity of columns on critical sheets for
#'    submitted Data Pack.
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
    dplyr::mutate(
      sheet = sheet,
      submission_order = as.integer(1:(dplyr::n()))) %>%
    {if (sheet == "PSNUxIM") {
      dplyr::filter(.,
        !stringr::str_detect(indicator_code, "\\d{4,}_(DSD|TA)|^$"))
      } else {.}}
  
  col_check <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet) %>%
    dplyr::select(indicator_code, template_order = col) %>%
    {if (sheet == "PSNUxIM") {
      dplyr::filter(.,
                    !stringr::str_detect(indicator_code, "\\d{4,}_(DSD|TA)|^$"))
    } else {.}} %>%
    dplyr::left_join(submission_cols, by = c("indicator_code" = "indicator_code")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  d[["info"]][["col_check"]][[as.character(sheet)]] <- character()
  d[["info"]][["col_check"]][[as.character(sheet)]] <- col_check
  
  # Alert to missing cols ####
  if (any(is.na(col_check$submission_order))) {
    
    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::select(sheet,indicator_code)
    
    d$tests$missing_cols<-dplyr::bind_rows(d$tests$missing_cols,missing_cols)
    attr(d$tests$missing_cols,"test_name")<-"Missing columns"
    
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", MISSING COLUMNS: Please ensure no columns have been deleted or renamed from",
        " the original Data Pack you have received. ->  \n\t* ",
        paste(missing_cols$indicator_code, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
  
  # Alert to duplicate columns ####
  submission_cols_no_blanks <- submission_cols %>%
    dplyr::filter(indicator_code != "") %>%
    dplyr::select(sheet,indicator_code)
  
  duplicate_columns <- submission_cols_no_blanks %>% 
    dplyr::mutate(duplicated_cols=duplicated(indicator_code)) %>% 
    dplyr::filter(duplicated_cols)
    
   
  if (NROW(duplicate_columns) > 0) {
    
    d$tests$duplicate_columns <-
      dplyr::bind_rows(duplicate_columns, d$tests$duplicate_columns)
    attr(d$tests$duplicate_columns,"test_name")<-"Duplicated columns"
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", DUPLICATE COLUMNS: The following required columns appear twice. This",
        " must be resolved in your submission in order for processing to continue.",
        " Please review those columns flagged by this test to determine whether they",
        " may have been inadvertently duplicated. ->  \n\t* ",
        paste(duplicate_columns$indicator_code, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
  }
  
  #  TODO: Add code to combine or drop duplicate columns.
  
  # Alert to columns which may be out of order ####
  
  columns_out_of_order <- col_check %>% 
    dplyr::filter(template_order != submission_order) %>% 
    dplyr::select(sheet,
                  columns_out_of_order = indicator_code)
    
  
  if ( NROW(columns_out_of_order) > 0 ) {

    d$tests$columns_out_of_order <- dplyr::bind_rows(columns_out_of_order,d$tests$columns_out_of_order)
    attr(d$tests$columns_out_of_order,"test_name") <- "Columns out of order"
    
    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ", OUT OF ORDER COLUMNS: While it is permitted to rearrange columns within",
        " your Data Pack as needed, this is not encouraged as it may introduce unintended",
        " formula errors. Please review these columns to ensure their rearrangement has",
        " not caused any issues. ->  \n\t* ",
        paste(columns_out_of_order$columns_out_of_order, collapse = "\n\t* "),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
  }
 
  return(d)
}
