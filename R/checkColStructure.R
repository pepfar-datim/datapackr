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
  msg <- NULL
  
  submission_cols <- names(d$data$extract) %>%
    tibble::as_tibble() %>%
    dplyr::select(indicator_code = value) %>%
    dplyr::mutate(submission_order = as.integer(1:(dplyr::n())))
  
  if (d$info$tool == "Data Pack") {
    schema <- datapackr::data_pack_schema
  } else if (d$info$tool == "Site Tool") {
    schema <- datapackr::site_tool_schema
  } else {stop("Cannot process that kind of tool.")}
  
  col_check <- schema %>%
    dplyr::filter(sheet_name == sheet) %>%
    dplyr::select(indicator_code, template_order = col) %>%
    dplyr::full_join(submission_cols, by = c("indicator_code" = "indicator_code")) %>%
    dplyr::mutate(order_check = template_order == submission_order)
  
  ## Alert to missing cols
  if (any(is.na(col_check$submission_order))) {
    missing_cols <- col_check %>%
      dplyr::filter(is.na(submission_order)) %>%
      dplyr::pull(indicator_code)
    msg <- paste0("In tab", sheet, 
                  " MISSING COLUMNS (Did you delete or rename these columns?):  ",
                  paste(missing_cols, collapse = ", "),"")
    d$info$warningMsg <- append(msg, d$info$warningMsg)
  }
  
  return(d)
}
