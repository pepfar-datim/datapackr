#' @export
#' @importFrom magrittr %>% %<>%
#' @title checkDuplicateRows(d)
#'
#' @description Checks data pulled from a single sheet in a Data Pack and
#' alerts where there are duplicate rows.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#' 
#' @return d
#' 
checkDuplicateRows <- function(d, sheet) {
  if (sheet %in% c("SNU x IM","PSNUxIM")) {
    data = d$data$SNUxIM
  } else {
    data = d$data$extract
  }
  
  # TEST for duplicates ####
  duplicates <- data %>%
    dplyr::select(PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::group_by(PSNU, Age, Sex, KeyPop, indicator_code) %>%
    dplyr::summarise(n = (dplyr::n())) %>%
    dplyr::filter(n > 1) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::select(PSNU, Age, Sex, KeyPop) %>%
    dplyr::distinct()
  
  if (NROW(duplicates) > 0) {
    d[["tests"]][["duplicates"]][[as.character(sheet)]] <- character()
    d[["tests"]][["duplicates"]][[as.character(sheet)]] <- duplicates
    
    dupes_msg <-
      capture.output(
        print(as.data.frame(duplicates), row.names = FALSE)
      )
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": DUPLICATE ROWS found. Duplicates are not permitted. -> \n\t",
        paste(dupes_msg, collapse = "\n\t"),
        "\n")
    
    d$info$warning_msg <- append(d$info$warning_msg, warning_msg)
    d$info$has_error <- TRUE
    
  }
  return(d)

}
