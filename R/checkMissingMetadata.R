#' @export
#' @importFrom magrittr %>% %<>%
#' @title checkMissingMetadata(d)
#'
#' @description Checks data pulled from a single sheet in a Data Pack and
#' alerts where there are NAs instead of valid metadata.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#' 
#' @return d
#' 
checkMissingMetadata <- function(d, sheet) {
  if (sheet %in% c("SNU x IM","PSNUxIM") & d$info$tool == "Data Pack") {
    data = d$data$SNUxIM
  } else {
    data = d$data$extract
  }
  
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
    
  missing_metadata <- data %>%
    dplyr::mutate(row = (1:dplyr::n()) + header_row,
                  sheet = sheet) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("^PSNU$|^ID$|^indicator_code$")),
                     dplyr::any_vars(is.na(.)))

  # Alert to missing metadata
  if (NROW(missing_metadata) > 0) {

    d$tests$missing_metadata <- dplyr::bind_rows(d$tests$missing_metadata, missing_metadata)
    
    attr(d$tests$missing_metadata,"test_name") <- "Missing metadata"
    
    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ", MISSING PSNU, INDICATOR_CODE, OR ID: Review any tabs flagged by this test",
        " to investigate whether PSNU, Age, Sex, or Key Population identifier",
        " information data have been deleted.",
        NROW(missing_metadata),
        " rows where blank entries exist in the PSNU, indicator_code, or ID columns.",
        " Note that blank entries in these columns will prevent processing of",
        " data in that row. The following rows are affected: ",
        paste(missing_metadata$row, collapse = ", "),
        "\n")
    
    d$info$messages <- appendMessage(d$info$messages, warning_msg,"ERROR")
    d$info$has_error <- TRUE
  }
  
  return(d)
  
}
