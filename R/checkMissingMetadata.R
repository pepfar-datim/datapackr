#' @export
#' @title checkMissingMetadata(d)
#'
#' @description Checks data pulled from a single sheet in a Data Pack and
#' alerts where there are NAs instead of valid metadata.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check.
#' @param quiet Logical. Should warning messages be printed? Default is TRUE.
#'
#' @return d
#'
checkMissingMetadata <- function(d, sheet, quiet = T) {
  
  if (!quiet) {
    messages <- MessageQueue()
  }

  # Get data ----
  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {

    data <- d$data$SNUxIM
  } else {
    data <- d$data$extract
  }
  
  # BELOW IS THE TRANSITION to LOAD DATAPACK ONCE THAT IS ADDED TO CREATEKEYCHAININFO
  # if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {
  #   
  #   data <- d$sheets[["PSNUxIM"]]
  # } else {
  #   data <- d$sheets[[as.character(sheet)]]
  # }
  
  # mung ----
  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)
  
  missing_metadata <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row = dplyr::row_number() + header_row,
                  sheet = sheet) %>%
    dplyr::filter_at(dplyr::vars(dplyr::matches("^PSNU$|^ID$|^indicator_code$")),
                     dplyr::any_vars(is.na(.)))
  
  # test ----
  if (NROW(missing_metadata) > 0) {
    lvl <- "ERROR"
    
    msg <-
      paste0(
        lvl,"! In tab ",
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
    
    
    d$tests$missing_metadata <- dplyr::bind_rows(d$tests$missing_metadata, missing_metadata)
    attr(d$tests$missing_metadata, "test_name") <- "Missing metadata"
    d$info$messages <- appendMessage(d$info$messages, msg, lvl)
    d$info$has_error <- TRUE
    
    if (!quiet) {
      messages <- appendMessage(messages, msg, lvl)
    }
    
  }
  
  if (!quiet) {
    printMessages(messages)
  }
  
  return(d)
  
}
