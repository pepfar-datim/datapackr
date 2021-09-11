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
  if (sheet %in% c("SNU x IM","PSNUxIM") & d$info$tool == "Data Pack") {
    data <- d$data$SNUxIM
  } else {
    data <- d$data$extract
  }

  if (d$info$tool == "OPU Data Pack") {
    header_cols <- d$info$schema %>%
      dplyr::filter(
        sheet_name == sheet,
        col_type == "row_header"
      ) %>%
      dplyr::pull(indicator_code) %>%
      c(., "mechCode_supportType")
  } else {
    header_cols <- c("PSNU", "indicator_code", "Age", "Sex", "KeyPop")
  }

  # TEST for duplicates ####
  duplicates <- data %>%
    dplyr::filter(value != 0) %>%
    dplyr::select(tidyselect::any_of(header_cols)) %>%
    dplyr::group_by(dplyr::across(tidyselect::everything())) %>%
    dplyr::summarise(n = (dplyr::n()), .groups = "drop") %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-n) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::across(tidyselect::everything())) %>%
    dplyr::mutate(sheet = sheet)

  if (NROW(duplicates) > 0) {

    d$tests$duplicate_rows <- dplyr::bind_rows(d$tests$duplicate_rows, duplicates)
    attr(d$tests$duplicate_rows, "test_name") <- "Duplicated rows"

    dupes_msg <-
      capture.output(
        print(as.data.frame(duplicates), row.names = FALSE)
      )

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": DUPLICATE ROWS found. Ensure rows are all unique, and the SNU Disaggregates",
        " are not repeated within tabs. This issue may have been caused by inadvertent",
        " or incorrect copying of data from one row to another. Duplicates are not permitted. -> \n\t",
        paste(dupes_msg, collapse = "\n\t"),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg,"ERROR")
    d$info$has_error <- TRUE

  }
  return(d)

}
