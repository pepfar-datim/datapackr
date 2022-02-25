#' @export
#' @title Check Data Pack sheet for non-numeric values
#'
#' @description Checks data pulled from a single sheet in a Data Pack and
#' alerts where there are non-numeric values instead of valid data.
#'
#' @param header_cols Header columns to check
#' @inheritParams datapackr_params
#'
#' @return d
#'
checkNumericValues <- function(d, sheet, header_cols = NULL) {

  if (sheet %in% c("SNU x IM", "PSNUxIM") & d$info$tool == "Data Pack") {
    data <- d$data$SNUxIM
  } else {
    data <- d$data$extract
  }

  header_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet,
                  !is.na(indicator_code),
                  !indicator_code %in% c("sheet_num", "ID", "SNU1"),
                  col_type %in% c("row_header", "target")) %>%
    dplyr::filter(col_type == "row_header")

  if (d$info$tool == "OPU Data Pack") {

    # Should `header_cols` be brought in as a parameter or reproduced here?
    data %<>%
      tidyr::gather(key = "mechCode_supportType",
                    value = "value",
                    -tidyselect::all_of(header_cols$indicator_code)) %>%
      dplyr::select(dplyr::all_of(header_cols$indicator_code),
                    mechCode_supportType, value) %>%
      tidyr::drop_na(value)

  }

  if (d$info$tool == "Data Pack" & sheet == "PSNUxIM" & d$info$cop_year %in% c(2021, 2022)) {
    data %<>%
      tidyr::gather(key = "mechCode_supportType",
                    value = "value",
                    -tidyselect::all_of(c(header_cols$indicator_code))) %>%
      dplyr::select(dplyr::all_of(header_cols$indicator_code), -indicator_code,
                    indicator_code = mechCode_supportType, value) %>%
      tidyr::drop_na(value)
  }

  non_numeric <- data %>%
    dplyr::mutate(value_numeric = suppressWarnings(as.numeric(value))) %>%
    dplyr::filter(is.na(value_numeric)) %>%
    dplyr::select(indicator_code, value) %>%
    dplyr::distinct() %>%
    dplyr::group_by(indicator_code) %>%
    dplyr::arrange(value) %>%
    dplyr::summarise(values = paste(value, collapse = ", ")) %>%
    dplyr::mutate(row_id = paste(indicator_code, values, sep = ":  ")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::select(row_id) %>%
    dplyr::mutate(sheet = sheet)

  d$tests$non_numeric <- dplyr::bind_rows(d$tests$non_numeric, non_numeric)
  attr(d$tests$non_numeric, "test_name") <- "Non-numeric values"

  if (NROW(non_numeric) > 0) {

    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": NON-NUMERIC VALUES found! Please ensure all values entered against",
        " FY22 Target columns include numeric values only - no letters or punctuation.",
        " It may be helpful to use an Excel filter to check unique values in a column for",
        " any non-numeric entries. ->  \n\t* ",
        paste(non_numeric$row_id, collapse = "\n\t* "),
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")

  }

  return(d)

}
