
y2ExtractDuplicateRows <- function(d) {

  duplicated_rows <- d$data$Year2 %>%
    dplyr::select(-value) %>%
    duplicated()

  if (any(duplicated_rows)) {
    warning_msg <-
      paste0(
        "WARNING! Duplicated rows were detected in your Year 2 tab.",
        "These will be dropped. Consult the validation test report ",
        "for specific details. \n")

    d$tests$year2_duplicated_rows <- d$data$Year2 %>%
      dplyr::filter(duplicated_rows)
    attr(d$tests$year2_duplicated_rows, "test_name") <- "Duplicated Year 2 rows"
    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  d$data$Year2 %<>%
    dplyr::filter(!duplicated_rows)

  d
}

y2ExtractInvalidDisaggs <- function(d) {


  year2_invalid_disaggs <-
    is.na(d$data$Year2$dataelementuid) |
    is.na(d$data$Year2$categoryoptioncombouid)


  if (any(year2_invalid_disaggs))  {
    warning_msg <-
      paste0(
        "WARNING! Invalid disaggregate combinations were found in the Year 2 tab.",
        "These will be dropped. Consult the validation test report ",
        "for specific details.",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    d$tests$year2_invalid_disaggs  <- d$data$Year2[year2_invalid_disaggs, ]
    attr(d$tests$year2_invalid_disaggs, "test_name") <- "Invalid Year 2 disaggs"
    d$data$Year2 <- d$data$Year2[!year2_invalid_disaggs, ]
  }

  d

}

y2TestColumnStructure <- function(d) {

  expected_cols <-
    d$info$schema %>% dplyr::filter(sheet_name == "Year 2") %>%
    dplyr::select(col, indicator_code)

  actual_cols <-
    data.frame(indicator_code = names(d$data$Year2)) %>%
    dplyr::mutate(actual_col = dplyr::row_number())

  cols_compare <-
    actual_cols %>% dplyr::full_join(expected_cols, by = "indicator_code") %>%
    dplyr::mutate(is_equal = identical(actual_col, col))

  if (!any(cols_compare$is_equal)) {
    warning_msg <-
      paste0(
        "WARNING! Columns in the Year 2 tab are missing or out of order.",
        "We will attempt to proceed with validation, however this must be fixed",
        " prior to final submission.",
        "Consult the validation report for details.",
        "\n")

    d$tests$year2_cols_out_of_order  <- cols_compare %>% dplyr::filter(!is_equal)
    attr(d$tests$year2_cols_out_of_order, "test_name") <- "Invalid Year 2 column order"

  }

  d

}

#' Title unpackYear2Sheet
#'
#' @param d
#'
#' @return d
#' @export
#'
unpackYear2Sheet <- function(d) {

  sheet <- "Year 2"

  header_row <- headerRow(tool = d$info$tool, cop_year = d$info$cop_year)

  if (is.null(d$sheets$`Year 2`)) {
    d$sheets$`Year 2` <-
      readxl::read_excel(
        path = d$keychain$submission_path,
        sheet = sheet,
        range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
        col_types = "text",
        .name_repair = "minimal"
      )
  }
  d$data$Year2 <- d$sheets$`Year 2`
  d$data$Year2 <- d$data$Year2[!(names(d$data$Year2) %in% c(""))]

  #Test column structure before any restructuring.
  d <- y2TestColumnStructure(d)

  cols_to_keep <- getColumnsToKeep(d, sheet)
  header_cols <- getHeaderColumns(cols_to_keep, sheet)


  #Use this to map back to Year1 targets
  de_map_local <-  datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
    dplyr::select(indicator_code,valid_ages.name, valid_sexes.name, valid_kps.name, categoryoptioncombouid, dataelementuid) %>%
    dplyr::mutate(indicator_code = stringr::str_replace(indicator_code, "\\.T$", ".T2")) %>%
    dplyr::mutate(indicator_code = stringr::str_replace(indicator_code, "\\.KP\\.T2", ".T2"))

  # Pare down to populated, updated targets only ####
  #blank_cols_idx <- which(names(d$data$Year2) == "")
  d$data$Year2 <- d$data$Year2 %>%
    dplyr::select(tidyselect::any_of(cols_to_keep$indicator_code))


  d$data$Year2 <- d$data$Year2 %>%
    tidyr::pivot_longer(cols = !tidyselect::any_of(header_cols$indicator_code),
                        names_to = "indicator_code",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    dplyr::select(-`Indicator Group`,
                  valid_sexes.name = "Sex",
                  valid_ages.name = "Age",
                  valid_kps.name = "KeyPop") %>%
    dplyr::left_join((datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year)
                      %>% dplyr::select(indicator_code, dataelementuid) %>%
                      dplyr::distinct())) %>%
    #TODO: Need  to sort out how to actually get the definitive UIDs
    dplyr::mutate(dataelementuid = dplyr::case_when(nchar(dataelementuid) == 11 ~ dataelementuid,
                                                        is.na(valid_kps.name) ~ substring(dataelementuid,0,11),
                                                        !is.na(valid_kps.name) ~ stringr::str_extract(dataelementuid, "(?<=\\{KP\\})[A-Za-z][A-Za-z0-9]{10}"),
                                                        TRUE ~ "FOO")) %>%
    dplyr::left_join(de_map_local)


  #No data should have any missing data element uids or category option combo
  #uids at this poinbt
  d <- y2ExtractInvalidDisaggs(d)

  #Create the DATIM export file

  d$datim$year2 <- d$data$Year2 %>%
    dplyr::mutate(orgunit = d$info$country_uids,
                  period = paste0(d$info$cop_year + 1, "Oct"),
                  attributeOptionCombo = default_catOptCombo()) %>%
    dplyr::select(dataElement = dataelementuid,
                  period,
                  orgunit,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo,
                  value = value) %>%
    dplyr::distinct() #TODO: Remove this. We need to be sure we have no duplicates from the join

  d

}
