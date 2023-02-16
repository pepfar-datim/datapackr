
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


    d$tests$year2_invalid_disaggs  <- d$data$Year2[year2_invalid_disaggs, ]

    d$data$Year2 <- d$data$Year2[!year2_invalid_disaggs, ]
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


  cols_to_keep <- getColumnsToKeep(d, sheet)
  header_cols <- getHeaderColumns(cols_to_keep, sheet)

  d$data$Year2 <- d$sheets$`Year 2`
  # Pare down to populated, updated targets only ####
  blank_cols_idx <- which(names(d$data$Year2) == "")
  d$data$Year2 <- d$data$Year2[, cols_to_keep$col]
  d$data$Year2 <- d$data$Year2[!(names(d$data$Year2) %in% c(""))]

  #TODO: Test column structure

  d$data$Year2 <- d$data$Year2 %>%
    tidyr::pivot_longer(cols = !tidyselect::any_of(header_cols$indicator_code),
                        names_to = "indicator_code",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    dplyr::select(-`Indicator Group`,
                  valid_sexes.name = "Sex",
                  valid_ages.name = "Age",
                  valid_kps.name = "KeyPop")

  #Drop duplicated rows
  d <- y2ExtractDuplicateRows(d)

  #Join with DE/COC map
  d$data$Year2 %<>%
    dplyr::left_join(datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year))

  #TODO: Seems like MANY year2 data element /COC are not complete in the map
  #TODO: Seems like this join produces duplicate rows.
  #Test for invalid disaggs.

  #TODO: What's up with the data element uids?
  d$data$Year2 %>%
    dplyr::mutate(dataelementuid_new = stringr::str_extract(
      dataelementuid,
      ifelse(
        is.na(valid_kps.name),
        "^[A-Za-z][A-Za-z0-9]{10}",
        "(?=\\{KP\\})[A-Za-z][A-Za-z0-9]{10}$"
      )
    ))

  d <- y2ExtractInvalidDisaggs(d)

  #Create the DATIM export file

  d$datim$year2 <- d$data$Year2 %>%
    dplyr::mutate(orgunit = d$info$country_uids,
                  attributeOptionCombo = default_catOptCombo()) %>%
    dplyr::select(dataElement = dataelementuid,
                  period = period,
                  orgunit,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo,
                  value = value) %>%
    dplyr::distinct() #TODO: Remove this. We need to be sure we have no duplicates from the join

  d

}
