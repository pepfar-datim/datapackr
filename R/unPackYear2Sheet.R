
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
    #TODO: Re-enable this once the data element /COC map is fixed
    #d$data$Year2 <- d$data$Year2[!year2_invalid_disaggs, ]
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

#' Title pickUIDFromType is a utility function used to obtain a
#' particular UID from a supplied list based on the type of
#' value we are dealing with. In the Year2 tab, values disaggregated
#' by AgeSex, KP and EID are co-mingled and must be separated based
#' on available disaggregates.
#'
#' @param type Disaggregate type (e.g. AgeSex, KP, EID)
#' @param de_uid_list A list of possible UIDs (e.g. c("UID1","{KP}UID2" )
#'
#' @return A UID based on the supplied type. Returns NA if no UID is matched.
#'
pickUIDFromType <- function(type, de_uid_list) {

  uid_regex <- "[A-Za-z][A-Za-z0-9]{10}"
  pick <- NA

  if (is.na(type) || length(de_uid_list) == 0) {
    return(NA)
  }

  if (length(de_uid_list) == 1) {
    pick <- stringr::str_extract(de_uid_list[1],paste0("^",uid_regex,"$"))
  } else {

    if (type == "AgeSex") {
      idx <- which(grepl(paste0("^",uid_regex,"$"), de_uid_list))
      pick <- de_uid_list[idx]
    }

    if (type == "KP") {
      idx <- which(grepl("\\{KP\\}", de_uid_list))
      pick <- stringr::str_extract(de_uid_list[idx],uid_regex)
    }

    if (type == "EID") {
      idx <- which(grepl("\\{EID\\}", de_uid_list))
      pick <- stringr::str_extract(de_uid_list[idx],uid_regex)
    }

  }

  if (is.na(pick)) {
    return(NA)
  }

  if (!is_uidish(pick)) {
     return(NA)
   }

 pick

}

#' Title unpackYear2Sheet
#'
#' @param d
#'
#' @return d
#' @export
#'
unpackYear2Sheet <- function(d) {

  #We will not process any Year2 data for regional data packs
  #Or any datapacks which have more than one country UID.
  if (length(d$info$country_uids) != 1) {
    return(d)
  }

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

  #A map of indicator codes and data element uids

  is_positive <- c("PLHIV|TX_CURR|TX_NEW|TX_PVLS|HTS_TST|HTS_RECENT|CXCA_SCRN|TB_ART|TX_TB|TB_PREV")

  map_ind_code_des <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
    dplyr::filter(indicator_code %in% cols_to_keep$indicator_code) %>%
      dplyr::select(indicator_code,
                    dataelementuid,
                    resultstatus) %>%
    dplyr::mutate(resultstatus = dplyr::case_when(grepl(is_positive, indicator_code) ~ "Positive",
                                                  TRUE ~ resultstatus)) %>%
    #TODO: This likely needs to be moved into the map
    dplyr::mutate(resultstatus = dplyr::case_when(
      indicator_code == "VMMC_CIRC.Neg.T2" ~ "Negative",
      indicator_code == "VMMC_CIRC.Pos.T2" ~ "Positive",
      indicator_code == "VMMC_CIRC.Unk.T2" ~ "Status Unknown",
      indicator_code == "HTS_TST.Index.Pos.Share.T2" ~ "Newly Tested Positives",
      indicator_code == "HTS_TST.Index.Pos.Share.T2" ~ "Newly Tested Positives",
      indicator_code == "HTS_TST.TB.Pos.Share.T2" ~ "Newly Tested Positives",
      indicator_code == "HTS_TST.PMTCT.Pos.Share.T2" ~ "Newly Tested Positives",
      indicator_code == "TB_STAT.N.New.Pos.T2" ~ "Newly Tested Positives",
      indicator_code == "TB_STAT.N.New.Neg.T2" ~ "New Negatives",

      Newly Tested Positives
      TRUE ~ resultstatus)) %>%
      dplyr::distinct()

  #A map of dataelement uids and COCs
  map_year1_des_cocs <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year) %>%
    dplyr::filter(grepl("\\.T$", indicator_code)) %>%
    dplyr::select(dataelementuid,  valid_ages.name,
                  valid_sexes.name, valid_kps.name,resultstatus, categoryoptioncombouid,
                  dataelementname, categoryoptioncomboname ) %>%
    dplyr::distinct()

  d$data$Year2 <- d$data$Year2 %>%
    dplyr::select(tidyselect::any_of(cols_to_keep$indicator_code)) %>%
  #Deal with HTS_TST
    dplyr::mutate(dplyr::across(!header_cols$indicator_code, as.numeric)) %>%
    dplyr::mutate(dplyr::across(tidyselect::contains("Share"), ~ .x * HTS_TST.Pos.Total_With_HEI.T2 )) %>%
  #Pivot longer
    tidyr::pivot_longer(cols = !tidyselect::any_of(header_cols$indicator_code),
                        names_to = "indicator_code",
                        values_to = "value",
                        values_drop_na = TRUE) %>%
    #Drop HTS_TST.POS
    dplyr::filter(indicator_code != "HTS_TST.Pos.Total_With_HEI.T2") %>%
    dplyr::select(-`Indicator Group`,
                  valid_sexes.name = "Sex",
                  valid_ages.name = "Age",
                  valid_kps.name = "KeyPop") %>%
    #OVC_HIVSTAT needs to be aggregated
    dplyr::mutate(
      valid_sexes.name = dplyr::case_when(
        indicator_code == "OVC_HIVSTAT.T2" ~ NA_character_,
        TRUE ~ valid_sexes.name),
      valid_ages.name = dplyr::case_when(
        indicator_code == "OVC_HIVSTAT.T2" ~ NA_character_,
        TRUE ~ valid_ages.name)) %>%
    dplyr::group_by(valid_sexes.name, valid_ages.name, valid_kps.name, indicator_code) %>%
    #TODO: This feels a bit risky. Should we only limit to OVC_HIVSTAT?
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    #Get the raw data element codes from the map
    #We will need to do a bit more processing to determine the actual UID
    #Based on what type of disagg we are dealing with
    dplyr::left_join(map_ind_code_des, by =  c("indicator_code")) %>%
    #Split the data element UID codes in the schema into a nested list
    #Determine the type of value we are dealing with (AgeSex/KP/EID)
    #TODO: How to determine when we need to use the EID data element?
     dplyr::mutate(de_uid_list = stringr::str_split(dataelementuid, "\\."),
                   type = dplyr::case_when(!is.na(valid_kps.name) ~ "KP",
                                           TRUE ~"AgeSex"),
                   dataelementuid = unlist(purrr::map2(type, de_uid_list,pickUIDFromType))) %>%
    dplyr::left_join(map_year1_des_cocs,
                     by = c("dataelementuid", "valid_ages.name", "valid_sexes.name", "valid_kps.name","resultstatus"))
    #TODO: Special handling for OVC_HIVSTAT, which is disaggegated in Year 2, but not in DATIM



  #No data should have any missing data element uids or category option combo
  #uids at this poinbt
  d <- y2ExtractInvalidDisaggs(d)

  #Create the DATIM export file

  d$datim$year2 <- d$data$Year2 %>%
    dplyr::mutate(orgUnit = d$info$country_uids,
                  period = paste0(as.numeric(d$info$cop_year) + 1, "Oct"),
                  attributeOptionCombo = default_catOptCombo()) %>%
    dplyr::select(dataElement = dataelementuid,
                  period,
                  orgUnit,
                  categoryOptionCombo = categoryoptioncombouid,
                  attributeOptionCombo,
                  value = value) %>%
    tidyr::drop_na() %>%  #TODO: Remove this. We should not have any NAs at this point
    dplyr::distinct() #TODO: Remove this. We need to be sure we have no duplicates from the join

  d

}
