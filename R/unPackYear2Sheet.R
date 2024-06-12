y2ExtractDuplicateRows <- function(d) {
  duplicated_rows <- d$data$Year2 %>%
    dplyr::select(-value) %>%
    duplicated()

  if (any(duplicated_rows)) {
    warning_msg <-
      paste0(
        "WARNING! Duplicated rows were detected in your Year 2 tab.",
        "These will be dropped. Consult the validation test report ",
        "for specific details. \n"
      )

    d$tests$year2_duplicated_rows <- d$data$Year2 %>%
      dplyr::filter(duplicated_rows)
    attr(d$tests$year2_duplicated_rows, "test_name") <-
      "Duplicated Year 2 rows"
    d$info$messages <-
      appendMessage(d$info$messages, warning_msg, "WARNING")
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
        "\n"
      )

    d$info$messages <-
      appendMessage(d$info$messages, warning_msg, "WARNING")
    d$tests$year2_invalid_disaggs  <-
      d$data$Year2[year2_invalid_disaggs, ]
    attr(d$tests$year2_invalid_disaggs, "test_name") <- "Invalid Year 2 disaggs"

    d$data$Year2 <- d$data$Year2[!year2_invalid_disaggs, ]
  }

  d

}

y2TestColumnStructure <- function(d) {
  expected_cols <-
    d$info$schema %>%
    dplyr::filter(sheet_name == "Year 2") %>%
    dplyr::select(col, indicator_code)

  actual_cols <-
    data.frame(indicator_code = names(d$data$Year2)) %>%
    dplyr::mutate(actual_col = dplyr::row_number())

  cols_compare <-
    actual_cols %>%
    dplyr::full_join(expected_cols, by = "indicator_code") %>%
    dplyr::mutate(is_equal = identical(actual_col, col))

  if (!any(cols_compare$is_equal)) {
    warning_msg <-
      paste0(
        "WARNING! Columns in the Year 2 tab are missing or out of order.",
        "We will attempt to proceed with validation, however this must be fixed",
        " prior to final submission.",
        "Consult the validation report for details.",
        "\n"
      )


    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")

    d$tests$year2_cols_out_of_order  <-
      cols_compare %>% dplyr::filter(!is_equal)
    attr(d$tests$year2_cols_out_of_order, "test_name") <-
      "Invalid Year 2 column order"

  }

  d

}

y2NegativeValues <- function(d) {

  neg_values <- d$data$Year2 %>%
    dplyr::filter(value < 0)

  if (NROW(neg_values) > 0) {
    warning_msg <-
      paste0(
        "WARNING! Negative values found in the Year 2 sheet!",
        "\n"
      )

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    d$tests$year2_negative_values  <-
      neg_values %>%
      dplyr::select(indicator_code,
                    Sex = valid_sexes.name,
                    Age = valid_ages.name,
                    KP = valid_kps.name,
                    value)
    attr(d$tests$year2_negative_values, "test_name") <- "Year 2 negative values"

      }


    d
}

y2DecimalValues <- function(d) {


  decimal_values <- d$data$Year2 %>%
    dplyr::filter(value %% 1 != 0)

  if (NROW(decimal_values) > 0) {
    warning_msg <-
      paste0(
        "WARNING! Decimal values found in the Year 2 sheet! These will be rounded.",
        "\n"
      )

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    d$tests$year2_decimal_values <-
      decimal_values %>%
      dplyr::select(indicator_code,
                    Sex = valid_sexes.name,
                    Age = valid_ages.name,
                    KP = valid_kps.name,
                    value)
    attr(d$tests$year2_decimal_values, "test_name") <- "Year 2 decimal values"

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
    pick <-
      stringr::str_extract(de_uid_list[1], paste0("^", uid_regex, "$"))
  } else {
    if (type == "AgeSex") {
      idx <- which(grepl(paste0("^", uid_regex, "$"), de_uid_list))
      pick <- de_uid_list[idx]
    }

    if (type == "KP") {
      idx <- which(grepl("\\{KP\\}", de_uid_list))
      pick <- stringr::str_extract(de_uid_list[idx], uid_regex)
    }

    if (type == "EID") {
      idx <- which(grepl("\\{EID\\}", de_uid_list))
      pick <- stringr::str_extract(de_uid_list[idx], uid_regex)
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

generateY2IndicatorCodeDataElementMap <- function(cols_to_keep, cop_year) {

  hts_recent_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "HTS_RECENT.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "HTS_RECENT.T2",
                  dataelementuid = "SsqfZeIs1Va.{KP}y42bZItNsea")

  tx_new_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "TX_NEW.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "TX_NEW.T2",
                  dataelementuid = "vmfKLKi1NBA.{KP}ktZYUSS0Zjo")

  tx_curr_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "TX_CURR.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "TX_CURR.T2",
                  dataelementuid = "di4b6joXm84.{KP}OLbhrUez4dP")

  tx_curr_subnat_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "TX_CURR_SUBNAT.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "TX_CURR.T2",
                  dataelementuid = "di4b6joXm84.{KP}OLbhrUez4dP")

  tx_pvls_d_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "TX_PVLS.D.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "TX_PVLS.D.Routine.T2",
                  dataelementuid = "WpZwPieQ060.{KP}P9OjdVVMHMW")

  tx_pvls_n_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "TX_PVLS.N.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "TX_PVLS.N.Routine.T2",
                  dataelementuid = "N55pM5ZuWcI.{KP}tGtB1nLnQjC")

  prep_new_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "PrEP_NEW.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "PrEP_NEW.T2",
                  dataelementuid = "rXV784LlUQ4.{KP}lXqlw8UxoqF")


  prep_ct_kps <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "PrEP_CT.KP.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "PrEP_CT.T2",
                  dataelementuid = "agoURWZyPpn.{KP}bwlf1Jfww0L")


  #TB_STAT.KnownPos.T2 seems to be missing entirely from the map
  tb_stat_known_pos_t2 <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(support_type == "DSD") %>%
    dplyr::filter(indicator_code == "TB_STAT.N.KnownPos.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(indicator_code = "TB_STAT.N.KnownPos.T2")

  pmtct_eid <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(dataelementuid == "euzbW4INAqn" &
                    indicator_code == "PMTCT_EID.N.2.T") %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    )


  is_positive <-
    c("PLHIV|TX_CURR|TX_NEW|TX_PVLS|HTS_TST|HTS_RECENT|CXCA_SCRN|HTS_RECENT")


   map_ind_code_des <-
    datapackr::getMapDataPack_DATIM_DEs_COCs(cop_year) %>%
    dplyr::filter(indicator_code %in% cols_to_keep$indicator_code) %>%
    dplyr::select(
      indicator_code,
      valid_ages.name,
      valid_sexes.name,
      valid_kps.name,
      dataelementuid,
      resultstatus,
      disagg_type
    ) %>%
    dplyr::mutate(resultstatus = dplyr::case_when(
      grepl(is_positive, indicator_code) ~ "Positive",
      TRUE ~ resultstatus
    )) %>%

    dplyr::mutate(
      resultstatus = dplyr::case_when(
        indicator_code == "VMMC_CIRC.Neg.T2" ~ "Negative",
        indicator_code == "VMMC_CIRC.Pos.T2" ~ "Positive",
        indicator_code == "VMMC_CIRC.Unk.T2" ~ "Status Unknown",
        indicator_code == "HTS_TST.Index.Pos.Share.T2" ~ "Newly Tested Positives",
        indicator_code == "HTS_TST.TB.Pos.Share.T2" ~ "Newly Tested Positives",
        indicator_code == "HTS_TST.PMTCT.Pos.Share.T2" ~ "Newly Tested Positives",
        indicator_code == "TB_STAT.N.New.Pos.T2" ~ "Newly Tested Positives",
        indicator_code == "TB_STAT.N.New.Neg.T2" ~ "New Negatives",
        indicator_code == "TB_STAT.N.KnownPos.T" ~ "Known Positives",
        indicator_code == "PMTCT_STAT.N.KnownPos.T2" ~ "Known Positives",
        indicator_code == "PMTCT_STAT.N.New.Neg.T2" ~ "New Negatives",
        indicator_code == "PMTCT_STAT.N.New.Pos.T2" ~ "Newly Tested Positives",
        indicator_code == "PMTCT_ART.Already.T2" ~ "Known Positives",
        indicator_code == "PMTCT_ART.New.T2" ~  "Newly Tested Positives",
        indicator_code == "TB_PREV.D.New.T2" ~ "Newly Tested Positives",
        indicator_code == "TB_PREV.D.Already.T2" ~ "Known Positives",
        indicator_code == "TB_PREV.N.New.T2" ~ "Newly Tested Positives",
        indicator_code == "TB_PREV.N.Already.T2" ~ "Known Positives",
        indicator_code == "TB_ART.New.T2" ~ "Newly Tested Positives",
        indicator_code == "TB_ART.Already.T2" ~  "Known Positives",
        indicator_code == "TB_STAT.N.New.Pos.T2" ~ "Newly Tested Positives",
        indicator_code == "TB_STAT.N.New.Neg.T2" ~ "New Negatives",
        indicator_code == "TB_STAT.N.KnownPos.T2" ~ "Known Positives",
        indicator_code == "TX_TB.D.Already.Neg.T2" ~ "TB Screen - Negative, Life-long ART, Already, Positive",
        indicator_code == "TX_TB.D.Already.Pos.T2" ~ "TB Screen - Positive, Life-long ART, Already, Positive",
        indicator_code == "TX_TB.D.New.Neg.T2" ~ "TB Screen - Negative, Life-long ART, New, Positive",
        indicator_code == "TX_TB.D.New.Pos.T2" ~ "TB Screen - Positive, Life-long ART, New, Positive",
        indicator_code == "OVC_SERV.Active.T2" &
          dataelementuid == "HVzzfyVVIs1" ~ "Active, Beneficiary",
        indicator_code == "OVC_SERV.Active.T2" &
          dataelementuid == "cx8hxarh4Ke" ~ "Active, Caregiver",
        indicator_code == "OVC_SERV.Grad.T2" &
          dataelementuid == "HVzzfyVVIs1" ~ "Graduated, Beneficiary",
        indicator_code == "OVC_SERV.Grad.T2" &
          dataelementuid == "cx8hxarh4Ke" ~ "Graduated, Caregiver",
        indicator_code == "GEND_GBV.PE.T2" ~ "Physical and/or Emotional Violence",
        indicator_code == "GEND_GBV.S.T2" ~ "Sexual Violence (Post-Rape Care)",
        TRUE ~ resultstatus
      )
    ) %>%
    dplyr::mutate(
      disagg_type = dplyr::case_when(
        indicator_code == "PLHIV.T2" ~ "Age/Sex/HIVStatus",
        indicator_code == "TX_PVLS.D.Routine.T2" ~ "Age/Sex/Indication/HIVStatus",
        indicator_code == "TX_PVLS.N.Routine.T2" ~ "Age/Sex/Indication/HIVStatus",
        indicator_code == "TX_CURR.T2" ~ "Age/Sex/HIVStatus",
        indicator_code == "TX_NEW.T2" ~ "Age/Sex/HIVStatus",
        indicator_code == "HTS_RECENT.T2" ~ "Age/Sex/HIVStatus",
        indicator_code == "HTS_SELF.T2" ~ "Age/Sex",
        indicator_code == "PrEP_NEW.T2" ~ "Age/Sex",
        indicator_code == "PrEP_CT.T2"  ~ "Age/Sex",
        TRUE ~ disagg_type
      )
    ) %>%
    #Deal with OVC_HIVSTAT. In the DP the data is disaggregated by age
    #However, in DATIM there is no disaggregation
    dplyr::mutate(
      valid_sexes.name = dplyr::case_when(
        indicator_code == "OVC_HIVSTAT.T2" ~ NA_character_,
        TRUE ~ valid_sexes.name
      )
    ) %>%
    dplyr::mutate(
      valid_ages.name = dplyr::case_when(
        indicator_code == "OVC_HIVSTAT.T2" ~ NA_character_,
        TRUE ~ valid_ages.name
      )
    ) %>%
    dplyr::bind_rows(
      hts_recent_kps,
      tx_new_kps,
      tx_curr_kps,
      tx_pvls_d_kps,
      tx_pvls_n_kps,
      tb_stat_known_pos_t2,
      prep_new_kps,
      prep_ct_kps,
      pmtct_eid
    ) %>%
    dplyr::distinct()
}

#' Title unpackYear2Sheet
#'
#' @inheritParams datapackr_params
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

  header_row <-
    headerRow(tool = d$info$tool,
              cop_year = d$info$cop_year)

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

  Year2_KP_stacked <- pick_schema(d$info$cop_year, d$info$tool) %>%
    dplyr::filter(sheet_name == "Year 2",
                  stringr::str_detect(dataelement_dsd, "\\{KP\\}"))

  d$data$Year2 %<>%
    dplyr::select(tidyselect::any_of(cols_to_keep$indicator_code)) %>%
    #Deal with HTS_TST
    dplyr::mutate(dplyr::across(!header_cols$indicator_code, as.numeric)) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::contains("Share"),
      ~ round(.x * HTS_TST.Pos.Total_With_HEI.T2)
    )) %>%
    #Pivot longer
    tidyr::pivot_longer(
      cols = !tidyselect::any_of(header_cols$indicator_code),
      names_to = "indicator_code",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    #Drop HTS_TST.POS but keep the under 1s and map it to PMTCT_EID <2 months
    # nolint start
    dplyr::mutate(
      indicator_code = dplyr::case_when(
        indicator_code == "HTS_TST.Pos.Total_With_HEI.T2"
          & Age == "<01"
          ~ paste0(indicator_code, ".EID.2"),
        indicator_code %in% Year2_KP_stacked$indicator_code
          & !is.na(KeyPop)
          ~ paste0(indicator_code, (".KP")),
        TRUE ~ indicator_code)) %>%
    # nolint end
    # Allow mapping of EID
    # OVC_HIVSTAT needs to be aggregated
    dplyr::mutate(
      Age = dplyr::case_when(
        indicator_code %in% c("HTS_TST.Pos.Total_With_HEI.T2.EID.2", "OVC_HIVSTAT.T2")
          ~ NA_character_,
        TRUE ~ Age),
      Sex = dplyr::case_when(
        indicator_code %in% c("HTS_TST.Pos.Total_With_HEI.T2.EID.2", "OVC_HIVSTAT.T2")
          ~ NA_character_,
        TRUE ~ Sex)) %>%
    #Get rid of the HTS_TST Pos at this point
    dplyr::filter(indicator_code != "HTS_TST.Pos.Total_With_HEI.T2") %>%
    #Drop TX_CURR_SUBNAT data for KPs. It should not be there.
    dplyr::filter(!(indicator_code == "TX_CURR_SUBNAT.T2" &
                      !is.na(KeyPop))) %>%
    dplyr::select(
      -`Indicator Group`,
      valid_sexes.name = "Sex",
      valid_ages.name = "Age",
      valid_kps.name = "KeyPop"
    ) %>%
    dplyr::group_by(valid_sexes.name,
                    valid_ages.name,
                    valid_kps.name,
                    indicator_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  #Data tests once we have the final shape
  d <- y2NegativeValues(d)
  d <- y2DecimalValues(d)


  start_rows <- NROW(d$data$Year2)
  # Get the raw data element codes from the map ----
  d$data$Year2 %<>%
    dplyr::left_join(
      getMapDataPack_DATIM_DEs_COCs(d$info$cop_year, year = 2),
      by = c(
        "indicator_code",
        "valid_ages.name",
        "valid_sexes.name",
        "valid_kps.name"))

  new_rows <- NROW(d$data$Year2)
  if (start_rows != new_rows) {
    warning("Rows were duplicated in a join.")
  }

  #No data should have any missing data element uids or category option combo
  #uids at this poinbt
  d <- y2ExtractInvalidDisaggs(d)

  # Create the DATIM export file ----
  d$datim$year2 <- d$data$Year2 %>%
    dplyr::mutate(
      orgUnit = d$info$country_uids,
      attributeOptionCombo = default_catOptCombo()
    ) %>%
    dplyr::select(
      dataElement = dataelementuid,
      period,
      orgUnit,
      categoryOptionCombo = categoryoptioncombouid,
      attributeOptionCombo,
      value = value
    ) %>%
    tidyr::drop_na() %>%
    dplyr::distinct()

  export_dups_vec <- duplicated(d$datim$year2[, 1:5])

  if (any(export_dups_vec)) {
    warning("Removing duplicates in final Year2 export.")
  }

  d$datim$year2 <- d$datim$year2[!export_dups_vec, ]

  d

}
