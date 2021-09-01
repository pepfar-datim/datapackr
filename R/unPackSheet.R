#' @export
#' @importFrom utils capture.output
#' @title Unpack a Data Pack sheet.
#'
#' @description Within a submitted Data Pack (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to unpack.
#'
#' @return d
#'
unPackDataPackSheet <- function(d, sheet) {
  header_row <- headerRow(tool = "Data Pack", cop_year = d$info$cop_year)

  d$data$extract <-
    readxl::read_excel(
      path = d$keychain$submission_path,
      sheet = sheet,
      range = readxl::cell_limits(c(header_row, 1), c(NA, NA)),
      col_types = "text",
      .name_repair = "minimal"
    )

  # If sheet is totally empty, skip
  if (all(is.na(d$data$extract$PSNU))) {
    d$data$extract <- NULL

    return(d)
  }

  # Run structural checks ####
  d <- checkColStructure(d, sheet)

  # Remove duplicate columns (Take the first example) ####
  duplicate_cols <- duplicated(names(d$data$extract))

  if (any(duplicate_cols)) {
    d$data$extract <- d$data$extract[,-which(duplicate_cols)]
  }

  # Make sure no blank column names ####
  d$data$extract %<>%
    tibble::as_tibble(.name_repair = "unique")

  # if tab has no target related content, send d back
  if (NROW(d$data$extract) == 0) {
    d$data$extract <- NULL
    return(d)
  }

  # TEST TX_NEW <1 from somewhere other than EID ####
    # TODO: Move this to checkAnalytics
  if (sheet == "TX") {

    d$tests$tx_new_invalid_lt1_sources <- d$data$extract %>%
      dplyr::select(PSNU, Age, Sex, TX_NEW.N.Age_Sex_HIVStatus.T,
        TX_NEW.N.IndexRate, TX_NEW.N.TBRate, TX_NEW.N.PMTCTRate,
        TX_NEW.N.PostANC1Rate, TX_NEW.N.EIDRate, TX_NEW.N.VMMCRateNew,
        TX_NEW.N.prevDiagnosedRate) %>%
      dplyr::filter(Age == "<01",
                    TX_NEW.N.Age_Sex_HIVStatus.T > 0) %>%
      dplyr::filter_at(dplyr::vars(-PSNU,-Age,-Sex, -TX_NEW.N.EIDRate,
                                   -TX_NEW.N.Age_Sex_HIVStatus.T),
                       dplyr::any_vars(.>0))
    attr(d$tests$tx_new_invalid_lt1_sources,"test_name")<-"Invalid TX <01 data source"


    if (NROW(d$tests$tx_new_invalid_lt1_sources) > 0) {
      warning_msg <-
        paste0(
          "WARNING! In tab TX",
          ": TX_NEW for <01 year olds being targeted through method other than EID.",
          " MER Guidance recommends all testing for <01 year olds be performed through EID rather than HTS",
          "\n")

      d$info$warning_msg$append(warning_msg,"WARNING")
    }
  }

  # List Target Columns ####
  target_cols <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & (col_type == "target" | (col_type == "result" & dataset == "subnat"))
  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(d$data$extract)) %>%
    dplyr::pull(indicator_code)

  if (NROW(target_cols) == 0) {
    d$data$extract <- NULL
    return(d)
  }

  # Add cols to allow compiling with other sheets ####
  d$data$extract %<>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
  # Select only target-related columns
    dplyr::select(PSNU, Age, Sex, KeyPop,
                  dplyr::one_of(target_cols)) %>%
  # Drop rows where entire row is NA
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
    # Extract PSNU uid
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"),
      # Tag sheet name
      sheet_name = sheet
    ) %>%
    dplyr::select(PSNU, psnuid, sheet_name, Age, Sex, KeyPop,
                  dplyr::everything())

  # TEST: No missing metadata ####
  d <- checkMissingMetadata(d, sheet)

  # If PSNU has been deleted, drop the row ####
  d$data$extract %<>%
    dplyr::filter(!is.na(PSNU))

  # TEST AGYW Tab for missing DSNUs ####
  if (sheet == "AGYW") {
    DataPack_DSNUs <- d$data$extract %>%
      dplyr::select(PSNU, psnu_uid = psnuid) %>%
      dplyr::distinct() %>%
      dplyr::mutate(DataPack = 1)

    DATIM_DSNUs <- datapackr::valid_PSNUs %>%
      dplyr::filter(country_uid %in% d$info$country_uids) %>%
      add_dp_psnu(.) %>%
      dplyr::arrange(dp_psnu) %>%
      dplyr::filter(!is.na(DREAMS)) %>%
      dplyr::select(PSNU = dp_psnu, psnu_uid, snu1) %>%
      dplyr::mutate(DATIM = 1)

    DSNU_comparison <- DataPack_DSNUs %>%
      dplyr::full_join(DATIM_DSNUs, by = "psnu_uid")

    d$tests$DSNU_comparison <- DSNU_comparison
    attr(d$tests$DSNU_comparison,"test_name") <- "DSNU List Comparison"

    if (any(is.na(DSNU_comparison$DataPack))) {
      missing_DSNUs <- DSNU_comparison %>%
        dplyr::filter(is.na(DataPack))

      warning_msg <- paste0(
        "WARNING! In tab ",
        sheet,
        ": MISSING DREAMS SNUs found! ->  \n\t* ",
        paste(missing_DSNUs$PSNU.y, collapse = "\n\t* "),
        "\n")

      d$info$warning_msg$append(warning_msg,"WARNING")
      d$info$missing_DSNUs <- TRUE
    }

    if (any(is.na(DSNU_comparison$DATIM))) {
      invalid_DSNUs <- DSNU_comparison %>%
        dplyr::filter(is.na(DATIM))

      warning_msg <- paste0(
        "WARNING! In tab ",
        sheet,
        ": INVALID DREAMS SNUs found! ->  \n\t* ",
        paste(invalid_DSNUs$PSNU.x, collapse = "\n\t* "),
        "\n")

      d$info$warning_msg$append(warning_msg,"WARNING")
    }

  }

  # Check for Formula changes ####
  d <- checkFormulas(d, sheet)

  # Gather all indicators as single column for easier processing ####
  d$data$extract %<>%
    tidyr::gather(key = "indicator_code",
                  value = "value",
                  -PSNU, -psnuid, -Age, -Sex, -KeyPop, -sheet_name) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value)

  # TEST that all Prioritizations completed ####
  if (sheet == "Prioritization") {

    # Remove _Military district from Prioritization extract as this can't be assigned a prioritization ####
    d$data$extract %<>%
      dplyr::filter(!stringr::str_detect(PSNU, "^_Military"),
                    # Excuse valid NA Prioritizations
                    value != "NA")

    blank_prioritizations <- d$data$extract %>%
      dplyr::filter(is.na(value)) %>%
      dplyr::select(PSNU)

    if (NROW(blank_prioritizations) > 0) {

      d$tests$blank_prioritizations <- blank_prioritizations
      attr(d$tests$blank_prioritizations ,"test_name") <- "Blank prioritization levels"

      warning_msg <-
        paste0(
          "ERROR! In tab ",
          sheet,
          ": MISSING PRIORITIZATIONS. Ensure a prioritization value is entered in each",
          " row of the column labeled 'SNU Prioritization' on the Prioritization tab.",
          " Refer to guidance on that tab and in the Data Pack User Guide to see",
          " appropriate entry options. You must enter a prioritization value for",
          " the following PSNUs -> \n\t* ",
          paste(blank_prioritizations$PSNU, collapse = "\n\t* "),
          "\n")

      d$info$warning_msg$append(warning_msg,"WARNING")
      d$info$has_error <- TRUE

    }

    # Test for valid priortization values
    invalid_prioritizations <- d$data$extract %>%
      dplyr::filter(!(value %in% c("1","2","4","5","6","7","8")) )


    if (NROW(invalid_prioritizations) > 0) {
      d$tests$invalid_prioritizations <- invalid_prioritizations
      attr(d$tests$invalid_prioritizations,"test_name")<-"Invalid prioritizations"

      invalid_prioritizations_strings <- invalid_prioritizations %>%
        tidyr::unite(row_id, c(PSNU, value), sep = ":  ") %>%
        dplyr::arrange(row_id) %>%
        dplyr::pull(row_id)

      warning_msg <-
        paste0(
          "ERROR! In tab ",
          sheet,
          ": INVALID PRIORITIZATIONS. The following Prioritizations are not valid for",
          " the listed PSNUs. Review the guidance on the Prioritization tab and in the",
          " Data Pack User Guide to understand valid prioritization options. Refer to those",
          " PSNUs flagged by this check and correct their validation values in the 'SNU Prioritization'",
          " column on the Prioritization tab. -> \n\t* ",
          paste(invalid_prioritizations_strings, collapse = "\n\t* "),
          "\n")

      d$info$warning_msg$append(warning_msg, "ERROR")
      d$info$has_error <- TRUE
    }

  }

  # TODO: Update this test to drop invalid Prioritizations, or maybe revert to prev yr prioritization

  # Drop NAs ####
  d$data$extract %<>%
    tidyr::drop_na(value)

  # TEST for non-numeric values ####
  # TODO: Update to use checkNumericValues instead
  non_numeric <- d$data$extract %>%
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
    dplyr::mutate(sheet=sheet)

  d$tests$non_numeric<-dplyr::bind_rows(d$tests$non_numeric,non_numeric)
  attr(d$tests$non_numeric,"test_name")<-"Non-numeric values"

  if(NROW(non_numeric) > 0) {

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

    d$info$warning_msg$append(warning_msg,"WARNING")
  }

  # Now that non-numeric cases noted, convert all to numeric & drop non-numeric ####
  d$data$extract %<>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
    tidyr::drop_na(value) %>%
  # Filter out zeros ####
    dplyr::filter(value != 0)

  # TEST: No invalid org units ####
  d <- checkInvalidOrgUnits(d, sheet)

  # TEST for Negative values ####
  negative_values <- d$data$extract %>%
    dplyr::filter(value < 0)

  d$tests$negative_values<-dplyr::bind_rows(d$test$negative_values,negative_values)
  attr(d$tests$negative_values,"test_name")<-"Negative values"

  if ( NROW(negative_values) > 0  ) {

    warning_msg <-
      paste0(
        "ERROR! In tab ",
        sheet,
        ": NEGATIVE VALUES found in the following columns! Ensure all values entered",
        " against FY22 Targets are whole, positive, numeric values. These will be removed. -> \n\t* ",
        paste(unique(d$tests$negative_values$indicator_code), collapse = "\n\t* "),
        "\n")

    d$info$warning_msg$append(warning_msg,"WARNING")
    d$info$has_error <- TRUE
  }

  # TEST for Decimal values ####
  decimals_allowed <- d$info$schema %>%
    dplyr::filter(sheet_name == sheet
                  & col_type == "target"
                  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% unique(d$data$extract$indicator_code)
                  & value_type == "percentage") %>%
    dplyr::pull(indicator_code)

  decimal_cols <- d$data$extract %>%
    dplyr::filter(value %% 1 != 0
                  & !indicator_code %in% decimals_allowed) %>%
    dplyr::rename(sheet = sheet_name)

    d$tests$decimal_values<-dplyr::bind_rows(d$tests$decimal_cols,decimal_cols)
    attr(d$tests$decimal_values,"test_name")<-"Decimal values"

  if (NROW(decimal_cols) > 0) {

    warning_msg <-
      paste0(
        "WARNING! In tab ",
        sheet,
        ": DECIMAL VALUES found in the following columns! Ensure all values entered",
        " against FY22 Targets are whole, positive, numeric values. (The only exception",
        " to this rule may be HIV_PREV.) These will be rounded. -> \n\t* ",
        paste(unique(decimal_cols$indicator_code), collapse = "\n\t* "),
        "\n")

    d$info$warning_msg$append(warning_msg,"WARNING")
  }

  # TEST for duplicates ####
  d <- checkDuplicateRows(d, sheet)

  # TEST for defunct disaggs ####
  d <- defunctDisaggs(d, sheet)

  # Aggregate OVC_HIVSTAT
  if (sheet == "OVC") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Age),
        Sex = dplyr::case_when(
          stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
          TRUE ~ Sex)) %>%
      dplyr::group_by(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }

  # Add ages to PMTCT_EID
  if (sheet == "PMTCT_EID") {
    d$data$extract %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
          TRUE ~ Age
        )
      )
  }

  if (sheet == "KP") {
    d$data$extract %<>%
      dplyr::mutate(
        Sex = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T"
            ~ stringr::str_replace(KeyPop, " PWID", ""),
          TRUE ~ Sex),
        KeyPop = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T" ~ NA_character_,
          TRUE ~ KeyPop)
      )
  }

  return(d)
}
