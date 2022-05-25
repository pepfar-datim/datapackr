#' @export
#' @title Unpack a Data Pack sheet.
#'
#' @description Within a submitted Data Pack (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
unPackDataPackSheet <- function(d, sheet) {
  
  # Check params ----
  params <- check_params(cop_year = d$info$cop_year,
                         tool = d$info$tool,
                         schema = d$info$schema,
                         sheets = sheet)
  
  names(params) <- stringr::str_replace(names(params), "sheets", "sheet")
  
  for (p in names(params)) {
    assign(p, purrr::pluck(params, p))
  }
  
  rm(params, p)
  
  # Read in data ----
  
  header_row <- headerRow(tool = tool, cop_year = cop_year)

  data <- d$sheets[[as.character(sheet)]]
  
  # Make sure no blank column names ####
  data %<>%
    tibble::as_tibble(.name_repair = "unique")

  # Remove duplicate columns (Take the first example) ####
  duplicate_cols <- duplicated(names(data))

  if (any(duplicate_cols)) {
    data <- data[, -which(duplicate_cols)]
  }
  
  # If tab empty or without targets, send d back ----
  target_cols <- schema %>%
    dplyr::filter(sheet_name == sheet
                  & (col_type == "target" | (col_type == "result" & dataset == "subnat"))
                  # Filter by what's in submission to avoid unknown column warning messages
                  & indicator_code %in% colnames(data)) %>%
    dplyr::pull(indicator_code)

  # TODO: Is this the best test for whether a sheet is empty??
  if (NROW(data) == 0 | all(is.na(data$PSNU)) | NROW(target_cols) == 0) {
    return(d)
  }

  # TEST TX_NEW <1 from somewhere other than EID ####
    # TODO: Move this to checkAnalytics
  # if (sheet == "TX") {
  # 
  #   d$tests$tx_new_invalid_lt1_sources <- d$data$extract %>%
  #     dplyr::select(PSNU, Age, Sex, TX_NEW.N.Age_Sex_HIVStatus.T,
  #       TX_NEW.N.IndexRate, TX_NEW.N.TBRate, TX_NEW.N.PMTCTRate,
  #       TX_NEW.N.PostANC1Rate, TX_NEW.N.EIDRate, TX_NEW.N.VMMCRateNew,
  #       TX_NEW.N.prevDiagnosedRate) %>%
  #     dplyr::filter(Age == "<01",
  #                   TX_NEW.N.Age_Sex_HIVStatus.T > 0) %>%
  #     dplyr::filter_at(dplyr::vars(-PSNU, -Age, -Sex, -TX_NEW.N.EIDRate,
  #                                  -TX_NEW.N.Age_Sex_HIVStatus.T),
  #                      dplyr::any_vars(. > 0))
  #   attr(d$tests$tx_new_invalid_lt1_sources, "test_name") <- "Invalid TX <01 data source"
  # 
  # 
  #   if (NROW(d$tests$tx_new_invalid_lt1_sources) > 0) {
  #     warning_msg <-
  #       paste0(
  #         "WARNING! In tab TX",
  #         ": TX_NEW for <01 year olds being targeted through method other than EID.",
  #         " MER Guidance recommends all testing for <01 year olds be performed through EID rather than HTS",
  #         "\n")
  # 
  #     d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  #   }
  # }

  # Add cols to allow compiling with other sheets
  data %<>%
    addcols(c("KeyPop", "Age", "Sex")) %>%
  # Select only target-related columns
    dplyr::select(PSNU, Age, Sex, KeyPop,
                  dplyr::one_of(target_cols)) %>%
  # Drop rows where entire row is NA ----
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>%
  # Extract PSNU uid
    dplyr::mutate(
      psnuid = stringr::str_extract(PSNU, "(?<=(\\(|\\[))([A-Za-z][A-Za-z0-9]{10})(?=(\\)|\\])$)"),
  # Tag sheet name
      sheet_name = sheet
    ) %>%
    dplyr::select(PSNU, psnuid, sheet_name, Age, Sex, KeyPop,
                  dplyr::everything())

  # If PSNU has been deleted, drop the row ----
  data %<>%
    dplyr::filter(!is.na(PSNU))

  # Gather all indicators as single column for easier processing ####
  data %<>%
    tidyr::gather(key = "indicator_code",
                  value = "value",
                  -PSNU, -psnuid, -Age, -Sex, -KeyPop, -sheet_name) %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value)

  # Drop invalid Prioritizations, or maybe revert to prev yr prioritization ----
  if (sheet == "Prioritization") {
    data %<>%
      dplyr::filter(
        !value %in% prioritization_dict()$value,
    # Remove _Military district from Prioritization extract as this can't be assigned a prioritization ####
        !stringr::str_detect(PSNU, "^_Military"),
        value != "NA")
  }

  # Convert non-numeric to numeric ----
  data %<>%
    dplyr::mutate(value = suppressWarnings(as.numeric(value))) %>%
  # Drop NAs and irredeemable non-numerics ----
    tidyr::drop_na(value) %>%
  # Filter out zeros ####
    dplyr::filter(value != 0)

  # Aggregate OVC_HIVSTAT ####
  if (sheet == "OVC") {
    data %<>%
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

  # Add ages to PMTCT_EID ####
  if (sheet == "PMTCT_EID") {
    data %<>%
      dplyr::mutate(
        Age = dplyr::case_when(
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2to12mo") ~ "02 - 12 months",
          stringr::str_detect(indicator_code, "PMTCT_EID(.)+2mo") ~ "<= 02 months",
          TRUE ~ Age
        )
      )
  }

  # Convert Sexes for KP_MAT ####
  if (sheet == "KP") {
    data %<>%
      dplyr::mutate(
        Sex = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T"
            ~ stringr::str_replace(KeyPop, " PWID", ""),
          TRUE ~ Sex),
        KeyPop = dplyr::case_when(indicator_code == "KP_MAT.N.Sex.T" ~ NA_character_,
          TRUE ~ KeyPop)
      )
  }

  return(data)
}
