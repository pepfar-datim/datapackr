#' @export
#' @title Unpack a Data Pack sheet.
#'
#' @description Within a submitted Data Pack (directed to by
#'    \code{d$keychain$submission_path}), extract data from a single sheet specified
#'    in \code{d$data$sheet}.
#'
#' @inheritParams datapackr_params
#' @param clean_orgs Logical. If TRUE, drops rows with missing or invalid org
#' units.
#' @param clean_disaggs Logical. If TRUE, drops rows with missing or incorrect
#' Age, Sex, or KeyPop.
#' @param clean_values Logical. If TRUE, will convert values to numeric type,
#' drop non-numerics, NAs, & negatives; and aggregate all data across duplicate
#' rows.
#'
#' @return d
#'
unPackDataPackSheet <- function(d,
                                sheet,
                                clean_orgs = TRUE,
                                clean_disaggs = TRUE,
                                clean_values = TRUE) {

  # Read in data ----
  data <- d$sheets[[as.character(sheet)]]

  # Remove duplicate columns (Take 1st) ####
  duplicate_cols <- duplicated(names(data))

  if (any(duplicate_cols)) {
    data <- data[, -which(duplicate_cols)]
  }

  # Make sure no blank column names ####
  data %<>%
    tibble::as_tibble(.name_repair = "unique")

  # If tab empty or without targets, send d back ----
  target_cols <- d$info$schema %>%
    dplyr::filter(
      sheet_name == sheet
      & (col_type == "target" | (col_type == "result" & dataset == "subnat"))
      # Filter by what's in submission to avoid unknown column warning messages
      & indicator_code %in% colnames(data)) %>%
    dplyr::pull(indicator_code)

  data %<>%
    # Add cols to allow compiling with other sheets ----
    addcols(c("KeyPop", "Age", "Sex")) %>%
    dplyr::mutate(
      psnuid = extract_uid(PSNU), # Extract PSNU uid ----
      sheet_name = sheet) %>% # Tag sheet name ----
    # Select only target-related columns ----
    dplyr::select(PSNU, psnuid, sheet_name, Age, Sex, KeyPop,
                  dplyr::one_of(target_cols)) %>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.))) %>% # Drop if entire row NA ----
    # Gather all indicators as single column for easier processing ----
    tidyr::pivot_longer(cols = -c(PSNU, psnuid, Age, Sex, KeyPop, sheet_name),
                        names_to = "indicator_code",
                        values_to = "value") %>%
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value)

  #TODO: Decide whether to map PMTCT_EID ages now or later.

  # Munge ----
  if (clean_orgs) {
    data %<>%
      dplyr::filter(
        psnuid %in% valid_PSNUs$psnu_uid) # Drop if invalid or blank org unit ----
  }

  if (clean_disaggs) {
    valid_disaggs <- d$info$schema %>%
      dplyr::filter(
        sheet_name == sheet
        & (col_type == "target" | (col_type == "result" & dataset == "subnat"))) %>%
      dplyr::select(indicator_code, valid_ages, valid_sexes, valid_kps)

    data %<>% # Drop invalid disaggs (Age, Sex, KeyPop) ----
      dplyr::left_join(valid_disaggs, by = c("indicator_code" = "indicator_code")) %>%
      dplyr::filter(purrr::map2_lgl(Age, valid_ages, ~.x %in% .y[["name"]])
                    & purrr::map2_lgl(Sex, valid_sexes, ~.x %in% .y[["name"]])
                    & purrr::map2_lgl(KeyPop, valid_kps, ~.x %in% .y[["name"]])) %>%
      dplyr::select(-valid_ages, -valid_sexes, -valid_kps)

    # Aggregate OVC_HIVSTAT ####
    if (sheet == "OVC") {
      data %<>%
        dplyr::mutate(
          Age = dplyr::case_when(
            stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
            TRUE ~ Age),
          Sex = dplyr::case_when(
            stringr::str_detect(indicator_code, "OVC_HIVSTAT") ~ NA_character_,
            TRUE ~ Sex))
    }
  }

  if (clean_values) {
    data %<>%
      dplyr::mutate(
        value = suppressWarnings(as.numeric(value))) %>% # Convert to numeric ----
      tidyr::drop_na(value) # Drop NAs & non-numerics ----

    # Clean Prioritizations ----
    if (sheet == "Prioritization") {
      data %<>%
        dplyr::filter(
          !stringr::str_detect(PSNU, "^_Military"),
          value %in% prioritization_dict()$value)
    } else {
      data %<>%
        dplyr::filter(value > 0)# Filter out zeros & negatives ----
    }

    # Aggregate (esp for OVC_HIVSTAT) ----
    data %<>%
      dplyr::group_by(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
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

  return(data)
}
