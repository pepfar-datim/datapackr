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
                                sheets,
                                clean_orgs = TRUE,
                                clean_disaggs = TRUE,
                                clean_values = TRUE) {

  keep_cols <- d$info$schema %>%
    dplyr::filter(
      sheet_name %in% sheets,
      !indicator_code %in% c("SNU1", "ID"),
      col_type %in% c("row_header", "target")) %>%
    dplyr::select(sheet_name, indicator_code, col_type,
                  valid_ages, valid_sexes, valid_kps) %>%
    tidyr::unnest(valid_ages, names_sep = ".") %>%
    tidyr::unnest(valid_sexes, names_sep = ".") %>%
    tidyr::unnest(valid_kps, names_sep = ".") %>%
    dplyr::select(sheet_name, indicator_code, col_type,
                  Age = valid_ages.name, Sex = valid_sexes.name,
                  KeyPop = valid_kps.name)

  header_cols <- keep_cols %>%
    dplyr::filter(col_type == "row_header")

  data <- d$sheets[names(d$sheets) %in% sheets] %>%
    purrr::map2_dfr(
      .,
      names(.),
      function(x, y) x %>%
        # Select only target-related columns ----
      # tidyselect::any_of removes duplicates (takes 1st), ignores blank col names
      dplyr::select(
        tidyselect::any_of(
          unique(keep_cols$indicator_code[keep_cols$sheet_name == y]))) %>%
        tidyr::pivot_longer(
          cols = -tidyselect::any_of(
            c(unique(header_cols$indicator_code[header_cols$sheet_name == y]))),
          names_to = "indicator_code",
          values_to = "value") %>%
        tibble::add_column(sheet_name = y)) %>% # Tag sheet name ----
  # Add cols to allow compiling with other sheets ----
    addcols(c("KeyPop", "Age", "Sex")) %>%
    dplyr::mutate(psnuid = extract_uid(PSNU)) %>% # Extract PSNU uid ----
    dplyr::select(PSNU, psnuid, sheet_name, indicator_code, Age, Sex, KeyPop, value) %>%
    tidyr::drop_na(value)

  #TODO: Decide whether to map PMTCT_EID ages now or later.

  # Munge ----
  if (clean_orgs) {
    data %<>%
      dplyr::filter(
        psnuid %in% valid_PSNUs$psnu_uid) # Drop if invalid or blank org unit ----
  }

  if (clean_disaggs) {
    # Drop invalid disaggs (Age, Sex, KeyPop) ----
    data %<>%
      dplyr::semi_join(keep_cols,
                       by = c("indicator_code", "sheet_name", "Age", "Sex", "KeyPop"))

    # Aggregate OVC_HIVSTAT ####
    if ("OVC" %in% sheets) {
      data %<>%
        dplyr::mutate(
          Age = dplyr::case_when(
            sheet_name == "OVC" & indicator_code == "OVC_HIVSTAT.T" ~ NA_character_,
            TRUE ~ Age),
          Sex = dplyr::case_when(
            sheet_name == "OVC" & indicator_code == "OVC_HIVSTAT.T" ~ NA_character_,
            TRUE ~ Sex))
    }
  }

  if (clean_values) {
    data %<>%
      dplyr::mutate(
        value = suppressWarnings(as.numeric(value))) %>% # Convert to numeric ----
      tidyr::drop_na(value) # Drop NAs & non-numerics ----

    # Clean Prioritizations ----
    data %<>%
      dplyr::filter(
        (sheet_name == "Prioritization"
          & stringr::str_sub(PSNU, 1, 9) != "_Military"
          & value %in% prioritization_dict()$value)
    # Drop value <= 0 (other than 0 prioritization) ---
        | (sheet_name != "Prioritization" & value > 0))

    # Aggregate (esp for OVC_HIVSTAT) (except prioritizations) ----
    pzs <- data[data$sheet_name == "Prioritization", ]

    data <- data[data$sheet_name != "Prioritization", ] %>%
      dplyr::group_by(dplyr::across(c(-value))) %>%
      dplyr::summarise(value = sum(value), .groups = "drop") %>%
      dplyr::bind_rows(pzs)

    data <- data[order(match(data$sheet_name, names(d$sheets))), ]
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
