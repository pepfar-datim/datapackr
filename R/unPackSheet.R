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
    dplyr::select(sheet = sheet_name, indicator_code, col_type,
                  valid_ages, valid_sexes, valid_kps)

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
        tidyselect::any_of(keep_cols$indicator_code[keep_cols$sheet == y])) %>%
      tidyr::pivot_longer(
        cols = -tidyselect::any_of(
          c(header_cols$indicator_code[header_cols$sheet == y])),
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
      dplyr::left_join(keep_cols, by = c("indicator_code" = "indicator_code",
                                       "sheet_name" = "sheet"))

    data <- data[purrr::map2_lgl(data$Age, data$valid_ages, ~.x %in% .y[["name"]]), ]
    data <- data[purrr::map2_lgl(data$Sex, data$valid_sexes, ~.x %in% .y[["name"]]), ]
    data <- data[purrr::map2_lgl(data$KeyPop, data$valid_kps, ~.x %in% .y[["name"]]), ]

    data %<>%
      dplyr::select(
        -tidyselect::any_of(
          c("valid_ages", "valid_sexes", "valid_kps", "col_type")))

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

    # Aggregate (esp for OVC_HIVSTAT) ----
    data %<>%
      dplyr::group_by(-value) %>%
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
