#' @title COP Memo Target Table Structure
#'
#' @inheritParams datapackr_params
#'
#' @return d object with d$memo$structure
#' @export
#'
memoStructure <- function(d, d2_session = dynGet("d2_default_session",
                                                  inherits = TRUE)) {

  if (!(d$info$cop_year %in% supportedCOPYears())) {
    warning("COP Memo structure unknown for given COP year")
    return(d)
  }

  if (d$info$cop_year %in% c("2023", "2024", "2025")) {
    row_order <- tibble::tribble(
      ~ind, ~options, ~partner_chunk,
      "TX_NEW", "<15", 1,
      "TX_NEW", "15+", 1,
      "TX_NEW", "Total", NA,
      "TX_CURR", "<15", 1,
      "TX_CURR", "15+", 1,
      "TX_CURR", "Total", NA,
      "TX_PVLS_(D)", "<15", 1,
      "TX_PVLS_(D)", "15+", 1,
      "TX_PVLS_(D)", "Total", NA,
      "TX_PVLS_(N)", "<15", 1,
      "TX_PVLS_(N)", "15+", 1,
      "TX_PVLS_(N)", "Total", NA,
      "HTS_SELF", "<15", 2,
      "HTS_SELF", "15+", 2,
      "HTS_SELF", "Total", NA,
      "HTS_TST", "<15", 2,
      "HTS_TST", "15+", 2,
      "HTS_TST", "Total", NA,
      "HTS_TST_POS", "<15", 2,
      "HTS_TST_POS", "15+", 2,
      "HTS_TST_POS", "Total", NA,
      "HTS_RECENT", "Total", 2,
      "HTS_INDEX", "<15", 2,
      "HTS_INDEX", "15+", 2,
      "HTS_INDEX", "Total", NA,
      "PMTCT_STAT", "<15", 3,
      "PMTCT_STAT", "15+", 3,
      "PMTCT_STAT", "Total", NA,
      "PMTCT_STAT_POS", "<15", 3,
      "PMTCT_STAT_POS", "15+", 3,
      "PMTCT_STAT_POS", "Total", NA,
      "PMTCT_ART", "<15", 3,
      "PMTCT_ART", "15+", 3,
      "PMTCT_ART", "Total", NA,
      "PMTCT_EID", "Total", 3,
      "TB_STAT", "<15", 4,
      "TB_STAT", "15+", 4,
      "TB_STAT", "Total", NA,
      "TB_ART", "<15", 4,
      "TB_ART", "15+", 4,
      "TB_ART", "Total", NA,
      "TB_PREV", "<15", 4,
      "TB_PREV", "15+", 4,
      "TB_PREV", "Total", NA,
      "TX_TB", "<15", 4,
      "TX_TB", "15+", 4,
      "TX_TB", "Total", NA,
      "VMMC_CIRC", "Total", 5,
      "KP_PREV", "Total", 5,
      "KP_MAT", "Total", 5,
      "PrEP_NEW", "Total", 5,
      "PrEP_CT", "Total", 5,
      "CXCA_SCRN", "Total", 6,
      "PP_PREV", "<15", 6,
      "PP_PREV", "15+", 6,
      "PP_PREV", "Total", NA,
      "OVC_SERV", "<18", 6,
      "OVC_SERV", "18+", 6,
      "OVC_SERV", "Total", NA,
      "OVC_HIVSTAT", "Total", 6,
      "GEND_GBV", "Total", 6,
      "AGYW_PREV", "Total", NA)
  }

  col_order <- tibble::tribble(
    ~value, ~name, ~col_order, ~id,
    0, "No Prioritization", 7, "CJYtvFbjeG2",
    1, "Scale-up: Saturation", 2, "IzmZerN7tDN",
    2, "Scale-up: Aggressive", 3, "AHMMjoPYta6",
    4, "Sustained", 4, "b1X6pxMHgs6",
    5, "Centrally Supported", 5, "pibJV72pMyW",
    6, "Sustained: Commodities", 6, "ma6CQUVQRWI",
    7, "Attained", 1, "ATX2xv8PsrX",
    8, "Not PEPFAR Supported", 8, "p0JrTY2hLii") %>%
    dplyr::mutate(Prioritization = paste0(value, " - ", name))


  age_order <- c("<15", "<18", "15+", "18+", "Total")

  d$memo$structure <- list(row_order = row_order, col_order = col_order, age_order = age_order)

  d$memo$inds <- getMemoIndicators(d$info$cop_year, d2_session)

  d
}
