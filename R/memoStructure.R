#' Title
#'
#' @param d
#'
#' @return d object with d$memo$structure
#' @export
#'

memoStructure <- function(d) {


if (!(d$info$cop_year %in% c("2020","2021","2022"))) {
  warning("COP Memo structure uknown for given COP year")
  return(d)
}

  if (d$info$cop_year == "2020") {
    row_order <- tibble::tribble(
      ~ind, ~options, ~in_partner_table,
      "HTS_INDEX", "<15", TRUE,
      "HTS_INDEX", "15+", TRUE,
      "HTS_INDEX", "Total", FALSE,
      "HTS_TST", "<15", TRUE,
      "HTS_TST", "15+", TRUE,
      "HTS_TST", "Total", FALSE,
      "HTS_TST_POS", "<15", TRUE,
      "HTS_TST_POS", "15+", TRUE,
      "HTS_TST_POS", "Total", FALSE,
      "TX_NEW", "<15", TRUE,
      "TX_NEW", "15+", TRUE,
      "TX_NEW", "Total", FALSE,
      "TX_CURR", "<15", TRUE,
      "TX_CURR", "15+", TRUE,
      "TX_CURR", "Total", FALSE,
      "TX_PVLS", "<15", TRUE,
      "TX_PVLS", "15+", TRUE,
      "TX_PVLS", "Total", FALSE,
      "CXCA_SCRN", "Total", TRUE,
      "OVC_SERV", "<18", TRUE,
      "OVC_SERV", "18+", TRUE,
      "OVC_SERV", "Total", FALSE,
      "OVC_HIVSTAT", "Total", TRUE,
      "PMTCT_STAT", "<15", TRUE,
      "PMTCT_STAT", "15+", TRUE,
      "PMTCT_STAT", "Total", FALSE,
      "PMTCT_STAT_POS", "<15", TRUE,
      "PMTCT_STAT_POS", "15+", TRUE,
      "PMTCT_STAT_POS", "Total", FALSE,
      "PMTCT_ART", "<15", TRUE,
      "PMTCT_ART", "15+", TRUE,
      "PMTCT_ART", "Total", FALSE,
      "PMTCT_EID", "Total", TRUE,
      "PP_PREV", "<15", TRUE,
      "PP_PREV", "15+", TRUE,
      "PP_PREV", "Total", FALSE,
      "KP_PREV", "Total", TRUE,
      "KP_MAT", "Total", TRUE,
      "VMMC_CIRC", "Total", TRUE,
      "HTS_SELF", "<15", TRUE,
      "HTS_SELF", "15+", TRUE,
      "HTS_SELF", "Total", FALSE,
      "PrEP_NEW", "Total", TRUE,
      "PrEP_CURR", "Total", TRUE,
      "TB_STAT", "<15", TRUE,
      "TB_STAT", "15+", TRUE,
      "TB_STAT", "Total", FALSE,
      "TB_ART", "<15", TRUE,
      "TB_ART", "15+", TRUE,
      "TB_ART", "Total", FALSE,
      "TB_PREV", "<15", TRUE,
      "TB_PREV", "15+", TRUE,
      "TB_PREV", "Total", FALSE,
      "TX_TB", "<15", TRUE,
      "TX_TB", "15+", TRUE,
      "TX_TB", "Total", FALSE,
      "GEND_GBV", "Total", TRUE)
  }

  #TOOD: Confirm the memo structure for 2022
  if (d$info$cop_year %in% c("2021")) {
    row_order <- tibble::tribble(
      ~ind, ~options, ~in_partner_table,
      "HTS_INDEX", "<15", TRUE,
      "HTS_INDEX", "15+", TRUE,
      "HTS_INDEX", "Total", FALSE,
      "HTS_TST", "<15", TRUE,
      "HTS_TST", "15+", TRUE,
      "HTS_TST", "Total", FALSE,
      "HTS_TST_POS", "<15", TRUE,
      "HTS_TST_POS", "15+", TRUE,
      "HTS_TST_POS", "Total", FALSE,
      "TX_NEW", "<15", TRUE,
      "TX_NEW", "15+", TRUE,
      "TX_NEW", "Total", FALSE,
      "TX_CURR", "<15", TRUE,
      "TX_CURR", "15+", TRUE,
      "TX_CURR", "Total", FALSE,
      "TX_PVLS", "<15", TRUE,
      "TX_PVLS", "15+", TRUE,
      "TX_PVLS", "Total", FALSE,
      "CXCA_SCRN", "Total", TRUE,
      "OVC_SERV", "<18", TRUE,
      "OVC_SERV", "18+", TRUE,
      "OVC_SERV", "Total", FALSE,
      "OVC_HIVSTAT", "Total", TRUE,
      "PMTCT_STAT", "<15", TRUE,
      "PMTCT_STAT", "15+", TRUE,
      "PMTCT_STAT", "Total", FALSE,
      "PMTCT_STAT_POS", "<15", TRUE,
      "PMTCT_STAT_POS", "15+", TRUE,
      "PMTCT_STAT_POS", "Total", FALSE,
      "PMTCT_ART", "<15", TRUE,
      "PMTCT_ART", "15+", TRUE,
      "PMTCT_ART", "Total", FALSE,
      "PMTCT_EID", "Total", TRUE,
      "PP_PREV", "<15", TRUE,
      "PP_PREV", "15+", TRUE,
      "PP_PREV", "Total", FALSE,
      "KP_PREV", "Total", TRUE,
      "KP_MAT", "Total", TRUE,
      "VMMC_CIRC", "Total", TRUE,
      "HTS_SELF", "<15", TRUE,
      "HTS_SELF", "15+", TRUE,
      "HTS_SELF", "Total", FALSE,
      "PrEP_NEW", "Total", TRUE,
      "PrEP_CURR", "Total", TRUE,
      "TB_STAT", "<15", TRUE,
      "TB_STAT", "15+", TRUE,
      "TB_STAT", "Total", FALSE,
      "TB_ART", "<15", TRUE,
      "TB_ART", "15+", TRUE,
      "TB_ART", "Total", FALSE,
      "TB_PREV", "<15", TRUE,
      "TB_PREV", "15+", TRUE,
      "TB_PREV", "Total", FALSE,
      "TX_TB", "<15", TRUE,
      "TX_TB", "15+", TRUE,
      "TX_TB", "Total", FALSE,
      "GEND_GBV", "Total", TRUE,
      "AGYW_PREV", "Total", FALSE)
  }
 #TOOD: Confirm the memo structure for 2022
  if (d$info$cop_year %in% c("2022")) {
    row_order <- tibble::tribble(
      ~ind, ~options, ~in_partner_table,
      "HTS_INDEX", "<15", TRUE,
      "HTS_INDEX", "15+", TRUE,
      "HTS_INDEX", "Total", FALSE,
      "HTS_TST", "<15", TRUE,
      "HTS_TST", "15+", TRUE,
      "HTS_TST", "Total", FALSE,
      "HTS_TST_POS", "<15", TRUE,
      "HTS_TST_POS", "15+", TRUE,
      "HTS_TST_POS", "Total", FALSE,
      "HTS_RECENT","<15",TRUE,
      "HTS_RECENT","15+",TRUE,
      "HTS_RECENT","Total",TRUE,
      "TX_NEW", "<15", TRUE,
      "TX_NEW", "15+", TRUE,
      "TX_NEW", "Total", FALSE,
      "TX_CURR", "<15", TRUE,
      "TX_CURR", "15+", TRUE,
      "TX_CURR", "Total", FALSE,
      "TX_PVLS", "<15", TRUE,
      "TX_PVLS", "15+", TRUE,
      "TX_PVLS", "Total", FALSE,
      "CXCA_SCRN", "Total", TRUE,
      "OVC_SERV", "<18", TRUE,
      "OVC_SERV", "18+", TRUE,
      "OVC_SERV", "Total", FALSE,
      "OVC_HIVSTAT", "Total", TRUE,
      "PMTCT_STAT", "<15", TRUE,
      "PMTCT_STAT", "15+", TRUE,
      "PMTCT_STAT", "Total", FALSE,
      "PMTCT_STAT_POS", "<15", TRUE,
      "PMTCT_STAT_POS", "15+", TRUE,
      "PMTCT_STAT_POS", "Total", FALSE,
      "PMTCT_ART", "<15", TRUE,
      "PMTCT_ART", "15+", TRUE,
      "PMTCT_ART", "Total", FALSE,
      "PMTCT_EID", "Total", TRUE,
      "PP_PREV", "<15", TRUE,
      "PP_PREV", "15+", TRUE,
      "PP_PREV", "Total", FALSE,
      "KP_PREV", "Total", TRUE,
      "KP_MAT", "Total", TRUE,
      "VMMC_CIRC", "Total", TRUE,
      "HTS_SELF", "<15", TRUE,
      "HTS_SELF", "15+", TRUE,
      "HTS_SELF", "Total", FALSE,
      "PrEP_NEW", "Total", TRUE,
      "PrEP_CURR", "Total", TRUE,
      "TB_STAT", "<15", TRUE,
      "TB_STAT", "15+", TRUE,
      "TB_STAT", "Total", FALSE,
      "TB_ART", "<15", TRUE,
      "TB_ART", "15+", TRUE,
      "TB_ART", "Total", FALSE,
      "TB_PREV", "<15", TRUE,
      "TB_PREV", "15+", TRUE,
      "TB_PREV", "Total", FALSE,
      "TX_TB", "<15", TRUE,
      "TX_TB", "15+", TRUE,
      "TX_TB", "Total", FALSE,
      "GEND_GBV", "Total", TRUE,
      "AGYW_PREV", "Total", FALSE)
  }

  col_order <- tibble::tribble(
    ~value, ~name, ~col_order,
    0, "No Prioritization", 7,
    1, "Scale-up: Saturation", 2,
    2, "Scale-up: Aggressive", 3,
    4, "Sustained", 4,
    5, "Centrally Supported", 5,
    6, "Sustained: Commodities", 6,
    7, "Attained", 1,
    8, "Not PEPFAR Supported", 8) %>%
    dplyr::mutate(Prioritization = paste0(value, " - ", name))

  d$memo$structure <- list(row_order = row_order, col_order = col_order)

  return(d)
}