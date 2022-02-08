
#' Title
#'
#' @param d Datapackr d object
#' @param d2_session Datimutils sesssion object
#'
#' @return Tibble in wide format with Agency,Partner,Mechanism and all
#' grouped indicators along the columns
#' @export
#'

preparePartnerMemoTable <- function(d, d2_session = dynGet("d2_default_session",
                                                           inherits = TRUE)) {

  inds <- getMemoIndicators(d$info$cop_year, d2_session = d2_session)

  df <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(target_value)) %>%
    dplyr::select(dataelement_id,
                  categoryoptioncombo_id,
                  mechanism_code,
                  funding_agency,
                  partner_desc,
                  value = target_value) %>%
    dplyr::group_by(dataelement_id, categoryoptioncombo_id, mechanism_code, funding_agency, partner_desc) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(combi = paste0("#{", dataelement_id, ".", categoryoptioncombo_id, "}")) %>%
    dplyr::select(-dataelement_id,-categoryoptioncombo_id) %>%
    dplyr::group_by(mechanism_code,funding_agency,partner_desc) %>%
    tidyr::nest()

  #Evaluate the indicators in parallel if possible
  if ("parallel" %in% rownames(installed.packages()) == TRUE) {
    df$indicator_results <-
      parallel::mclapply(df$data, function(x)
        evaluateIndicators(x$combi, x$value, inds = inds), mc.cores = parallel::detectCores())
  } else {
    df$indicator_results <-
      lapply(df$data, function(x)
        evaluateIndicators(x$combi, x$value, inds = inds))
  }


  #Munge the data into the correct format
  df <- df %>%
    dplyr::select(-data) %>%
    tidyr::unnest(indicator_results) %>%
    dplyr::rename("Mechanism" = mechanism_code, "Agency" = funding_agency, "Partner" = partner_desc, Value = value) %>%
    dplyr::select(-id, -numerator, -denominator) %>%
    seperateIndicatorMetadata(.) %>%
    tidyr::complete(., tidyr::nesting(Mechanism, Agency, Partner), Indicator, Age, fill = list(Value = 0)) %>%
    tidyr::drop_na()

  #Get the memo structure
  d <- memoStructure(d)

  df_rows <- d$memo$structure %>%
    purrr::pluck("row_order") %>%
    dplyr::select(ind, options)

  d_base <- tidyr::crossing(df_rows, dplyr::distinct(unique(df[, c("Mechanism", "Partner", "Agency")]))) %>%
    dplyr::mutate(Value = 0) %>%
    dplyr::rename("Indicator" = ind,
                  Age = options)

  #Calculate totals

  d_totals <- dplyr::bind_rows(d_base, df) %>%
    dplyr::group_by(`Indicator`, `Age`) %>%
    dplyr::summarise(`Value` = sum(`Value`)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`Partner` = "Total", `Mechanism` = "Total", Agency = "Total")

  #Remove dedupe
  #TODO: Are we dealing with codes of mechanisms here??
  d_partners <- dplyr::filter(df, !(`Mechanism` %in% c("00001", "00000"))) #nolint

  d_indicators <- d$memo$structure %>%
    purrr::pluck("row_order") %>%
    dplyr::filter(in_partner_table) %>%
    dplyr::select(ind, options) %>%
    dplyr::mutate(indicator_name = factor(paste(ind, options)))

  #Put totals at the bottom of the table
  partner_levels <- c(sort(unique(df$Partner)), "Total")
  agency_levels <- c(sort(unique(df$Agency)), "Total")

  #Return the final data frame
  d$data$partners_table <- dplyr::bind_rows(df, d_totals) %>%
    dplyr::mutate(indicator_name = paste(`Indicator`, `Age`)) %>%
    dplyr::mutate(`Label` = indicator_name) %>%
    dplyr::arrange(`Agency`, `Partner`, `Mechanism`, indicator_name) %>%
    dplyr::select(`Agency`, `Partner`, `Mechanism`, `Label`, `Value`) %>%
    tidyr::pivot_wider(names_from = `Label`, values_from = `Value`, values_fill = 0) %>%
    dplyr::select(`Agency`, `Partner`, `Mechanism`, d_indicators$indicator_name) %>%
    dplyr::mutate(`Partner` = factor(Partner, levels = partner_levels),
                  `Agency` = factor(Agency, levels = agency_levels)) %>%
    dplyr::arrange(`Agency`, `Partner`, `Mechanism`)

  return(d)
}
