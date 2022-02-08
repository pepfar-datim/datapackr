#' Title
#' @description Utility function which retrieves prioritization table
#' data from the DATIM analytics API.
#' @param d Datapackr d object
#' @param d2_session Datimutils session
#' @param include_no_prio If set to true, include No prioritization data
#'
#' @return A modified d object with d$data$memo$datim$prio
#' @export
#'

fetchPrioritizationTable <- function(d, d2_session, include_no_prio = TRUE) {

  inds <- getMemoIndicators(d$info$cop_year, d2_session = d2_session) %>%
    select(name, id)

  #TODO: Replace this with memoStructure
  df_cols <- tibble::tribble(
    ~id, ~shortName, ~col_name,
    "ATX2xv8PsrX", "PPG Attained", "Attained",
    "IzmZerN7tDN", "PPG Scale-up: Saturation", "Scale-up: Saturation",
    "AHMMjoPYta6", "PPG Scale up: Aggressive", "Scale-up: Aggressive",
    "b1X6pxMHgs6", "PPG Sustained", "Sustained",
    "pibJV72pMyW", "PPG Centrally Supported", "Centrally Supported",
    "CJYtvFbjeG2", "PPG No Prioritization", "No Prioritization",
    "p0JrTY2hLii", "PPG Not PEPFAR Supported", "Not PEPFAR Supported"
  )

  df_rows <- d$memo$structure %>%
    purrr::pluck("row_order") %>%
    dplyr::select(ind, options)

  df_base <- tidyr::crossing(df_rows, dplyr::select(df_cols, col_name)) %>%
    dplyr::arrange(ind, options, col_name) %>%
    dplyr::mutate(Value = 0) %>%
    dplyr::rename("Indicator" = ind,
                  Age = options)

  psnus <- dplyr::bind_rows(datapackr::valid_PSNUs) %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    dplyr::filter(!is.na(psnu_type)) %>%
    dplyr::pull(psnu_uid) %>%
    unique()

  #Break up into 2048 character URLS (approximately)
  n_requests <- ceiling(nchar(paste(psnus, sep = "", collapse = ";")) / 2048)
  n_groups <- split(sample(psnus), 1:n_requests)

  getPrioTable <- function(x) {
    datimutils::getAnalytics(ou = x,
                             dx = inds$id,
                             pe_f = paste0(d$info$cop_year, "Oct"),
                             d2_session = d2_session)
  }

  df <- n_groups %>% purrr::map_dfr(function(x) getPrioTable(x))

  if (is.null(df) | NROW(df) == 0) {
    return(d)
  }

  prios <- n_groups %>% purrr::map_dfr(function(x) getExistingPrioritization(x, d$info$cop_year, d2_session))

  df <- df %>%
    dplyr::rename("psnu_uid" = `Organisation unit`) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%
    dplyr::inner_join(inds, by = c(`Data` = "id")) %>%
    dplyr::select(-Data) %>%
    datapackr::seperateIndicatorMetadata(.) %>%
    dplyr::left_join(., prios, by = "psnu_uid") %>%
    dplyr::mutate(prioritization = as.character(prioritization)) %>%
    dplyr::mutate(prioritization = dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                                                    TRUE ~ prioritization)) %>%
    dplyr::group_by(`Indicator`, `Age`, `prioritization`) %>%
    dplyr::summarise(Value = sum(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename("col_name" = "prioritization")

  df_totals <- df %>%
    dplyr::filter(Age != "Total") %>%
    group_by(Indicator, col_name) %>%
    dplyr::summarise(Value = sum(Value)) %>%
    dplyr::mutate(Age = "Total") %>%
    dplyr::ungroup() %>%
    dplyr::select(names(df))

  df_final <- dplyr::bind_rows(df, df_totals, df_base) %>%
    dplyr::group_by(Indicator, Age, col_name) %>%
    dplyr::summarise(Value = sum(Value)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(col_name = factor(col_name, levels = df_cols$col_name)) %>%
    dplyr::mutate(Indicator = factor(Indicator, levels = unique(df_rows$ind))) %>%
    dplyr::arrange(Indicator, col_name) %>%
    tidyr::pivot_wider(names_from = col_name, values_from = "Value") %>%
    suppressWarnings()

  #Remove NOT pepfar supported if its only zeros, otherwise, show this, since its potentially problematic
  if (df_final %>% dplyr::select("Not PEPFAR Supported") %>% sum(., na.rm = TRUE) == 0) {
    df_final <- df_final %>% select(-`Not PEPFAR Supported`)
  }

  df_final %<>%
    mutate("Total" = rowSums(across(where(is.numeric)))) %>%
    dplyr::select("Indicator", "Age", 3:dim(.)[2])

  if (!include_no_prio & any("No Prioritization" %in% names(df_final))) {
    df_final %<>% dplyr::select(-`No Prioritization`)
  }

  d$data$memo$datim$prio <- df_final

  return(d)
}
