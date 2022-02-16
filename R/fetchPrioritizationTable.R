#' Title
#'
#' @param psnus A list of PSNUs
#' @param cop_year The cop year
#' @param d2_session Datimutils d2_session object
#'
#' @return A data frame consisting of Organisation unit, Value, prioritization
#' @export
#'

getExistingPrioritization <- function(psnus, cop_year, d2_session) {
  period <- paste0(cop_year, "Oct")
  ous <- paste(psnus, sep = "", collapse = ";")
  prios <-
    datimutils::getAnalytics(
      dx = "r4zbW3owX9n",
      pe_f = period,
      ou = ous,
      d2_session = d2_session
    )

  if (is.null(prios)) {
    return(data.frame("psnu_uid" = psnus, "prioritization" = "No Prioritization"))
  }

  prios %>%
    dplyr::select(-Data) %>%
    dplyr::rename("psnu_uid" = "Organisation unit",
                  "value" = "Value") %>%
    dplyr::left_join(datapackr::prioritization_dict()) %>%
    dplyr::select(psnu_uid, "prioritization" = "name") %>%
    dplyr::mutate(prioritization = dplyr::case_when(
      is.na(prioritization) ~ "No Prioritization",
      TRUE ~ prioritization
    ))

}


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

fetchPrioritizationTable <- function(d, d2_session = dynGet("d2_default_session",
                                                            inherits = TRUE), include_no_prio = TRUE) {

  inds <- getMemoIndicators(d$info$cop_year, d2_session = d2_session) %>%
    select(name, id)

  #Get the structure if it does not exist
  if (is.null(d$memo$structure)) {
    d <- memoStructure(d)
  }

  #TODO: Replace this with memoStructure
  df_cols <- d$memo$structure$col_order %>%
    dplyr::select(id,col_name = name)


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
  n_groups <- n_groups <- split(psnus, ceiling(seq_along(psnus) / (length(psnus) / n_requests)))

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
    datapackr::separateIndicatorMetadata(.) %>%
    dplyr::left_join(., prios, by = "psnu_uid") %>%
    dplyr::mutate(prioritization = as.character(prioritization)) %>%
    dplyr::mutate(prioritization = dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                                                    TRUE ~ prioritization)) %>%
    dplyr::group_by(`Indicator`, `Age`, `prioritization`) %>%
    dplyr::summarise(Value = sum(Value),.groups = "drop") %>%
    dplyr::rename("col_name" = "prioritization")

  df_totals <- df %>%
    dplyr::filter(Age != "Total") %>%
    group_by(Indicator, col_name) %>%
    dplyr::summarise(Value = sum(Value),.groups = "drop") %>%
    dplyr::mutate(Age = "Total") %>%
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

  #Remove NOT PEPFAR supported if its only zeros, otherwise, show this, since its potentially problematic
  if (df_final %>% dplyr::select("Not PEPFAR Supported") %>% sum(., na.rm = TRUE) == 0) {
    df_final <- df_final %>% select(-`Not PEPFAR Supported`)
  }

  df_final %<>%
    dplyr::mutate("Total" = rowSums(across(where(is.numeric)))) %>%
    dplyr::select(Total != 0) %>% # Remove all rows which are completely zero
    dplyr::select(where(~ any(. != 0))) # Remove all columns which are completely zero

  if (!include_no_prio & any("No Prioritization" %in% names(df_final))) {
    df_final %<>% dplyr::select(-`No Prioritization`)
  }

  d$data$memo$datim$prio <- df_final

  return(d)
}
