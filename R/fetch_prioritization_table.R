#' @export
#' @title Fetch Prioritization Table
#'
#' @inheritParams datapackr_params
#'
#' @return A data frame consisting of Organisation unit, Value, prioritization
#'
fetch_prioritization_table <- function(psnus, cop_year,
                                       d2_session = dynGet("d2_default_session",
                                                           inherits = TRUE)) {
  period <- paste0(cop_year, "Oct")
  ous <- paste(psnus, sep = "", collapse = ";")

  #We need to split up the requests if there are many PSNUs
  getPriosFromDatim <- function(x) {
    datimutils::getAnalytics(
      dx = "r4zbW3owX9n",
      pe_f = period,
      ou = ous,
      d2_session = d2_session
    )
  }

  n_requests <- ceiling(nchar(paste(psnus, sep = "", collapse = ";")) / 2048)
  n_groups <- split(psnus, ceiling(seq_along(psnus) /
                                     (length(psnus) / n_requests)))

  prios <- n_groups %>%
    purrr::map_dfr(function(x) getPriosFromDatim(x))

  if (is.null(prios)) {
    return(data.frame("psnu_uid" = psnus,
                      "prioritization" = "No Prioritization",
                      value = 0))
  }

  prios %>%
    dplyr::select(-Data) %>%
    dplyr::rename("psnu_uid" = "Organisation unit",
                  "value" = "Value") %>%
    dplyr::left_join(datapackr::prioritization_dict(), by = "value") %>%
    dplyr::select(psnu_uid, "prioritization" = "name", value) %>%
    dplyr::mutate(prioritization = dplyr::case_when(
      is.na(prioritization) ~ "No Prioritization",
      TRUE ~ prioritization
    ))

}
