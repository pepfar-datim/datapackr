#' @export
#' @title Fetch Prioritization Table
#'
#' @inheritParams datapackr_params
#'
#' @return A data frame consisting of Organisation unit, Value, prioritization
#'
fetchPrioritizationTable <- function(psnus, cop_year,
                                       d2_session = dynGet("d2_default_session",
                                                           inherits = TRUE)) {
  period <- paste0(cop_year, "Oct")


  #We need to split up the requests if there are many PSNUs
  getPriosFromDatim <- function(x) {
    datimutils::getAnalytics(
      dx = "r4zbW3owX9n",
      pe_f = period,
      ou = x,
      d2_session = d2_session
    )
  }

  n_requests <- ceiling(nchar(paste(psnus, sep = "", collapse = ";")) / 2048)

  if (n_requests > 1) {
    n_groups <- split(psnus, cut(seq_along(psnus), breaks = n_requests + 1  , labels = FALSE))
  } else {
    n_groups <- list("1" = psnus)
  }

  prios <- n_groups %>%
    purrr::map_dfr(function(x) getPriosFromDatim(x))

  if (NROW(prios) == 0) {
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