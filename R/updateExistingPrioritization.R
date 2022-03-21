#' @export
#' @title Update Existing Prioritization
#' @description Provided a data frame of priorization levels and an analytics
#' table, replace the existing prioritizations. This is normally
#' used with an OPU DataPack.
#' @param analytics_table A table of COP memo indicators at the PSNU level.
#' @inheritParams datapackr_params
#'
#' @return
#'
updateExistingPrioritization <- function(prios, analytics_table) {

  prios <- prios %>%  dplyr::select(-value)

    analytics_table %>%
    dplyr::select(-prioritization) %>%
    dplyr::left_join(prios, by = "psnu_uid") %>%
    dplyr::mutate(prioritization = dplyr::case_when(
      is.na(prioritization) ~ "No Prioritization",
      TRUE ~ prioritization))

}
