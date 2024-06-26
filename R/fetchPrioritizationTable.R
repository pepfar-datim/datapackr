
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

  stopifnot("PSNUs must be a vector" =  is.vector(psnus))
  #We need to split up the requests if there are many PSNUs
  # Explicitly filter Military data which may have an assigned prioritization
  # level below the PSNU level. This will get aggregated in the analytics
  # call leading to incorrect prioritization levels. All Military data
  # should always be classified as No prioritization anyway and is handled
  # later when left joining to the main data.
  getPriosFromDatim <- function(x) {

    tryCatch({
      datimutils::getAnalytics(
        "filter=mINJi7rR1a6:POHZmzofoVx;PvuaP6YALSA;AookYR4ECPH",
        dx = "r4zbW3owX9n",
        pe_f = period,
        ou = x,
        d2_session = d2_session)
    },
    error = function(cond) {
      message(cond)
      warning("Could not retrieve prioritization levels")
      return(NULL)
    })
  }

  n_requests <- ceiling(nchar(paste(psnus, sep = "", collapse = ";")) / 2048)

  if (n_requests > 1) {
    n_groups <- split(psnus, cut(seq_along(psnus), breaks = n_requests + 1, labels = FALSE))
  } else {
    n_groups <- list("1" = psnus)
  }

  prios <- n_groups %>% purrr::map_dfr(getPriosFromDatim)

  if (NROW(prios) == 0) {
    return(data.frame("orgUnit" = unique(psnus),
                      "value" = 0))
  }

  #Make this compatible with the input to getPrioritizationMap
  prios %>%
    dplyr::select(-Data) %>%
    dplyr::rename(orgUnit = "Organisation unit",
                          value = "Value")

  # #Helper methods from adorn_import_file
  # #Produces a full prioritization map consisting of
  # #a two column tibble. The column id corresponds to the
  # #organisation unit UID
  # #which might or might not be a prioritization PSNU.
  # #The prioritization column is represented as character text .
  # #like "Attained".
  # getPriorizationSNU(psnus$psnu_uid) %>%
  # getPrioritizationMap(., prios)
}
