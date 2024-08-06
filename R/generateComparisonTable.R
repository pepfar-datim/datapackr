#' @export
#' @title Generate Comparison Table
#'
#' @inheritParams datapackr_params
#' @description Generates a PSNUxIM level comparison between
#' the COP indicator values in the (OPU) DataPack and what is currently
#' present in DATIM.
#'
#' @return A comparison table
#'
generateComparisonTable <- function(d, expanded=F) {

  if (is.null(d$memo$datapack$by_psnu)) {
    d_datapack <- data.frame("psnu_uid" = character(),
                             "Proposed" = numeric())
  } else {
    d_datapack <- d$memo$datapack$by_psnu %>%
      dplyr::rename("Proposed" = value)
  }

  if (is.null(d$memo$datim$by_psnu)) {
    d_datim <-  data.frame("psnu_uid" = character(),
                           "Current" = numeric())
  } else {
    d_datim <-  d$memo$datim$by_psnu %>%
      dplyr::rename("Current" = value)
  }

  if (NROW(d_datapack) > 0 || NROW(d_datim) > 0) {

    d$memo$comparison <- dplyr::full_join(d_datapack, d_datim)  %>%
      dplyr::mutate(Current = ifelse(is.na(Current), 0, Current),
                    Proposed = ifelse(is.na(Proposed), 0, Proposed)) %>%
      dplyr::mutate("Identical" = dplyr::near(Current, Proposed, tol = 1e-5),
                    "Diff" = Proposed - Current,
                    "Percent diff" = round(Diff / Proposed * 100, digits = 1)) %>%
      { if (expanded == T)
        dplyr::filter(!Identical) else .
      } %>%
      tidyr::pivot_longer(cols = c(Current, Proposed, Diff, `Percent diff`), names_to = "Data Type")  %>%
      dplyr::mutate(`Data Type` = factor(`Data Type`,
                                         levels = ifelse(expanded == T,
                                                         c("Proposed", "Current","Identical", "Diff", "Percent diff"),
                                                         c("Proposed", "Current", "Diff", "Percent diff")))) %>%
      { if (expanded == T)
        dplyr::select(-psnu_uid) else dplyr::select(-psnu_uid, -Identical)
      }

  }

 d
}
