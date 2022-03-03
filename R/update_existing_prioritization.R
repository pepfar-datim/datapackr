#' @export
#' @title Update Existing Prioritization
#'
#' @inheritParams datapackr_params
#'
#' @return
#'
updateExistingPrioritization <- function(d, d2_session =
                                             dynGet("d2_default_session",
                                                    inherits = TRUE)) {

  psnus <- d$data$analytics$psnu_uid %>% unique() %>% unlist()

  #Break up into 2048 character URLS (approximately)
  n_requests <- ceiling(nchar(paste(psnus, sep = "", collapse = ";")) / 2048)
  
  if (n_requests > 1) {
    n_groups <- split(psnus, cut(seq_along(psnus), breaks = n_requests + 1  , labels = FALSE))
  } else {
    n_groups <- list("1"=psnus)
  }

  getPrioTable <- function(x) {
    datimutils::getAnalytics(dx = "r4zbW3owX9n",
                             pe_f = paste0((d$info$cop_year), "Oct"),
                             ou = x,
                             d2_session = d2_session)
  }

  prios <- n_groups %>% purrr::map_dfr(getPrioTable)

  if (NROW(prios) > 0 & any(is.na(prios$Value))) {
    msg <- paste("ERROR! Missing prioritization PSNU prioritization levels",
                 "have been detected. Affected PSNUs will be classified as No",
                 "Prioritization but may lead to inconsistencies",
                 "in the draft memo generation and comparison")

    d$info$messages <- appendMessage(d$info$messages, "ERROR")

    prios <- prios %>%
      dplyr::mutate("Value" = dplyr::case_when(is.na(Value) ~ 0,
                                               TRUE ~ Value))
  }

  if (NROW(prios) == 0 & NROW(d$data$analytics) > 0) {
    msg <- paste("ERROR! We could not obtain any prioritization information",
                 "from DATIM All PSNUs will be classified as No Prioritization",
                 "but may lead to inconsistencies in the draft memo",
                 "generation and comparison")

    d$info$messages <- appendMessage(d$info$messages, "ERROR")
    prios <- tibble::tibble("Organisation unit" = psnus,
                            "Value" = 0,
                            "Data" = NA_character_)
  }

  prios %<>%
    dplyr::select(-Data) %>%
    dplyr::rename("psnu_uid" = "Organisation unit",
                  "value" = "Value") %>%
    dplyr::left_join(datapackr::prioritization_dict()) %>%
    dplyr::select(psnu_uid,
                  "prioritization" = "name")

  d$data$analytics %<>%
    dplyr::select(-prioritization) %>%
    dplyr::left_join(prios, by = "psnu_uid") %>%
    dplyr::mutate(prioritization = dplyr::case_when(
      is.na(prioritization) ~ "No Prioritization",
      TRUE ~ prioritization))

  d
}
