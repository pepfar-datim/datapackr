#' @export
#' @importFrom magrittr %>% %<>%
#' @title Handle AGYW data from a Data Pack
#'
#' @description Used to process AGYW sheets in a Data Pack
#' alerts on missing or invalid SNUs.
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'

handleAGYW <- function(d, sheet) {

  DataPack_DSNUs <- d$data$extract %>%
    dplyr::select(PSNU, psnu_uid = psnuid) %>%
    dplyr::distinct() %>%
    dplyr::mutate(DataPack = 1)

  DATIM_DSNUs <- datapackr::valid_PSNUs %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    add_dp_psnu(.) %>%
    dplyr::arrange(dp_psnu) %>%
    dplyr::filter(!is.na(DREAMS)) %>%
    dplyr::select(PSNU = dp_psnu, psnu_uid, snu1) %>%
    dplyr::mutate(DATIM = 1)

  DSNU_comparison <- DataPack_DSNUs %>%
    dplyr::full_join(DATIM_DSNUs, by = "psnu_uid")

  d$tests$DSNU_comparison <- DSNU_comparison
  attr(d$tests$DSNU_comparison, "test_name") <- "DSNU List Comparison"

  if (any(is.na(DSNU_comparison$DataPack))) {
    missing_DSNUs <- DSNU_comparison %>%
      dplyr::filter(is.na(DataPack))

    warning_msg <- paste0(
      "WARNING! In tab ",
      sheet,
      ": MISSING DREAMS SNUs found! ->  \n\t* ",
      paste(missing_DSNUs$PSNU.y, collapse = "\n\t* "),
      "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
    d$info$missing_DSNUs <- TRUE
  }

  if (any(is.na(DSNU_comparison$DATIM))) {
    invalid_DSNUs <- DSNU_comparison %>%
      dplyr::filter(is.na(DATIM))

    warning_msg <- paste0(
      "WARNING! In tab ",
      sheet,
      ": INVALID DREAMS SNUs found! ->  \n\t* ",
      paste(invalid_DSNUs$PSNU.x, collapse = "\n\t* "),
      "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  return(d)

}
