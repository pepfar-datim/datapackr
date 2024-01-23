#' @export
#' @title Check Mechanisms
#'
#' @description Tests mechanisms found in the Data Pack PSNUxIM tab
#' against the Mechanism View from DATIM.
#'
#' @inheritParams datapackr_params
#'
#' @return d
#'
checkMechanisms <- function(d,
                            cached_mechs_path = paste0(Sys.getenv("support_files_directory"), "mechs.rds"),
                            d2_session = dynGet("d2_default_session",
                                                inherits = TRUE)) {

  #Do not refer to d$datim$MER or d$datim$OPU directly.
  mechs_data <- unique(d$data$analytics$mechanism_code)

  stopifnot("Mechanisms cannot be null." = !is.null(mechs_data) || length(mechs_data) != 0L)

  period_info <- datimvalidation::getPeriodFromISO(paste0(d$info$cop_year, "Oct"))


  mechs_datim <- datapackr::getMechanismView(d2_session = d2_session,
                                             update_stale_cache = TRUE,
                                             cached_mechs_path = cached_mechs_path) %>%
    dplyr::filter(ou == d$info$operating_unit$ou) %>%
    dplyr::filter(!is.na(startdate)) %>%
    dplyr::filter(!is.na(enddate)) %>%
    dplyr::filter(startdate <= period_info$startDate) %>%
    dplyr::filter(enddate >= period_info$endDate) %>%
    dplyr::pull(mechanism_code)


  #TODO: There should really be a better check here.
  #Default should only be allowable with certain data elements.
  #Allow for the default mechanism
  mechs_datim <- append("default", mechs_datim)
  #Allow for the dedupe mechanisms in COP21/COP22/COP23 DataPacks and OPUs

  #TODO: There is some discrepancy here regarding
  #the use of OPU Data Pack and PSNUxIM.
  #Get rid of OPU Data Pack completely and only use PSNUxIM.

  can_have_dedupe <-
    (d$info$tool %in% c("Data Pack", "OPU Data Pack") &&
    d$info$cop_year %in% c(2021:2024)) ||
  (d$info$tool == "PSNUxIM" && d$info$cop_year >= 2023)

  if (can_have_dedupe) {
    mechs_datim <- append(c("00000", "00001"), mechs_datim)
  }

  bad_mechs <- sort(mechs_data[!(mechs_data %in% mechs_datim)])

  if (length(bad_mechs) > 0) {
    msg <- paste0("ERROR!: Invalid mechanisms found in the PSNUxIM tab. ",
                  "Please ensure that these mechanisms have been marked ",
                  "as active for COP",
                  substr(d$info$cop_year, 3, 4), " in FACTS-Info or ",
                  "reallocate values to a valid mechanism:",
                  paste(bad_mechs, sep = "", collapse = ", "))
    d$tests$bad_mechs <- data.frame(mechanism_code = bad_mechs)
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    d$info$has_error <- TRUE
  }

  return(d)
}
