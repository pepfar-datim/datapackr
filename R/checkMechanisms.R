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

  #Allow for the default mechanism
  mechs_datim <- append("default", mechs_datim)

  #Append the dedupe mechanisms in DataPacks and OPUs
  mechs_datim <- append(c("00000", "00001"), mechs_datim)

  bad_mechs <- sort(mechs_data[!(mechs_data %in% mechs_datim)])

  if (length(bad_mechs) > 0) {
    msg <- paste0("ERROR!: Invalid mechanisms found in the PSNUxIM tab. ",
                  "Please ensure that these mechanisms have been marked ",
                  "as active for COP",
                  substr(d$info$cop_year, 3, 4), " in FACTS-Info or ",
                  "reallocate values to a valid mechanism.",
                  "Please reference the FAQ section of the User Guide or navigate to ",
                  "https://peptest.dfafacts.gov/assets/guides/HowtoMakeMechanismsAppearinDATIM.pdf ,",
                  "to see step by step instructions on how to resolve.",
                  "The following mechanisms are implicated: ",
                  paste(bad_mechs, sep = "", collapse = ", "))
    d$tests$bad_mechs <- data.frame(mechanism_code = bad_mechs)
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    d$info$has_error <- TRUE
  }

  return(d)
}
