#' @export
#' @importFrom magrittr %>% %<>%
#' @title Handle TX data from a Data Pack
#'
#' @description Used to process TX sheets in a Data Pack (legacy).
#'
#' @param d Datapackr object.
#' @param sheet Sheet to check
#'
#' @return d
#'

handleTX <- function(d, sheet) {
  d$tests$tx_new_invalid_lt1_sources <- d$data$extract %>%
    dplyr::select(PSNU, Age, Sex, TX_NEW.N.Age_Sex_HIVStatus.T,
                  TX_NEW.N.IndexRate, TX_NEW.N.TBRate, TX_NEW.N.PMTCTRate,
                  TX_NEW.N.PostANC1Rate, TX_NEW.N.EIDRate, TX_NEW.N.VMMCRateNew,
                  TX_NEW.N.prevDiagnosedRate) %>%
    dplyr::filter(Age == "<01",
                  TX_NEW.N.Age_Sex_HIVStatus.T > 0) %>%
    dplyr::filter_at(dplyr::vars(-PSNU, -Age, -Sex, -TX_NEW.N.EIDRate,
                                 -TX_NEW.N.Age_Sex_HIVStatus.T),
                     dplyr::any_vars(. > 0))
  attr(d$tests$tx_new_invalid_lt1_sources, "test_name") <- "Invalid TX <01 data source"

  if (NROW(d$tests$tx_new_invalid_lt1_sources) > 0) {
    warning_msg <-
      paste0(
        "WARNING! In tab TX",
        ": TX_NEW for <01 year olds being targeted through method other than EID.",
        " MER Guidance recommends all testing for <01 year olds be performed through EID rather than HTS",
        "\n")

    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }
  return(d)
}
