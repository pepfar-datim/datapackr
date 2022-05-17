#' Title
#' @description Checks whether any MER data has been allocated
#' to PSNUs which are prioritized as "Not PEPFAR supported".
#'
#' @param d Datapackr object with generated analytics table
#'
#' @return d Datapackr object
#' @export
#'
checkNotPEPFARSupportedPSNUs <- function(d) {

  mer_dataelements <-
    pick_schema(d$info$cop_year, d$info$tool) %>%
    dplyr::filter(dataset == "mer" & col_type == "target") %>%
    dplyr::select(dataelement_dsd, dataelement_ta) %>%
    as.matrix(.) %>%
    as.vector(.) %>%
    purrr::keep(is_uidish(.))

  mer_data_not_pepfar_supported <- d$data$analytics %>%
    dplyr::select(psnu, dataelement_id, dataelement_name, mechanism_desc, prioritization) %>%
    dplyr::filter(prioritization == "Not PEPFAR Supported") %>%
    dplyr::filter(dataelement_id %in% mer_dataelements)

  # Alert to missing metadata
  if (NROW(mer_data_not_pepfar_supported) > 0) {

    d$tests$mer_data_not_pepfar_supported <- mer_data_not_pepfar_supported

    attr(d$tests$mer_data_not_pepfar_supported, "test_name") <- "MER data in non-PEPFAR supported PSNUs"

    warning_msg <-
      paste(
        "WARNING!", NROW(mer_data_not_pepfar_supported),
        "instances of MER data which has been allocated",
        "to a PSNU which has a prioritization of Not PEPFAR supported",
        "Please double check that the prioritization of the PSNU",
        "is correct or reallocate the data to a PSNU which",
        "is supported by PEPFAR.",
        "\n")


    d$info$messages <- appendMessage(d$info$messages, warning_msg, "WARNING")
  }

  return(d)

}
