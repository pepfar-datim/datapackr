#' @export
#' @title createPAWExport
#'
#' @inheritParams datapackr_params
#'
#' @return A data frame in standardized DHIS2 export format for PAW
#' @export
#'
createPAWExport <- function(d) {

  if (d$info$tool == "Data Pack") {

    if (d$info$cop_year %in% c(2023, 2024, 2025)) {
      if (d$info$has_psnuxim) {
        paw_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                         d$datim$prioritizations,
                                         d$datim$OPU)
      } else {
        paw_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                         d$datim$prioritizations,
                                         d$datim$UndistributedMER)
      }
    }
  }

  if (d$info$tool %in% c("PSNUxIM")) {
    paw_export <-  d$datim$OPU
  }

  paw_export %>%
    dplyr::mutate(value = as.character(value)) %>%
    #Filter any non-dedupe zeros
    dplyr::filter(!(value == "0" & !grepl("^0000[01]", attributeOptionCombo)))

}
