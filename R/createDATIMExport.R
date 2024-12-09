#' @export
#' @title createDATIMExport
#'
#' @inheritParams datapackr_params
#'
#' @return A data frame in standardized DHIS2 export format for DATIM
#' @export
#'
createDATIMExport <- function(d) {

  if (d$info$tool == "Data Pack") {

    # 2023/2024
    # remove pop data for datim
    if (d$info$cop_year %in% c(2023, 2024, 2025)) {
      if (d$info$has_psnuxim) {
        datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                         d$datim$prioritizations,
                                         d$datim$OPU)
      } else {
        datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                         d$datim$prioritizations,
                                         d$datim$UndistributedMER)
      }

      # pop data needs to removed for COP24, specifically FY24 Targets aka 2023Oct
      # pop data should not be in 2023 as well so it is okay to run here
      pop_data <- c("KssDaTsGWnS", "lJtpR5byqps", "nF19GOjcnoD", "P2XNbiNnIqV")
      datim_export <-
        datim_export %>%
        dplyr::filter(!dataElement %in% pop_data)

    }
  }

  # handle OPU
  if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {
    datim_export <-  d$datim$OPU
  }

  datim_export %>%
    dplyr::mutate(value = as.character(value)) %>%
  #Filter any non-dedupe zeros
  dplyr::filter(!(value == "0" & !grepl("^0000[01]", attributeOptionCombo)))

}
