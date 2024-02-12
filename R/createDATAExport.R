#' @export
#' @title createDATAExport
#'
#' @param d
#' @param export_type paw or datim are passed as export types
#'
#' @return A data frame in standardized DHIS2 export format for DATIM or PAW
#' @export
#'
#'
createDATAExport <- function(d, export_type = NULL) {

  if (is.null(export_type) || !export_type %in% c("datim", "paw")) {
    stop("you must provide a proper export type!")
  }

  if (export_type == "datim") {

    if (d$info$tool == "Data Pack") {

      # 2023 exceptions ----
      if (d$info$cop_year == 2023) {
        if (d$info$has_psnuxim) {
          datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                           d$datim$prioritizations,
                                           d$datim$OPU)
        } else {
          datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                           d$datim$prioritizations,
                                           d$datim$UndistributedMER)
        }
      }


      # 2024 exceptions ----
      if (d$info$cop_year == 2024) {
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

  } else {

    if (d$info$tool == "Data Pack") {

      if (d$info$cop_year %in% c(2023, 2024)) {
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

    if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {
      paw_export <-  d$datim$OPU
    }

    paw_export %>%
      dplyr::mutate(value = as.character(value)) %>%
      #Filter any non-dedupe zeros
      dplyr::filter(!(value == "0" & !grepl("^0000[01]", attributeOptionCombo)))

  }

}
