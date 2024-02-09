#' @export
#' @title createPAWExport
#'
#' @param d
#'
#' @return A data frame in standardized DHIS2 export format for PAW
#' @export
#'
createPAWExport <- function(d) {

  # pack data for PAW
  types <- c("Undistributed MER", "SUBNAT_IMPATT", "PSNUxIM", "OPU PSNUxIM")

  for (type in types) {
    tryCatch(
      {
        d <- packForPAW(d, type = type)
      },
      error = function(e) {
        stop("An error occurred for type ", type, ": ", conditionMessage(e))
      }
    )
  }


  if (d$info$tool == "Data Pack") {
    if (d$info$cop_year == 2022) {
      if (d$info$has_psnuxim) {
        paw_export <- dplyr::bind_rows(d$paw$subnat_impatt,
                                         d$paw$fy22_prioritizations,
                                         d$paw$MER)
      } else {
        paw_export <- dplyr::bind_rows(d$paw$subnat_impatt,
                                         d$paw$fy22_prioritizations,
                                         d$paw$UndistributedMER)
      }
    }

    if (d$info$cop_year %in% c(2023, 2024)) {
      if (d$info$has_psnuxim) {
        paw_export <- dplyr::bind_rows(d$paw$subnat_impatt,
                                         d$paw$prioritizations,
                                         d$paw$OPU)
      } else {
        paw_export <- dplyr::bind_rows(d$paw$subnat_impatt,
                                         d$paw$prioritizations,
                                         d$paw$UndistributedMER)
      }
    }
  }

  if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {
    paw_export <-  d$paw$OPU
  }

  paw_export %>%
    dplyr::mutate(value = as.character(value)) %>%
    #Filter any non-dedupe zeros
    dplyr::filter(!(value == "0" & !grepl("^0000[01]", attributeOptionCombo)))

}
