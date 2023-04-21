
#' Title
#'
#' @param d
#'
#' @return A data frame in standardized DHIS2 export format
#' @export
#'
createDATIMExport <- function(d) {

  if (d$info$tool == "Data Pack") {
    if (d$info$cop_year == 2022) {
      if (d$info$has_psnuxim) {
        datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                         d$datim$fy22_prioritizations,
                                         d$datim$MER)
      } else {
        datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
                                         d$datim$fy22_prioritizations,
                                         d$datim$UndistributedMER)
      }
    }

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
  }

  if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {
    datim_export <-  d$datim$OPU
  }

  if (NROW(datim_export) > 0) {
    datim_export %>%
      dplyr::mutate(value = as.character("value"))
  } else {
    return(NULL)
  }


}
