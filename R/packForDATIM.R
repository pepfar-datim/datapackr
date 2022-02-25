#' @export
#' @title Pack Data Pack Data For DATIM
#'
#' @description
#' Flexible function that allows packaging of a variety of datapackr outputs as
#' DATIM import files.
#'
#' @param type Type of dataset to prep for DATIM. Choose from \code{PSNUxIM},
#' \code{SUBNAT_IMPATT}, \code{OPU PSNUxIM}, or \code{Site}.
#' @inheritParams datapackr_params
#'
#' @return Data frame ready for DATIM import
#'
packForDATIM <- function(d, type = NULL) {

  if (is.null(type)) {
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM', 'Undistributed MER'")
  } else if (type == "SUBNAT_IMPATT") {
    d <- exportSubnatToDATIM(d)
  } else if (type == "PSNUxIM") {
    d <- packForDATIM_MER(d)
  } else if (type == "OPU PSNUxIM") {
    d <- packForDATIM_OPU(d)
  } else if (type == "Undistributed MER") {
    d$data$UndistributedMER <- packForDATIM_UndistributedMER(data = d$data$MER,
                                                             cop_year = d$info$cop_year)
  } else{
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM'")
  }

  return(d)
}
