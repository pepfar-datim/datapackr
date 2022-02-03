#' @export
#' @importFrom magrittr %>% %<>%
#' @title packForDATIM(data)
#'
#' @description
#' Flexible function that allows packaging of a variety of datapackr outputs as
#' DATIM import files.
#'
#' @param d Datapackr object.
#' @param type Type of dataset to prep for DATIM. Choose from \code{PSNUxIM},
#' \code{SUBNAT_IMPATT}, \code{OPU PSNUxIM}, or \code{Site}.
#'
#' @return Data frame ready for DATIM import
#'
packForDATIM <- function(d, type = NULL) {

  # call datim map once for cop year to pass to functions
  datim_map <- datapackr::getMapDataPack_DATIM_DEs_COCs(d$info$cop_year)

  if (is.null(type)) {
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM', 'Undistributed MER'")
  } else if (type == "SUBNAT_IMPATT") {
    d <- exportSubnatToDATIM(d, datim_map)
  } else if (type == "PSNUxIM") {
    d <- packForDATIM_MER(d, datim_map)
  } else if (type == "OPU PSNUxIM") {

    # exceptions for 2020 and 2021
    if (!d$info$cop_year %in% c(2020, 2021)) {
      stop("The COP year provided is not supported by packForDATIM_OPU")
    } else {
      d <- packForDATIM_OPU(d, datim_map)
    }

  } else if (type == "Undistributed MER") {
    d <- packForDATIM_UndistributedMER(d, datim_map)
  } else{
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM'")
  }

  return(d)
}
