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
  
  if (is.null(type)) {
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM'")
  } else if (type == "SUBNAT_IMPATT") {
    d <- exportSubnatToDATIM(d)
  } else if (type == "PSNUxIM") {
    d <- exportDistributedDataToDATIM(d)
  } else if (type == "OPU PSNUxIM") {
    d <- packForDATIM_OPU(d)
  } else{
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT', 'OPU PSNUxIM'")
  }
  
  return(d)
}
