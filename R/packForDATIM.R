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
#' \code{SUBNAT_IMPATT}, or \code{Site}.
#' 
#' @return Data frame ready for DATIM import
#' 
packForDATIM <- function(d, type = NA) {
  
  if (is.na(type)) {
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT'")
  } else if (type == "SUBNAT_IMPATT") {
    d <- exportSubnatToDATIM(d)
  } else if (type == "PSNUxIM") {
    d <- exportDistributedDataToDATIM(d)
  } else{
    stop("Please specify data type in parameters: 'PSNUxIM', 'SUBNAT_IMPATT'")
  }
  
  return(d)
}
